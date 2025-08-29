[@@@ocaml.flambda_o3]

open! Core0
open Cyclesim
module Out_channel = Stdio.Out_channel

let vcdcycle = 10
let default_hierarchy_separator = '$'

let char_allowed c =
  (* Names in VCD files are not allowed to have whitespace because they indicate the end
     of a token. Other disallowed characters were determined emperically based on the
     behavior seen in GTKWave *)
  let disallowed_chars = [ '\\'; '|'; ':'; '!'; '\''; ' ' ] in
  let is_allowed c = List.for_all disallowed_chars ~f:(fun x -> Char.O.(c <> x)) in
  Char.is_print c && is_allowed c
;;

let sanitize_name name =
  let name_sanitized = String.map ~f:(fun c -> if char_allowed c then c else '_') name in
  (* If we somehow end up with a signal or a scope that has a valid hierarchy but an empty
     name at its hierarchy level, still emit a valid VCD. *)
  match name_sanitized with
  | "" -> "EMPTY_NAME"
  | name -> name
;;

module Timescale = struct
  type t =
    | Fs of int
    | Ps of int
    | Ns of int
    | Us of int
    | Ms of int
    | S of int
  [@@deriving sexp_of]

  let to_string t =
    match t with
    | Fs i -> [%string "%{i#Int}fs"]
    | Ps i -> [%string "%{i#Int}ps"]
    | Ns i -> [%string "%{i#Int}ns"]
    | Us i -> [%string "%{i#Int}us"]
    | Ms i -> [%string "%{i#Int}ms"]
    | S i -> [%string "%{i#Int}s"]
  ;;
end

module Var = struct
  module Type = struct
    type t =
      | Event
      | Integer
      | Parameter
      | Real
      | Reg
      | Supply0
      | Supply1
      | Time
      | Tri
      | Triand
      | Trior
      | Trireg
      | Tri0
      | Tri1
      | Want
      | Wire
      | Wor
    [@@deriving sexp_of, compare ~localize, hash]

    let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
  end

  module Vcd_wave_format = struct
    type t =
      | Binary
      | Index of
          { strings : string array
          ; bitwidth : int
          }
      | Map of
          { map : string Map.M(Bits).t
          ; bitwidth : int
          }
    [@@deriving sexp_of]

    (* Convert the Hardcaml wave format to the closest VCD-representable option *)
    let of_signal_wave_format f =
      let max_bits_for_strings strings =
        let longest =
          strings
          |> List.map ~f:String.length
          |> List.max_elt ~compare:Int.compare
          |> Option.value ~default:1 (* Lower bound the length at one character *)
          |> Int.max 1
        in
        8 * longest
      in
      (* For efficiency, pre-compute the bitwidth for strings *)
      match f with
      | Wave_format.Index l ->
        Index { strings = List.to_array l; bitwidth = max_bits_for_strings l }
      | Wave_format.Map m ->
        Map
          { map = Map.of_alist_exn (module Bits) m
          ; bitwidth = max_bits_for_strings (List.map m ~f:(fun (_, s) -> s))
          }
      | Wave_format.Custom _ ->
        Binary
        (* We have to drop the custom wave format because the maximum length of the string
           needs to be known in advance (when defining the signals at the top of the VCD
           file), and this isn't possible with an arbitrary conversion function. *)
      | _ -> Binary
    ;;

    (* Get the width that the signal will be in the VCD given the width of the original
       signal and the format *)
    let get_vcd_width ~signal_width t =
      match t with
      | Binary -> signal_width
      | Index { bitwidth; _ } | Map { bitwidth; _ } -> bitwidth
    ;;
  end

  type t =
    { typ : Type.t
    ; name : string
    ; id : string
    ; width : int
    ; wave_format : Vcd_wave_format.t [@compare.ignore]
    }
  [@@deriving sexp_of, fields ~getters, compare ~localize, hash]

  let create ?(typ = Type.Wire) ?(wave_format = Wave_format.Binary) ~name ~id ~width () =
    { typ
    ; name
    ; id
    ; width
    ; wave_format = Vcd_wave_format.of_signal_wave_format wave_format
    }
  ;;

  let define chan { typ; name; id; width; wave_format } =
    let actual_width = Vcd_wave_format.get_vcd_width ~signal_width:width wave_format in
    Out_channel.output_string
      chan
      [%string "$var %{typ#Type} %{actual_width#Int} %{id} %{sanitize_name name} $end\n"]
  ;;

  let write_string chan { typ = _; name = _; id; width; wave_format } bits =
    let actual_width = Vcd_wave_format.get_vcd_width ~signal_width:width wave_format in
    if actual_width = 1
    then Out_channel.output_string chan [%string "%{bits}%{id}\n"]
    else Out_channel.output_string chan [%string "b%{bits} %{id}\n"]
  ;;

  (* Pre-generate a lookup table for a slight performance boost when converting strings *)
  let char_to_binary_table =
    Array.init 256 ~f:(fun i ->
      i
      |> Int.Binary.to_string
      |> String.chop_prefix_exn ~prefix:"0b"
      |> String.pad_left ~char:'0' ~len:8)
  ;;

  let write_bits chan ({ typ = _; name = _; id = _; width; wave_format } as t) bits =
    if Bits.width bits <> width
    then raise_s [%message "Invalid bit width" (t : t) (bits : Bits.t)];
    (* Convert an ASCII string to binary and then write it in a way compatible with
       SystemVerilog strings. This is not particularly performant, but custom wave formats
       are ideally used sparingly (for state parameters and such). *)
    let write_string_ascii s =
      let actual_width = Vcd_wave_format.get_vcd_width ~signal_width:width wave_format in
      let bitstring =
        s
        |> String.concat_map ~f:(fun c -> char_to_binary_table.(Char.to_int c))
        |> String.pad_left ~char:'0' ~len:actual_width
      in
      write_string chan t bitstring
    in
    match wave_format with
    | Binary -> write_string chan t (Bits.to_string bits)
    | Index { strings; _ } ->
      let idx = Bits.to_unsigned_int bits in
      let s = if idx < Array.length strings then strings.(idx) else "?" in
      write_string_ascii s
    | Map { map; _ } ->
      let s = Map.find map bits |> Option.value ~default:"?" in
      write_string_ascii s
  ;;

  let write_all_x chan ({ typ = _; name = _; id = _; width; wave_format } as t) =
    let actual_width = Vcd_wave_format.get_vcd_width ~signal_width:width wave_format in
    let s =
      List.init actual_width ~f:(Fn.const Logic.Four_state.(to_char X))
      |> String.of_char_list
    in
    write_string chan t s
  ;;

  module Generator = struct
    type t = int ref

    let min_id_char = 33
    let max_id_char = 126
    let id_char_range = max_id_char - min_id_char + 1
    let create () = ref 0

    let rec create_identifier x =
      let y, left = x % id_char_range, x / id_char_range in
      let c = Char.of_int_exn (y + min_id_char) in
      if left <> 0 then c :: create_identifier (left - 1) else c :: []
    ;;

    let next t =
      let x = !t in
      Int.incr t;
      create_identifier x |> String.of_char_list
    ;;
  end
end

module Scope = struct
  module Type = struct
    type t =
      | Begin
      | Fork
      | Function
      | Module
      | Task
    [@@deriving sexp_of]

    let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
  end

  type t =
    { name : string
    ; typ : Type.t
    ; vars : Var.t list
    ; subscopes : t list
    }
  [@@deriving sexp_of, fields ~getters]

  let create ?(subscopes = []) ?(typ = Type.Module) ~name ~vars () =
    { name; typ; vars; subscopes }
  ;;

  (* Recursive helper function for building the hierarchy *)
  let rec create_helper ?(typ = Type.Module) ~name ~vars () =
    let vars_in_this_scope = Queue.create () in
    let vars_by_subscope = Hashtbl.create (module String) in
    List.iter vars ~f:(fun (typ, name, id, width, wave_format) ->
      match name with
      | [] ->
        failwith "Got an empty split variable name; this shouldn't be possible to reach"
      | basename :: [] ->
        (* Base case; the variable is in the current scope *)
        Queue.enqueue vars_in_this_scope (typ, basename, id, width, wave_format)
      | scope :: rest_of_the_name ->
        (* The variable is in a subscope *)
        (* Manually adjust the scope name to disambiguate inputs and outputs of the
           current module - we prefer to see the inputs and outputs sorted before other
           signals within the same scope, so prefix them with '-'. *)
        let scope =
          match scope with
          | "i" -> "-inputs"
          | "o" -> "-outputs"
          | module_name -> module_name
        in
        let vars_in_subscope =
          Hashtbl.find_or_add vars_by_subscope scope ~default:Queue.create
        in
        Queue.enqueue vars_in_subscope (typ, rest_of_the_name, id, width, wave_format));
    { name
    ; typ
    ; vars =
        vars_in_this_scope
        |> Queue.to_list
        |> List.map ~f:(fun (typ, name, id, width, wave_format) ->
          { Var.typ; name; id; width; wave_format })
        |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
    ; subscopes =
        Hashtbl.to_alist vars_by_subscope
        |> List.map ~f:(fun (scope_name, vars) ->
          create_helper ~typ ~name:scope_name ~vars:(Queue.to_list vars) ())
        |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
    }
  ;;

  let create_auto_hierarchy
    ?typ
    ?(split_on = default_hierarchy_separator)
    ~name
    ~(vars : Var.t list)
    ()
    =
    create_helper
      ?typ
      ~name
      ~vars:
        (List.map vars ~f:(fun { typ; name; id; width; wave_format } ->
           let name_split = String.split ~on:split_on name in
           typ, name_split, id, width, wave_format))
      ()
  ;;

  let rec write chan { name; typ; vars; subscopes } =
    Out_channel.output_string
      chan
      [%string "$scope %{typ#Type} %{sanitize_name name} $end\n"];
    List.iter vars ~f:(Var.define chan);
    List.iter subscopes ~f:(write chan);
    Out_channel.output_string chan [%string "$upscope $end\n"]
  ;;

  let rec all_vars t = List.concat ([ t.vars ] @ List.map t.subscopes ~f:all_vars)
end

module Config = struct
  type t =
    { date : string
    ; version : string
    ; comment : string option
    ; timescale : Timescale.t
    }
  [@@deriving sexp_of]

  let default =
    { date = "..."
    ; version = "hardcaml"
    ; comment = Some "Hardware design in ocaml"
    ; timescale = Ns 1
    }
  ;;

  let write chan t =
    Out_channel.output_string chan [%string "$date\n  %{t.date}\n$end\n"];
    Out_channel.output_string chan [%string "$version\n  %{t.version}\n$end\n"];
    Option.iter t.comment ~f:(fun comment ->
      Out_channel.output_string chan [%string "$comment\n  %{comment}\n$end\n"]);
    Out_channel.output_string chan [%string "$timescale %{t.timescale#Timescale} $end\n"]
  ;;
end

(* ['prev] must be a mutable type if it is going to be used *)
type 'data trace =
  { var : Var.t
  ; data : 'data
  ; prev : Bits.Mutable.t
  }

let enddefinitions chan = Out_channel.output_string chan "$enddefinitions $end\n"

let dumpvars_as_x chan vars =
  Out_channel.output_string chan "$dumpvars\n";
  List.iter vars ~f:(fun v -> Var.write_all_x chan v);
  Out_channel.output_string chan "$end\n"
;;

let write_header chan ~config ~scopes =
  Config.write chan config;
  List.iter scopes ~f:(Scope.write chan);
  enddefinitions chan;
  dumpvars_as_x chan (List.concat (List.map scopes ~f:Scope.all_vars))
;;

let write_time chan time = Out_channel.output_string chan [%string "#%{time#Int}\n"]

let wrap chan sim =
  let var_generator = Var.Generator.create () in
  (* Prefix clock and reset with dashes so they show up first if the viewer sorts the list
     of signals *)
  let clock =
    Var.create ~name:"-clock" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let reset =
    Var.create ~name:"-reset" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let write_var_fast v d = Var.write_string chan v d in
  (* list of signals to trace *)
  let create_var ?wave_format name width data =
    { var = Var.create ?wave_format ~name ~id:(Var.Generator.next var_generator) ~width ()
    ; data
    ; prev = Bits.Mutable.create width
    }
  in
  let trace signals =
    List.map signals ~f:(fun (name, (s : Bits.t ref)) ->
      create_var name (Bits.width !s) s)
  in
  let trace_internal (s : Cyclesim.Traced.internal_signal list) =
    List.concat_map s ~f:(fun ({ signal; mangled_names } as trace) ->
      let wave_format = Signal.Type.get_wave_format signal in
      Cyclesim.lookup_node_or_reg sim trace
      (* It is possible for a sim to request to trace a signal that corresponds to a
         nonexistent node, handle this case by ignoring it. *)
      |> Option.value_map ~default:[] ~f:(fun s ->
        List.map mangled_names ~f:(fun name ->
          create_var ~wave_format name (Node.width_in_bits s) s)))
  in
  let trace_in = trace (in_ports sim) in
  let trace_out = trace (out_ports sim ~clock_edge:Before) in
  let trace_internal = trace_internal (traced sim).internal_signals in
  (* filter out 'clock' and 'reset' *)
  let trace_in =
    List.filter trace_in ~f:(fun s ->
      (not (String.equal (Var.name s.var) "clock"))
      && not (String.equal (Var.name s.var) "reset"))
  in
  (* write the VCD header *)
  let scopes =
    [ Scope.create_auto_hierarchy
        ~name:"inputs"
        ~vars:(clock :: reset :: List.map trace_in ~f:(fun t -> t.var))
        ()
    ; Scope.create_auto_hierarchy
        ~name:"outputs"
        ~vars:(List.map trace_out ~f:(fun t -> t.var))
        ()
    ; Scope.create_auto_hierarchy
        ~name:"various"
        ~vars:(List.map trace_internal ~f:(fun t -> t.var))
        ()
    ]
  in
  let time = ref 0 in
  write_header chan ~config:{ Config.default with version = "hardcaml-cyclesim" } ~scopes;
  (* reset *)
  let write_reset () =
    write_time chan !time;
    write_var_fast clock "0";
    write_var_fast reset "1";
    List.iter trace_in ~f:(fun t ->
      let str = Bits.to_bstr !(t.data) in
      write_var_fast t.var str;
      Bits.Mutable.copy_bits ~src:!(t.data) ~dst:t.prev);
    List.iter trace_out ~f:(fun t ->
      let str = Bits.to_bstr !(t.data) in
      write_var_fast t.var str;
      Bits.Mutable.copy_bits ~src:!(t.data) ~dst:t.prev);
    List.iter trace_internal ~f:(fun t ->
      let bits = Cyclesim.Node.to_bits t.data in
      (match t.var.wave_format with
       | Binary -> write_var_fast t.var (Bits.to_bstr bits)
       | _ -> Var.write_bits chan t.var bits);
      Bits.Mutable.copy_bits ~src:bits ~dst:t.prev);
    time := !time + vcdcycle
  in
  (* Since we write all Xs at the start, always write all values on the first cycle to
     ensure that the initial value is consistent with what we expect. *)
  let first = ref true in
  (* cycle *)
  let write_cycle () =
    write_time chan !time;
    write_var_fast clock "1";
    write_var_fast reset "0";
    List.iter trace_in ~f:(fun t ->
      if !first || not (Bits.Mutable.equal_bits !(t.data) t.prev)
      then (
        write_var_fast t.var (Bits.to_bstr !(t.data));
        Bits.Mutable.copy_bits ~src:!(t.data) ~dst:t.prev));
    List.iter trace_out ~f:(fun t ->
      if !first || not (Bits.Mutable.equal_bits !(t.data) t.prev)
      then (
        write_var_fast t.var (Bits.to_bstr !(t.data));
        Bits.Mutable.copy_bits ~src:!(t.data) ~dst:t.prev));
    List.iter trace_internal ~f:(fun t ->
      if !first || not (Node.equal_bits_mutable t.data t.prev)
      then (
        let bits = Cyclesim.Node.to_bits t.data in
        (match t.var.wave_format with
         | Binary -> write_var_fast t.var (Bits.to_bstr bits)
         | _ -> Var.write_bits chan t.var bits);
        Node.to_bits_mutable t.data t.prev));
    write_time chan (!time + (vcdcycle / 2));
    write_var_fast clock "0";
    first := false;
    time := !time + vcdcycle
  in
  Private.modify sim [ After, Reset, write_reset; Before, At_clock_edge, write_cycle ]
;;
