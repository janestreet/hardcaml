[@@@ocaml.flambda_o3]

open! Core0
module Out_channel = Stdio.Out_channel

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
      | Wand
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

  (* Write a 4-state value. Pure values go through the ordinary [write_bits] path; impure
     values are written in "01XZ" notation for [Binary] formats. The string-valued formats
     ([Index] / [Map]) have no mapping for impure values, so those are written as all-X. *)
  let write_bits_four_state
    chan
    ({ typ = _; name = _; id = _; width; wave_format } as t)
    ~bits
    ~mask
    =
    if Bits.width bits <> width || Bits.width mask <> width
    then raise_s [%message "Invalid bit width" (t : t) (bits : Bits.t) (mask : Bits.t)];
    let value = Four_state_logic.create_exn ~bits ~mask in
    match Four_state_logic.impurity value with
    | Pure -> write_bits chan t bits
    | X | Z ->
      (match wave_format with
       | Binary ->
         (* Lowercase to match the x / z characters written elsewhere in the file. *)
         write_string chan t (String.lowercase (Four_state_logic.to_string value))
       | Index _ | Map _ -> write_all_x chan t)
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

(* Synthesise the implicit [-clock] and [-reset] [Var.t]s used by [wrap]. Prefix the names
   with dashes so that a sorted waveform viewer shows them first. *)
let make_implicit_clock_and_reset_vars var_generator =
  let clock =
    Var.create ~name:"-clock" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let reset =
    Var.create ~name:"-reset" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  clock, reset
;;

(* Build the standard [inputs] / [outputs] / [various] scopes used by [wrap]. The
   synthesised clock and reset live in [inputs]. *)
let build_clocked_scopes ~clock ~reset ~input_vars ~output_vars ~internal_vars =
  [ Scope.create_auto_hierarchy ~name:"inputs" ~vars:(clock :: reset :: input_vars) ()
  ; Scope.create_auto_hierarchy ~name:"outputs" ~vars:output_vars ()
  ; Scope.create_auto_hierarchy ~name:"various" ~vars:internal_vars ()
  ]
;;

let wrap chan sim =
  let vcdcycle = 10 in
  let var_generator = Var.Generator.create () in
  let clock, reset = make_implicit_clock_and_reset_vars var_generator in
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
          create_var ~wave_format name (Cyclesim.Node.width_in_bits s) s)))
  in
  let trace_in = trace (Cyclesim.in_ports sim) in
  let trace_out = trace (Cyclesim.out_ports sim ~clock_edge:Before) in
  let trace_internal = trace_internal (Cyclesim.traced sim).internal_signals in
  (* filter out 'clock' and 'reset' *)
  let trace_in =
    List.filter trace_in ~f:(fun s ->
      (not (String.equal (Var.name s.var) "clock"))
      && not (String.equal (Var.name s.var) "reset"))
  in
  (* write the VCD header *)
  let scopes =
    build_clocked_scopes
      ~clock
      ~reset
      ~input_vars:(List.map trace_in ~f:(fun t -> t.var))
      ~output_vars:(List.map trace_out ~f:(fun t -> t.var))
      ~internal_vars:(List.map trace_internal ~f:(fun t -> t.var))
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
      if !first || not (Cyclesim.Node.equal_bits_mutable t.data t.prev)
      then (
        let bits = Cyclesim.Node.to_bits t.data in
        (match t.var.wave_format with
         | Binary -> write_var_fast t.var (Bits.to_bstr bits)
         | _ -> Var.write_bits chan t.var bits);
        Cyclesim.Node.to_bits_mutable t.data t.prev));
    write_time chan (!time + (vcdcycle / 2));
    write_var_fast clock "0";
    first := false;
    time := !time + vcdcycle
  in
  Cyclesim.Private.modify
    sim
    [ After, Reset, write_reset; Before, At_clock_edge, write_cycle ]
;;

(* Write a VCD file from event-based waveform data already populated up front.

   Unlike [wrap], this does not synthesise clock/reset signals: any such waves are
   expected to already be present in [waves] and are emitted like any other signal. *)
let write_mutable_event_based_gen
  (type wave_data)
  ~(config : Config.t)
  ~(num_events : wave_data -> int)
  ~(get_time_at_index : wave_data -> int -> int)
  ~(write_event : Out_channel.t -> Var.t -> wave_data -> int -> unit)
  chan
  (waves : wave_data Wave_data.Wave.t array)
  =
  let var_generator = Var.Generator.create () in
  let vars =
    Array.map waves ~f:(fun (wave : _ Wave_data.Wave.t) ->
      Var.create
        ~name:wave.name
        ~id:(Var.Generator.next var_generator)
        ~width:wave.width
        ~wave_format:wave.wave_format
        ())
  in
  (* Group variables into scopes based on [Wave_data.Type.t], mirroring the layout used by
     [wrap]. Empty scopes are omitted. *)
  let scope_name_of_typ : Wave_data.Type.t -> string = function
    | Input -> "inputs"
    | Output -> "outputs"
    | Internal -> "various"
  in
  let by_scope = Hashtbl.create (module String) in
  Array.iteri waves ~f:(fun i wave ->
    let q =
      Hashtbl.find_or_add by_scope (scope_name_of_typ wave.typ) ~default:Queue.create
    in
    Queue.enqueue q vars.(i));
  let scopes =
    [ "inputs"; "outputs"; "various" ]
    |> List.filter_map ~f:(fun name ->
      Hashtbl.find by_scope name
      |> Option.map ~f:(fun q ->
        Scope.create_auto_hierarchy ~name ~vars:(Queue.to_list q) ()))
  in
  write_header chan ~config ~scopes;
  let last_time_written = ref None in
  Wave_data.event_sequence_in_time_order
    waves
    ~num_events:(fun wave -> num_events wave.wave_data)
    ~time_at_index:(fun wave event_index -> get_time_at_index wave.wave_data event_index)
  |> Sequence.iter ~f:(fun { Wave_data.wave_index; event_index } ->
    let wave = waves.(wave_index) in
    let time = get_time_at_index wave.wave_data event_index in
    (* Only emit a [#time] marker when the time advances; multiple events at the same time
       are written under a single marker. *)
    (match !last_time_written with
     | Some t when t = time -> ()
     | _ ->
       write_time chan time;
       last_time_written := Some time);
    write_event chan vars.(wave_index) wave.wave_data event_index)
;;

let write_mutable_event_based ?(config = Config.default) chan waves =
  write_mutable_event_based_gen
    ~config
    ~num_events:Wave_data_in_mutable_events.num_events
    ~get_time_at_index:Wave_data_in_mutable_events.get_time_at_index
    ~write_event:(fun chan var wave_data event_index ->
      Var.write_bits
        chan
        var
        (Wave_data_in_mutable_events.get_data_at_index wave_data event_index))
    chan
    waves
;;

let write_mutable_event_based_four_state ?(config = Config.default) chan waves =
  write_mutable_event_based_gen
    ~config
    ~num_events:Wave_data_in_mutable_events_four_state.num_events
    ~get_time_at_index:Wave_data_in_mutable_events_four_state.get_time_at_index
    ~write_event:(fun chan var wave_data event_index ->
      let bits =
        Wave_data_in_mutable_events_four_state.get_data_at_index wave_data event_index
      in
      let mask =
        Wave_data_in_mutable_events_four_state.get_mask_at_index wave_data event_index
      in
      Var.write_bits_four_state chan var ~bits ~mask)
    chan
    waves
;;

(* Reverse the [-inputs] / [-outputs] rename done by [Scope.create_helper] when reading
   back a VCD that was written with [Scope.create_auto_hierarchy]. *)
let unrename_io_scope_name = function
  | "-inputs" -> "i"
  | "-outputs" -> "o"
  | name -> name
;;

(* Determine the [Wave_data.Type.t] and the remaining hierarchy from a VCD scope path.

   When a VCD was written by Hardcaml, the top-level scope is one of [inputs], [outputs]
   or [various], and we use it to recover [typ]. For other VCDs we fall back to [Internal]
   and treat the whole path as hierarchical name components. *)
let typ_and_hierarchy_of_scope_path path =
  match path with
  | "inputs" :: rest -> Wave_data.Type.Input, List.map rest ~f:unrename_io_scope_name
  | "outputs" :: rest -> Output, List.map rest ~f:unrename_io_scope_name
  | "various" :: rest -> Internal, List.map rest ~f:unrename_io_scope_name
  | rest -> Internal, List.map rest ~f:unrename_io_scope_name
;;

let four_state_chars_of_bit (bit : Hardcaml_vcd.Types.Bit.t) =
  Four_state_logic.plane_chars
    (match bit with
     | V0 -> L0
     | V1 -> L1
     | Vx -> X
     | Vz -> Z)
;;

(* Real-valued signals have no natural [Bits.t] encoding, so their waves are 64 bits wide
   whatever size their [$var] declared, hold the IEEE bits of each double, and format the
   float back for display. Note that [Custom] wave formats do not survive bin_io
   serialization (they degrade to a bit string). *)
let real_width = 64

let real_wave_format : Wave_format.t =
  Custom (fun bits -> sprintf "%g" (Int64.float_of_bits (Bits.to_int64_trunc bits)))
;;

(* String-valued signals ([$var string]) also have no natural [Bits.t] encoding. Each
   distinct string a signal takes is interned in first-seen order as values are read, the
   wave stores indexes into that table, and the completed table becomes the wave's
   [Wave_format.Index] format (which, unlike [Custom], survives bin_io serialization). *)
let string_width = 32

module Value_kind = struct
  type t =
    | Bits
    | Real
    | String
  [@@deriving equal ~localize, sexp_of]

  let of_var_type : Hardcaml_vcd.Types.Var_type.t -> t = function
    | Real | Realtime | Shortreal | Real_parameter -> Real
    | String -> String
    | Event
    | Integer
    | Parameter
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
    | Wand
    | Wire
    | Wor
    | Bit
    | Byte
    | Enum
    | Int
    | Logic
    | Longint
    | Port
    | Shortint -> Bits
  ;;
end

module String_intern = struct
  type t =
    { index_by_string : (string, int) Hashtbl.t
    ; strings : string Queue.t
    }

  let create () =
    { index_by_string = Hashtbl.create (module String); strings = Queue.create () }
  ;;

  let index t string =
    Hashtbl.find_or_add t.index_by_string string ~default:(fun () ->
      Queue.enqueue t.strings string;
      Queue.length t.strings - 1)
  ;;

  let wave_format t : Wave_format.t = Index (Queue.to_list t.strings)
end

(* Convert a value change to its (bits, mask) planes. Raises when the kind of value does
   not match the signal's declaration. *)
let value_change_to_id_and_planes
  ~kind_of_id
  ~width_of_id
  ~string_index
  (vc : Hardcaml_vcd.Types.Value_change.t)
  =
  let check_kind id (expected : Value_kind.t) =
    let declared : Value_kind.t = kind_of_id id in
    if not (Value_kind.equal declared expected)
    then
      raise_s
        [%message
          "[Vcd.read_mutable_event_based_four_state]: value change does not match the \
           signal's declared type"
            (vc : Hardcaml_vcd.Types.Value_change.t)
            (declared : Value_kind.t)]
  in
  match vc with
  | Real_value (f, id) ->
    check_kind id Real;
    ( id
    , Bits.of_int64_trunc ~width:real_width (Int64.bits_of_float f)
    , Bits.zero real_width )
  | String_value (s, id) ->
    check_kind id String;
    id, Bits.of_int_trunc ~width:string_width (string_index id s), Bits.zero string_width
  | Scalar_value (b, id) ->
    check_kind id Bits;
    let bit, mask = four_state_chars_of_bit b in
    id, Bits.of_string (String.make 1 bit), Bits.of_string (String.make 1 mask)
  | Vector_value (bs, id) ->
    check_kind id Bits;
    let width = width_of_id id in
    let bits_s =
      String.of_char_list (List.map bs ~f:(fun b -> fst (four_state_chars_of_bit b)))
    in
    let mask_s =
      String.of_char_list (List.map bs ~f:(fun b -> snd (four_state_chars_of_bit b)))
    in
    (* The VCD spec left-extends values shorter than the declared width: with 0 when the
       leading bit is 0 or 1, and with the leading bit itself when it is X or Z. *)
    let pad_bit, pad_mask =
      match List.hd bs with
      | Some (Vx | Vz) -> four_state_chars_of_bit (List.hd_exn bs)
      | Some (V0 | V1) | None -> '0', '0'
    in
    ( id
    , Bits.of_string (String.pad_left bits_s ~char:pad_bit ~len:width)
    , Bits.of_string (String.pad_left mask_s ~char:pad_mask ~len:width) )
;;

(* Result of the reader's first pass over the declarations. *)
type declarations =
  { waves : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t Queue.t
  ; wave_by_id :
      Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t Hashtbl.M(String).t
      (* Maps each identifier to the wave of its first [$var]. Later [$var]s with the same
         identifier are aliases and share that wave's [wave_data]. *)
  ; kind_by_id : Value_kind.t Hashtbl.M(String).t
      (* Whether each identifier's values are bit vectors, floats, or strings. *)
  ; intern_by_id : String_intern.t Hashtbl.M(String).t
      (* String tables for [$var string] signals, keyed by identifier for value processing
         and by [wave_data] unique id so the final wave formats can be filled in at the
         end (aliases share both a table and a [wave_data]). *)
  ; intern_by_unique_id : String_intern.t Hashtbl.M(Int).t
  ; max_time : int ref
  (* Shared by all the waves; the second pass updates it as it sees [#time] markers. *)
  }

let read_declarations (vcd : Hardcaml_vcd.t) =
  let scope_stack = ref [] in
  let waves = Queue.create () in
  let max_time = ref 0 in
  let wave_by_id = Hashtbl.create (module String) in
  let kind_by_id = Hashtbl.create (module String) in
  let intern_by_id = Hashtbl.create (module String) in
  let intern_by_unique_id = Hashtbl.create (module Int) in
  List.iter vcd.declarations ~f:(fun (d : Hardcaml_vcd.Types.Declaration.t) ->
    match d with
    | Comment _ | Date _ | Version _ | Timescale _ | Enddefinitions -> ()
    | Scope (_typ, name) -> scope_stack := name :: !scope_stack
    | Upscope ->
      (match !scope_stack with
       | _ :: rest -> scope_stack := rest
       | [] ->
         raise_s
           [%message
             "[Vcd.read_mutable_event_based_four_state]: $upscope without matching $scope"])
    | Var { var_type; var_size; var_id; var_ref } ->
      let kind = Value_kind.of_var_type var_type in
      let width, wave_format =
        match kind with
        | Real -> real_width, real_wave_format
        | String ->
          (* A placeholder: replaced with the interned [Index] format once every value has
             been read. *)
          string_width, (Index [] : Wave_format.t)
        | Bits ->
          ( (* Some producers declare zero-size vars, such as [$var parameter 0], but
               [Bits.t] widths must be positive. *)
            Int.max 1 var_size
          , (Bit_or Hex : Wave_format.t) )
      in
      let path = List.rev !scope_stack in
      let typ, hierarchy = typ_and_hierarchy_of_scope_path path in
      let name =
        String.concat
          ~sep:(String.make 1 default_hierarchy_separator)
          (List.filter hierarchy ~f:(fun s -> not (String.is_empty s))
           @ [ var_ref.ref_name ])
      in
      (* Several [$var]s may share an identifier when a signal is dumped under more than
         one name, such as a port and the net it connects to. Each alias becomes its own
         wave, but they share one [wave_data]: inserting a value change once updates all
         of them. *)
      let existing = Hashtbl.find wave_by_id var_id in
      (match existing with
       | None -> ()
       | Some (alias : _ Wave_data.Wave.t) ->
         let declared = Hashtbl.find_exn kind_by_id var_id in
         if not (Value_kind.equal declared kind)
         then
           raise_s
             [%message
               "[Vcd.read_mutable_event_based_four_state]: VCD identifier declared with \
                inconsistent types"
                 (var_id : string)
                 ~kinds:((declared, kind) : Value_kind.t * Value_kind.t)];
         if alias.width <> width
         then
           raise_s
             [%message
               "[Vcd.read_mutable_event_based_four_state]: VCD identifier declared with \
                inconsistent widths"
                 (var_id : string)
                 ~widths:((alias.width, width) : int * int)]);
      Hashtbl.set kind_by_id ~key:var_id ~data:kind;
      let wave_data =
        match existing with
        | Some alias -> alias.wave_data
        | None -> Wave_data_in_mutable_events_four_state.create width max_time
      in
      (match kind with
       | Bits | Real -> ()
       | String ->
         let intern =
           Hashtbl.find_or_add intern_by_id var_id ~default:String_intern.create
         in
         Hashtbl.set
           intern_by_unique_id
           ~key:(Wave_data_in_mutable_events_four_state.unique_id wave_data)
           ~data:intern);
      let wave : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t =
        { name; width; typ; wave_format; wave_data }
      in
      Queue.enqueue waves wave;
      if Option.is_none existing then Hashtbl.add_exn wave_by_id ~key:var_id ~data:wave);
  (match !scope_stack with
   | [] -> ()
   | _ ->
     raise_s
       [%message
         "[Vcd.read_mutable_event_based_four_state]: VCD ended with unclosed scopes"
           (!scope_stack : string list)]);
  { waves; wave_by_id; kind_by_id; intern_by_id; intern_by_unique_id; max_time }
;;

let read_mutable_event_based_four_state (vcd : Hardcaml_vcd.t)
  : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t array
  =
  let { waves; wave_by_id; kind_by_id; intern_by_id; intern_by_unique_id; max_time } =
    read_declarations vcd
  in
  let undeclared id =
    raise_s
      [%message
        "[Vcd.read_mutable_event_based_four_state]: value change for undeclared VCD \
         identifier"
          (id : string)]
  in
  let wave_of_id id : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t =
    match Hashtbl.find wave_by_id id with
    | Some wave -> wave
    | None -> undeclared id
  in
  let kind_of_id id =
    match Hashtbl.find kind_by_id id with
    | Some kind -> kind
    | None -> undeclared id
  in
  let string_index id s = String_intern.index (Hashtbl.find_exn intern_by_id id) s in
  let width_of_id id = (wave_of_id id).width in
  let current_time = ref 0 in
  let process_value_change vc =
    let id, bits, mask =
      value_change_to_id_and_planes ~kind_of_id ~width_of_id ~string_index vc
    in
    Wave_data_in_mutable_events_four_state.insert
      (wave_of_id id).wave_data
      !current_time
      ~bits
      ~mask
  in
  List.iter
    vcd.simulation_commands
    ~f:(fun (cmd : Hardcaml_vcd.Types.Simulation_command.t) ->
      match cmd with
      | Sim_time t ->
        current_time := t;
        max_time := max !max_time t
      | Sim_comment _ -> ()
      (* [$dumpvars] / [$dumpall] / [$dumpon] set the value of every signal at the current
         time, and [$dumpoff] lists the values signals hold as dumping stops. All of them
         are representable in 4-state wave data, so they are recorded like any other value
         change. *)
      | Sim_dumpvars vcs | Sim_dumpall vcs | Sim_dumpon vcs | Sim_dumpoff vcs ->
        List.iter vcs ~f:process_value_change
      | Sim_value_change vc -> process_value_change vc);
  (* Every value has been read, so the string tables are complete: give string waves their
     [Index] formats. *)
  Queue.to_array waves
  |> Array.map
       ~f:(fun (wave : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t) ->
         match
           Hashtbl.find
             intern_by_unique_id
             (Wave_data_in_mutable_events_four_state.unique_id wave.wave_data)
         with
         | None -> wave
         | Some intern -> { wave with wave_format = String_intern.wave_format intern })
;;
