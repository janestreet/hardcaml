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

(* Synthesise the implicit [-clock] and [-reset] [Var.t]s used by both [wrap] and
   [write_cycle_based]. Prefix the names with dashes so that a sorted waveform viewer
   shows them first. *)
let make_implicit_clock_and_reset_vars var_generator =
  let clock =
    Var.create ~name:"-clock" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  let reset =
    Var.create ~name:"-reset" ~id:(Var.Generator.next var_generator) ~width:1 ()
  in
  clock, reset
;;

(* Build the standard [inputs] / [outputs] / [various] scopes used by both [wrap] and
   [write_cycle_based]. The synthesised clock and reset live in [inputs]. *)
let build_clocked_scopes ~clock ~reset ~input_vars ~output_vars ~internal_vars =
  [ Scope.create_auto_hierarchy ~name:"inputs" ~vars:(clock :: reset :: input_vars) ()
  ; Scope.create_auto_hierarchy ~name:"outputs" ~vars:output_vars ()
  ; Scope.create_auto_hierarchy ~name:"various" ~vars:internal_vars ()
  ]
;;

(* Write one signal value, dispatching on [wave_format] so we use [Bits.to_bstr] for the
   plain [Binary] case (cheaper, matches what [wrap] does today) and [Var.write_bits] for
   the formats that require ASCII/Index/Map encoding. *)
let write_signal_bits chan (var : Var.t) bits =
  match var.wave_format with
  | Binary -> Var.write_string chan var (Bits.to_bstr bits)
  | _ -> Var.write_bits chan var bits
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

(* Write a VCD file from cycle-based waveform data already populated up front.

   This mirrors [wrap]: index 0 of each wave is treated as the post-reset state and is
   emitted at [t = 0] with the synthesised [-clock = 0] and [-reset = 1]. Index [N >= 1]
   is emitted at [t = N * vcdcycle] with [-clock = 1] and [-reset = 0], followed by a
   clock-low half-cycle. As in [wrap], waves named ["clock"] or ["reset"] are dropped to
   avoid colliding with the synthesised pair. *)
let write_cycle_based
  ?(config = Config.default)
  chan
  (waves : Wave_data_in_cycles.t Wave_data.Wave.t array)
  =
  let vcdcycle = 2 in
  let var_generator = Var.Generator.create () in
  let clock, reset = make_implicit_clock_and_reset_vars var_generator in
  let write_var_fast v d = Var.write_string chan v d in
  let traces =
    Array.filter_map waves ~f:(fun (wave : _ Wave_data.Wave.t) ->
      if String.equal wave.name "clock" || String.equal wave.name "reset"
      then None
      else (
        let var =
          Var.create
            ~name:wave.name
            ~id:(Var.Generator.next var_generator)
            ~width:wave.width
            ~wave_format:wave.wave_format
            ()
        in
        let prev = Bits.Mutable.create wave.width in
        Some (wave, var, prev)))
  in
  let vars_of_typ typ =
    Array.to_list traces
    |> List.filter_map ~f:(fun (wave, var, _) ->
      if Wave_data.Type.equal wave.typ typ then Some var else None)
  in
  let scopes =
    build_clocked_scopes
      ~clock
      ~reset
      ~input_vars:(vars_of_typ Input)
      ~output_vars:(vars_of_typ Output)
      ~internal_vars:(vars_of_typ Internal)
  in
  write_header chan ~config:{ config with version = "hardcaml-cyclesim" } ~scopes;
  let num_cycles =
    Array.fold traces ~init:0 ~f:(fun acc (wave, _, _) ->
      Int.max acc (Wave_data_in_cycles.length wave.wave_data))
  in
  if num_cycles = 0
  then ()
  else (
    (* Write a single signal value at [cycle], updating [prev] if we wrote. With
       [~force:true] the value is always written (used during the reset phase and on the
       first cycle, mirroring [wrap]'s [first] flag). *)
    let write_value ((wave : _ Wave_data.Wave.t), var, prev) ~cycle ~force =
      if cycle < Wave_data_in_cycles.length wave.wave_data
      then (
        let bits = Wave_data_in_cycles.get wave.wave_data cycle in
        if force || not (Bits.Mutable.equal_bits bits prev)
        then (
          write_signal_bits chan var bits;
          Bits.Mutable.copy_bits ~src:bits ~dst:prev))
    in
    let time = ref 0 in
    (* Reset phase: t = 0, clock = 0, reset = 1, dump cycle 0. *)
    write_time chan !time;
    write_var_fast clock "0";
    write_var_fast reset "1";
    Array.iter traces ~f:(fun trace -> write_value trace ~cycle:0 ~force:true);
    time := !time + vcdcycle;
    (* Cycle phases: t = N * vcdcycle for N >= 1, dumping cycle N. *)
    let first = ref true in
    for cycle = 1 to num_cycles - 1 do
      write_time chan !time;
      write_var_fast clock "1";
      write_var_fast reset "0";
      Array.iter traces ~f:(fun trace -> write_value trace ~cycle ~force:!first);
      write_time chan (!time + (vcdcycle / 2));
      write_var_fast clock "0";
      first := false;
      time := !time + vcdcycle
    done)
;;

(* Write a VCD file from event-based waveform data already populated up front.

   Unlike [wrap], this does not synthesise clock/reset signals: any such waves are
   expected to already be present in [waves] and are emitted like any other signal. *)
let write_event_based
  ?(config = Config.default)
  chan
  (waves : Wave_data_in_events.Bits.t Wave_data.Wave.t array)
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
  Wave_data.event_sequence_in_time_order waves
  |> Sequence.iter ~f:(fun { Wave_data.wave_index; event_index } ->
    let wave = waves.(wave_index) in
    let store = Wave_data_in_events.Bits.event_store wave.wave_data in
    let time = Wave_data_in_events.Bits.Event_store.get_time_at_index store event_index in
    let bits = Wave_data_in_events.Bits.Event_store.get_data_at_index store event_index in
    (* Only emit a [#time] marker when the time advances; multiple events at the same time
       are written under a single marker. *)
    (match !last_time_written with
     | Some t when t = time -> ()
     | _ ->
       write_time chan time;
       last_time_written := Some time);
    Var.write_bits chan vars.(wave_index) bits)
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

let bit_to_binary_char (bit : Hardcaml_vcd.Types.Bit.t) =
  match bit with
  | V0 -> '0'
  | V1 -> '1'
  | Vx | Vz ->
    raise_s
      [%message
        "[Vcd.read_event_based]: cannot represent VCD X/Z bit in a [Bits.t] value"
          (bit : Hardcaml_vcd.Types.Bit.t)]
;;

(* Returns [None] if any bit in the value is X / Z (and [allow_skip_xz] is set),
   [Some (id, bits)] otherwise. Raises on real values, or on X/Z when not allowed. *)
let value_change_to_id_and_bits
  ?(allow_skip_xz = false)
  ~width_of_id
  (vc : Hardcaml_vcd.Types.Value_change.t)
  =
  let contains_xz_bit = function
    | Hardcaml_vcd.Types.Bit.Vx | Vz -> true
    | V0 | V1 -> false
  in
  match vc with
  | Real_value _ ->
    raise_s
      [%message
        "[Vcd.read_event_based]: real-valued VCD signals are not supported"
          (vc : Hardcaml_vcd.Types.Value_change.t)]
  | Scalar_value (b, id) ->
    if allow_skip_xz && contains_xz_bit b
    then None
    else (
      let s = String.make 1 (bit_to_binary_char b) in
      Some (id, Bits.of_string s))
  | Vector_value (bs, id) ->
    if allow_skip_xz && List.exists bs ~f:contains_xz_bit
    then None
    else (
      let width = width_of_id id in
      let s = String.of_char_list (List.map bs ~f:bit_to_binary_char) in
      (* The VCD spec allows shorter values than the declared width to be left-padded with
         zeros (or with the leading X/Z bit, which we don't support here). *)
      let s = String.pad_left s ~char:'0' ~len:width in
      Some (id, Bits.of_string s))
;;

(* Parse a VCD into a [Wave_data_in_events.Bits.t Wave_data.Wave.t array].

   This is intended to round-trip VCDs produced by [wrap], [write_event_based] and
   [write_cycle_based]. Some VCD features that aren't trivially representable raise: X/Z
   values in mid-simulation value changes, real-valued signals, [$dumpoff] regions.
   Initial all-X [$dumpvars] blocks (as written by Hardcaml's writers) are skipped. *)
let read_event_based (vcd : Hardcaml_vcd.t) =
  (* First pass: walk declarations to build the wave list and an id -> wave map. *)
  let scope_stack = ref [] in
  let waves = Queue.create () in
  let max_time_by_id = Hashtbl.create (module String) in
  let wave_by_id = Hashtbl.create (module String) in
  List.iter vcd.declarations ~f:(fun (d : Hardcaml_vcd.Types.Declaration.t) ->
    match d with
    | Comment _ | Date _ | Version _ | Timescale _ | Enddefinitions -> ()
    | Scope (_typ, name) -> scope_stack := name :: !scope_stack
    | Upscope ->
      (match !scope_stack with
       | _ :: rest -> scope_stack := rest
       | [] ->
         raise_s [%message "[Vcd.read_event_based]: $upscope without matching $scope"])
    | Var { var_type = _; var_size; var_id; var_ref } ->
      let path = List.rev !scope_stack in
      let typ, hierarchy = typ_and_hierarchy_of_scope_path path in
      let name =
        String.concat
          ~sep:(String.make 1 default_hierarchy_separator)
          (hierarchy @ [ var_ref.ref_name ])
      in
      let max_time = ref 0 in
      let wave_data = Wave_data_in_events.Bits.create var_size max_time in
      let wave : _ Wave_data.Wave.t =
        { name
        ; width = var_size
        ; typ
        ; wave_format = Bit_or Hex
        ; is_pseudo_clock = false
        ; wave_data
        }
      in
      Queue.enqueue waves wave;
      (match Hashtbl.add wave_by_id ~key:var_id ~data:wave with
       | `Ok -> ()
       | `Duplicate ->
         raise_s
           [%message
             "[Vcd.read_event_based]: duplicate VCD identifier in declarations"
               (var_id : string)]);
      Hashtbl.add_exn max_time_by_id ~key:var_id ~data:max_time);
  (match !scope_stack with
   | [] -> ()
   | _ ->
     raise_s
       [%message
         "[Vcd.read_event_based]: VCD ended with unclosed scopes"
           (!scope_stack : string list)]);
  let width_of_id id = (Hashtbl.find_exn wave_by_id id).width in
  let current_time = ref 0 in
  let insert_event ~id ~bits =
    let wave = Hashtbl.find_exn wave_by_id id in
    let max_time = Hashtbl.find_exn max_time_by_id id in
    let store = Wave_data_in_events.Bits.event_store wave.wave_data in
    Wave_data_in_events.Bits.Event_store.insert store !current_time bits;
    if !current_time > !max_time then max_time := !current_time
  in
  let process_dump_block vcs =
    (* [$dumpvars], [$dumpall], [$dumpon] all set the value of every signal at the current
       time. Hardcaml's writers emit a leading [$dumpvars] block of all-X values; we skip
       any X/Z entries and process the rest as ordinary value changes. *)
    List.iter vcs ~f:(fun vc ->
      match value_change_to_id_and_bits ~allow_skip_xz:true ~width_of_id vc with
      | None -> ()
      | Some (id, bits) -> insert_event ~id ~bits)
  in
  List.iter
    vcd.simulation_commands
    ~f:(fun (cmd : Hardcaml_vcd.Types.Simulation_command.t) ->
      match cmd with
      | Sim_time t -> current_time := t
      | Sim_comment _ -> ()
      | Sim_dumpvars vcs | Sim_dumpall vcs | Sim_dumpon vcs -> process_dump_block vcs
      | Sim_dumpoff _ ->
        raise_s
          [%message
            "[Vcd.read_event_based]: [$dumpoff] regions are not supported (X / Z values \
             cannot be represented in [Bits.t])"]
      | Sim_value_change vc ->
        (match value_change_to_id_and_bits ~allow_skip_xz:false ~width_of_id vc with
         | Some (id, bits) -> insert_event ~id ~bits
         | None ->
           (* [allow_skip_xz=false] should always return [Some] or raise. *)
           assert false));
  Queue.to_array waves
;;
