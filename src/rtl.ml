(* generic rtl writing for vhdl/verilog *)

open Base
include Rtl_intf
module Out_channel = Stdio.Out_channel
module Filename = Stdlib.Filename
module Signals_name_map = Rtl_ast.Signals_name_map

module Language = struct
  type t =
    | Verilog
    | Vhdl
  [@@deriving sexp_of]

  let file_extension = function
    | Verilog -> ".v"
    | Vhdl -> ".vhd"
  ;;
end

module Hierarchy_path : sig
  type t [@@deriving sexp_of]

  val empty : t
  val push : t -> string -> t
  val is_top_circuit : t -> Circuit.t -> bool
end = struct
  type t = string list

  let empty = []
  let push t s = s :: t
  let to_string_list t = List.rev t
  let sexp_of_t t = [%sexp (to_string_list t : string list)]

  let is_top_circuit t circuit =
    match t with
    | [ name ] -> String.equal (Circuit.name circuit) name
    | _ -> false
  ;;
end

module Output_mode = struct
  type t =
    | In_directory of string
    | To_buffer of Buffer.t
    | To_channel of Out_channel.t
    | To_file of string
  [@@deriving sexp_of]
end

module Output = struct
  module Mode = struct
    type t =
      | In_directory of string
      | To_buffer of Buffer.t
      | To_channel of Out_channel.t
      | To_file of
          { file : string
          ; out_channel : Out_channel.t
          }
    [@@deriving sexp_of]
  end

  type t =
    { language : Language.t
    ; mode : Mode.t
    }
  [@@deriving sexp_of]

  let create ~(output_mode : Output_mode.t) ~language =
    let mode : Mode.t =
      match output_mode with
      | In_directory d -> In_directory d
      | To_buffer b -> To_buffer b
      | To_channel c -> To_channel c
      | To_file file -> To_file { file; out_channel = Out_channel.create file }
    in
    { language; mode }
  ;;

  module Output_rtl = struct
    type t =
      { rtl : Buffer.t
      ; name_map : Signals_name_map.t
      }

    let output_new ~blackbox ~(language : Language.t) circuit =
      let buffer = Buffer.create 1024 in
      match language with
      | Verilog ->
        let ast =
          Rtl_ast.of_circuit ~blackbox (Rtl_name.create (module Rtl_name.Verilog)) circuit
        in
        Rtl_verilog_of_ast.to_buffer buffer ast;
        { rtl = buffer; name_map = Rtl_ast.Signals_name_map.create ast }
      | Vhdl ->
        let ast =
          Rtl_ast.of_circuit ~blackbox (Rtl_name.create (module Rtl_name.Vhdl)) circuit
        in
        Rtl_vhdl_of_ast.to_buffer buffer ast;
        { rtl = buffer; name_map = Rtl_ast.Signals_name_map.create ast }
    ;;

    let output_deprecated ~blackbox ~language circuit =
      let buffer = Buffer.create 1024 in
      let name_map =
        match (language : Language.t) with
        | Vhdl -> Rtl_deprecated.Vhdl.write blackbox (Buffer.add_string buffer) circuit
        | Verilog ->
          Rtl_deprecated.Verilog.write blackbox (Buffer.add_string buffer) circuit
      in
      { rtl = buffer; name_map }
    ;;

    let use_deprecated_generator = false

    let output ~blackbox ~language output circuit =
      let rtl =
        if use_deprecated_generator
        then output_deprecated ~blackbox ~language circuit
        else output_new ~blackbox ~language circuit
      in
      output rtl.rtl;
      rtl.name_map
    ;;
  end

  let output_circuit (blackbox : bool) t circuit hierarchy_path =
    try
      let output, close =
        match t.mode with
        | In_directory directory ->
          (* New file created for each circuit. *)
          let name = Circuit.name circuit in
          let file_name =
            Filename.concat directory (name ^ Language.file_extension t.language)
          in
          let chan = Out_channel.create file_name in
          Out_channel.output_buffer chan, fun () -> Out_channel.close chan
        | To_buffer buffer -> Buffer.add_buffer buffer, Fn.id
        | To_channel out_channel ->
          (* Note; up to caller to flush. *)
          Out_channel.output_buffer out_channel, Fn.id
        | To_file { file = _; out_channel } ->
          (* File is created before any processing occurs, then closed with the top
             level module. *)
          ( Out_channel.output_buffer out_channel
          , fun () ->
              if Hierarchy_path.is_top_circuit hierarchy_path circuit
              then Out_channel.close out_channel )
      in
      let ret = Output_rtl.output ~blackbox ~language:t.language output circuit in
      close ();
      ret
    with
    | exn ->
      raise_s
        [%message
          "Error while writing circuit"
            ~circuit_name:(Circuit.name circuit : string)
            (hierarchy_path : Hierarchy_path.t)
            ~output:(t : t)
            (exn : exn)]
  ;;
end

module Blackbox = struct
  type t =
    | None
    | Top
    | Instantiations
  [@@deriving sexp_of]
end

let output_with_name_map
  ?output_mode
  ?database
  ?(blackbox = Blackbox.None)
  language
  circuit
  =
  let output_mode =
    Option.value
      output_mode
      ~default:
        (Output_mode.To_file (Circuit.name circuit ^ Language.file_extension language))
  in
  let output =
    try Output.create ~language ~output_mode with
    | exn ->
      raise_s
        [%message
          "Error while initializing output mode."
            ~circuit_name:(Circuit.name circuit)
            (language : Language.t)
            (output_mode : Output_mode.t)
            (exn : exn)]
  in
  let database = Option.value database ~default:(Circuit_database.create ()) in
  let circuits_already_output = Hash_set.create (module String) in
  let name_map = ref (Map.empty (module Signals_name_map.Uid_with_index)) in
  let add_to_name_map m =
    name_map := Map.merge_skewed !name_map m ~combine:(fun ~key:_ v1 _v2 -> v1)
  in
  let rec output_circuit blackbox circuit hierarchy_path =
    let circuit_name = Circuit.name circuit in
    if not (Hash_set.mem circuits_already_output circuit_name)
    then (
      Hash_set.add circuits_already_output circuit_name;
      let hierarchy_path = Hierarchy_path.push hierarchy_path circuit_name in
      match (blackbox : Blackbox.t) with
      | None ->
        output_instantitions (None : Blackbox.t) circuit hierarchy_path;
        Output.output_circuit false output circuit hierarchy_path |> add_to_name_map
      | Top -> Output.output_circuit true output circuit hierarchy_path |> add_to_name_map
      | Instantiations ->
        output_instantitions Top circuit hierarchy_path;
        Output.output_circuit false output circuit hierarchy_path |> add_to_name_map)
  and output_instantitions (blackbox : Blackbox.t) circuit hierarchy_path =
    Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
      match signal with
      | Inst { instantiation; _ } ->
        (match Circuit_database.find database ~mangled_name:instantiation.inst_name with
         | None ->
           (* No hardcaml implementation available.  Downstream tooling will provide the
              implmentation. *)
           ()
         | Some circuit -> output_circuit blackbox circuit hierarchy_path)
      | _ -> ())
  in
  output_circuit blackbox circuit Hierarchy_path.empty;
  !name_map
;;

let output ?output_mode ?database ?blackbox language circuit =
  ignore
    (output_with_name_map ?output_mode ?database ?blackbox language circuit
      : Signals_name_map.t)
;;

let print ?database ?blackbox language circuit =
  output ~output_mode:(To_channel Out_channel.stdout) ?database ?blackbox language circuit
;;

module Digest = struct
  type t = Md5_lib.t

  let create ?database ?blackbox language circuit =
    let buffer = Buffer.create 1024 in
    output ~output_mode:(To_buffer buffer) ?database ?blackbox language circuit;
    Md5_lib.bytes (Buffer.contents_bytes buffer)
  ;;

  let to_string t = Md5_lib.to_hex t
  let to_constant t = Constant.of_hex_string ~signedness:Unsigned ~width:128 (to_string t)
  let sexp_of_t t = [%sexp_of: string] (to_string t)
  let of_verilog verilog = Md5_lib.string verilog
end

module Expert = struct
  let output_with_name_map = output_with_name_map
end
