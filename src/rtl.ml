(* generic rtl writing for vhdl/verilog *)

open! Core0
include Rtl_intf
module Out_channel = Stdio.Out_channel
module Language = Rtl_language

module Hierarchy_path : sig
  type t [@@deriving sexp_of]

  val empty : t
  val push : t -> string -> t
end = struct
  type t = string list

  let empty = []
  let push t s = s :: t
  let to_string_list t = List.rev t
  let sexp_of_t t = [%sexp (to_string_list t : string list)]
end

module Circuit_instance = struct
  type t =
    { circuit : Circuit.t
    ; hierarchy_path : Hierarchy_path.t
    ; rtl : Rope.t Lazy.t
    ; blackbox : Rope.t Lazy.t
    ; name_map : Rtl_ast.Signals_name_map.t Lazy.t
    }
  [@@deriving fields ~getters]

  let with_exn t ~f =
    try f () with
    | exn ->
      raise_s
        [%message
          "Error while writing circuit"
            ~circuit_name:(Circuit.name t.circuit : string)
            ~hierarchy_path:(t.hierarchy_path : Hierarchy_path.t)
            (exn : exn)]
  ;;

  let create ~(language : Language.t) ~config ~circuit ~hierarchy_path =
    let ast = lazy (Rtl_ast.of_circuit ~blackbox:false ~language ~config circuit) in
    let ast_blackbox =
      lazy (Rtl_ast.of_circuit ~blackbox:true ~language ~config circuit)
    in
    let to_rope ast =
      let ast = Lazy.force ast in
      match language with
      | Verilog -> Rtl_verilog_of_ast.to_rope ast
      | Systemverilog -> Rtl_verilog_of_ast.to_rope ast
      | Vhdl -> Rtl_vhdl_of_ast.to_rope ast
    in
    let rtl = lazy (to_rope ast) in
    let blackbox = lazy (to_rope ast_blackbox) in
    { circuit
    ; hierarchy_path
    ; rtl
    ; blackbox
    ; name_map = lazy (Rtl_ast.Signals_name_map.create (Lazy.force ast))
    }
  ;;

  let module_name t = Circuit.name t.circuit

  (* The following fields are computed when the lazy thunk is evaluated. Wrap in a
     descriptive exception. *)

  let rtl t = with_exn t ~f:(fun () -> Lazy.force t.rtl)
  let blackbox t = with_exn t ~f:(fun () -> Lazy.force t.blackbox)
  let name_map t = with_exn t ~f:(fun () -> Lazy.force t.name_map)
end

module Hierarchical_circuits = struct
  type t =
    { subcircuits : t list
    ; top : Circuit_instance.t
    }
  [@@deriving fields ~getters]

  (* Create the full design hierarchy for a single circuit - done lazily so we can extract
     the bit we actually want afterward without doing much computation. *)
  let rec maybe_create
    ~circuits_already_output
    ~database
    ~hierarchy_path
    ~config
    language
    circuit
    =
    let circuit_name = Circuit.name circuit in
    let hierarchy_path = Hierarchy_path.push hierarchy_path circuit_name in
    if Hash_set.mem circuits_already_output circuit_name
    then None
    else (
      Hash_set.add circuits_already_output circuit_name;
      Some
        { top = Circuit_instance.create ~language ~circuit ~config ~hierarchy_path
        ; subcircuits =
            List.filter_map
              (Circuit.instantiations circuit |> List.rev)
              ~f:(fun inst ->
                match Circuit_database.find database ~mangled_name:inst.circuit_name with
                | None -> None
                | Some circuit ->
                  maybe_create
                    ~circuits_already_output
                    ~database
                    ~hierarchy_path
                    ~config
                    language
                    circuit)
        })
  ;;

  let top_level_circuit_names_are_unique circuits =
    ignore
      (List.fold
         (List.map circuits ~f:Circuit.name)
         ~init:(Set.empty (module String))
         ~f:(fun name_set name ->
           if Set.mem name_set name
           then
             raise_s
               [%message "Top level circuit name has already been used" (name : string)]
           else Set.add name_set name)
       : Set.M(String).t)
  ;;

  let create ?database ?(config = Rtl_config.default) language circuits =
    top_level_circuit_names_are_unique circuits;
    let database = Option.value ~default:(Circuit_database.create ()) database in
    let circuits_already_output = Hash_set.create (module String) in
    let hierarchy_path = Hierarchy_path.empty in
    List.map circuits ~f:(fun circuit ->
      match
        maybe_create
          ~circuits_already_output
          ~database
          ~hierarchy_path
          ~config
          language
          circuit
      with
      | None -> raise_s [%message "Unable to create top level circuit"]
      | Some circuit -> circuit)
  ;;

  let top t = List.map t ~f:(fun t -> t.top)

  let subcircuits (t : t list) =
    let rec f subs all =
      List.fold_right ~init:all subs ~f:(fun sub all ->
        let all = sub.top :: all in
        f sub.subcircuits all)
    in
    let f t = f t.subcircuits [] in
    List.map t ~f |> List.concat
  ;;

  let top_level_subcircuits (t : t list) =
    List.map t ~f:(fun { top = _; subcircuits } ->
      List.map subcircuits ~f:(fun { top; subcircuits = _ } -> top))
    |> List.concat
  ;;
end

let create = Hierarchical_circuits.create

let rtl_of_hierarchical_circuits circuits =
  List.map circuits ~f:(function
    | `Rtl circuits -> List.map circuits ~f:Circuit_instance.rtl
    | `Blackbox circuits -> List.map circuits ~f:Circuit_instance.blackbox)
  |> List.concat
  |> Rope.concat
;;

let full_hierarchy circuits =
  rtl_of_hierarchical_circuits
    [ `Rtl (Hierarchical_circuits.subcircuits circuits)
    ; `Rtl (Hierarchical_circuits.top circuits)
    ]
;;

let top_levels_only (circuits : Hierarchical_circuits.t list) =
  rtl_of_hierarchical_circuits [ `Rtl (Hierarchical_circuits.top circuits) ]
;;

let top_levels_as_blackboxes (circuits : Hierarchical_circuits.t list) =
  rtl_of_hierarchical_circuits [ `Blackbox (Hierarchical_circuits.top circuits) ]
;;

let top_levels_and_blackboxes (circuits : Hierarchical_circuits.t list) =
  rtl_of_hierarchical_circuits
    [ `Blackbox (Hierarchical_circuits.subcircuits circuits)
    ; `Rtl (Hierarchical_circuits.top circuits)
    ]
;;

let print ?database ?config language circuit =
  let circuits = create ?database ?config language [ circuit ] in
  full_hierarchy circuits |> Rope.to_string |> Out_channel.print_string
;;

module Digest = struct
  type t = Md5_lib.t

  let create rope = Md5_lib.string (Rope.to_string rope)
  let to_string t = Md5_lib.to_hex t
  let to_constant t = Constant.of_hex_string ~signedness:Unsigned ~width:128 (to_string t)
  let sexp_of_t t = [%sexp_of: string] (to_string t)
end
