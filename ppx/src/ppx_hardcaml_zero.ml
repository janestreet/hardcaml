open Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let deriver = "hardcaml"
let raise_errorf = Location.raise_errorf

(*
   * Option parsing
*)

type options_t =
  { rtlprefix : expression option
  ; rtlsuffix : expression option
  ; rtlmangle : expression option
  ; ast : bool
  ; derive_from_map2 : bool
  ; pre : bool
  }

let parse_rtlmangle expr ~loc =
  match expr with
  | [%expr true] -> Some [%expr "$"]
  | [%expr false] -> None
  | e -> Some e
;;

(*
   * Attribute definition and parsing
*)

module Attribute : sig
  type t

  val find : t -> label_declaration -> expression option
  val exists : t
  val bits : t
  val length : t
  val rtlmangle : t
  val rtlname : t
  val rtlprefix : t
  val rtlsuffix : t
  val doc : t
end = struct
  type t = (label_declaration, expression) Attribute.t

  let find t label_declaration = Attribute.get t label_declaration

  let create name =
    Attribute.declare
      name
      Label_declaration
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)
  ;;

  let exists = create "exists"
  let bits = create "bits"
  let length = create "length"
  let rtlmangle = create "rtlmangle"
  let rtlname = create "rtlname"
  let rtlprefix = create "rtlprefix"
  let rtlsuffix = create "rtlsuffix"

  (* This represents the [ocaml.doc] attribute, which maps to documentation comments. The
     leading [hardcaml.] token is required to bypass some compiler (or ppx) related
     checks. I mention it because it's an undocumented hack. *)
  let doc = create "hardcaml.ocaml.doc"
end

let get_bits ~loc label_declaration =
  match Attribute.(find bits) label_declaration with
  | Some expr -> expr
  | None -> pexp_constant ~loc (Pconst_integer ("1", None))
;;

let get_length ~loc label_declaration =
  match Attribute.(find length) label_declaration with
  | Some expr -> expr
  | None -> raise_errorf ~loc "[%s] length attribute must be set" deriver
;;

let field_name ~loc txt = pexp_constant ~loc (Pconst_string (txt, loc, None))

let get_rtlname ~loc txt label_declaration =
  match Attribute.(find rtlname) label_declaration with
  | Some expr -> expr
  | None -> field_name ~loc txt
;;

let get_rtlprefix ~loc:_ opts label_declaration =
  match Attribute.(find rtlprefix) label_declaration with
  | Some expr -> Some expr
  | None -> opts.rtlprefix
;;

let get_rtlsuffix ~loc:_ opts label_declaration =
  match Attribute.(find rtlsuffix) label_declaration with
  | Some expr -> Some expr
  | None -> opts.rtlsuffix
;;

let get_rtlmangle ~loc opts label_declaration =
  match Attribute.(find rtlmangle) label_declaration with
  | Some expr -> parse_rtlmangle expr ~loc
  | None -> opts.rtlmangle
;;

let get_doc ~loc label_declaration =
  match Attribute.(find doc) label_declaration with
  | Some expr ->
    (match expr.pexp_desc with
     | Pexp_constant (Pconst_string (str, _, _)) -> Some str
     | _ -> raise_errorf ~loc "[%s] doc atttribute must be a string" deriver)
  | None -> None
;;

(*
   * Identifier manipulation
*)

let mk_rtlident ~loc name prefix suffix =
  match prefix, suffix with
  | None, None -> [%expr [%e name]]
  | Some pre, None -> [%expr Ppx_hardcaml_runtime0.concat [ [%e pre]; [%e name] ]]
  | None, Some suf -> [%expr Ppx_hardcaml_runtime0.concat [ [%e name]; [%e suf] ]]
  | Some pre, Some suf ->
    [%expr Ppx_hardcaml_runtime0.concat [ [%e pre]; [%e name]; [%e suf] ]] [@metaloc loc]
;;

let mangle_name ~loc name mangle =
  match mangle with
  | Some separator ->
    [%expr Ppx_hardcaml_runtime0.concat [ [%e name]; [%e separator]; _n ]]
  | None -> [%expr _n]
;;

(*
   * Code generation utility functions
*)

let expand_array_init ~loc vname label_declaration =
  let nbits = get_bits ~loc label_declaration in
  let length = get_length ~loc label_declaration in
  [%expr
    Ppx_hardcaml_runtime0.Array.init [%e length] ~f:(fun _i ->
      ( Ppx_hardcaml_runtime0.concat [ [%e vname]; Ppx_hardcaml_runtime0.Int.to_string _i ]
      , [%e nbits] ))]
;;

let expand_array_init_str ~loc vname mapid mid label_declaration =
  let length = get_length ~loc label_declaration in
  [%expr
    Ppx_hardcaml_runtime0.Array.init [%e length] ~f:(fun _i ->
      [%e mapid] [%e pexp_ident ~loc mid] ~f:(fun (_n, _b) -> [%e vname], _b))]
;;

let is_ptyp_var_with_name typ name =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc with
  | Ptyp_var (v, _) when String.equal v name -> true
  | _ -> false
;;

(*
   * Expand t label
*)

let expand_port_names_and_widths_label_array
  var
  loc
  label_declaration
  name
  prefix
  suffix
  mangle
  typ
  =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    let rtlident = mk_rtlident ~loc name prefix suffix in
    expand_array_init ~loc rtlident label_declaration
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    ->
    let mid = { txt = Ldot (mname, "port_names_and_widths"); loc } in
    let mangled =
      [%expr
        Ppx_hardcaml_runtime0.concat
          [ [%e mangle_name ~loc name mangle]; Ppx_hardcaml_runtime0.Int.to_string _i ]]
    in
    let rtlident = mk_rtlident ~loc mangled prefix suffix in
    let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "map"))) in
    expand_array_init_str ~loc rtlident mapid mid label_declaration
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_port_names_and_widths_label_array: only supports abstract record \
       labels"
      deriver
;;

let expand_port_names_and_widths_label
  var
  loc
  label_declaration
  name
  prefix
  suffix
  mangle
  desc
  ~of_array
  =
  let ainit =
    expand_port_names_and_widths_label_array
      var
      loc
      label_declaration
      name
      prefix
      suffix
      mangle
      desc
  in
  match of_array with
  | None -> ainit
  | Some of_array -> [%expr [%e of_array] [%e ainit]]
;;

let expand_port_names_and_widths_expresion
  opts
  var
  ({ pld_name = { txt; loc; _ }; _ } as label_declaration)
  ptyp_desc
  =
  let rtlname = get_rtlname ~loc txt label_declaration
  and rtlprefix = get_rtlprefix ~loc opts label_declaration
  and rtlsuffix = get_rtlsuffix ~loc opts label_declaration
  and rtlmangle = get_rtlmangle ~loc opts label_declaration in
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    let nbits = get_bits ~loc label_declaration
    and rtlident = mk_rtlident ~loc rtlname rtlprefix rtlsuffix in
    pexp_tuple ~loc [ rtlident; nbits ]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    ->
    let mid = { txt = Ldot (mname, "port_names_and_widths"); loc } in
    let mangled = mangle_name ~loc rtlname rtlmangle in
    let rtlident = mk_rtlident ~loc mangled rtlprefix rtlsuffix in
    let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "map"))) in
    [%expr [%e mapid] [%e pexp_ident ~loc mid] ~f:(fun (_n, _b) -> [%e rtlident], _b)]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_port_names_and_widths_label
      ~of_array:(Some [%expr Ppx_hardcaml_runtime0.Array.to_list])
      var
      loc
      label_declaration
      rtlname
      rtlprefix
      rtlsuffix
      rtlmangle
      ptyp_desc
  (* 'a array, 'a Module.t array *)
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_port_names_and_widths_label
      ~of_array:None
      var
      loc
      label_declaration
      rtlname
      rtlprefix
      rtlsuffix
      rtlmangle
      ptyp_desc
  | Ptyp_constr ({ txt = Lident "iarray"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_port_names_and_widths_label
      ~of_array:(Some [%expr Ppx_hardcaml_runtime0.Iarray.of_array])
      var
      loc
      label_declaration
      rtlname
      rtlprefix
      rtlsuffix
      rtlmangle
      ptyp_desc
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_port_names_and_widths_label: only supports abstract record labels"
      deriver
;;

let expand_port_names_and_widths_label
  opts
  var
  ({ pld_name = { txt; loc }; _ } as label_declaration)
  =
  let expand_inner_expression ptyp_desc =
    expand_port_names_and_widths_expresion opts var label_declaration ptyp_desc
  in
  let expr =
    match label_declaration.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) ->
      let exists =
        match Attribute.(find exists) label_declaration with
        | Some exists -> exists
        | None -> raise_errorf ~loc "[%s] exists attribute must be in [option]" deriver
      in
      [%expr if [%e exists] then Some [%e expand_inner_expression ptyp_desc] else None]
    | ptyp_desc ->
      (match Attribute.(find exists) label_declaration with
       | Some _ -> raise_errorf ~loc "[%s] exists attribute must be in [option]" deriver
       | None -> ());
      expand_inner_expression ptyp_desc
  in
  Located.mk ~loc (Lident txt), expr
;;

(*
   * Expand map label
*)

let mkfield var memb =
  let loc = Location.none in
  pexp_field
    ~loc
    (pexp_ident ~loc (Located.mk ~loc (Lident var)))
    (Located.mk ~loc (Lident memb))
;;

module Iter_or_map = struct
  type t =
    | Iter
    | Map

  let name = function
    | Iter -> "iter"
    | Map -> "map"
  ;;

  let name2 = function
    | Iter -> "iter2"
    | Map -> "map2"
  ;;

  let option_map t loc =
    match t with
    | Iter -> [%expr Base.Option.iter]
    | Map -> [%expr Base.Option.map]
  ;;

  let option_map2_exn t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.option_iter2_exn]
    | Map -> [%expr Ppx_hardcaml_runtime0.option_map2_exn]
  ;;

  let array_map t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.Array.iter]
    | Map -> [%expr Ppx_hardcaml_runtime0.Array.map]
  ;;

  let array_init t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.Array.for_]
    | Map -> [%expr Ppx_hardcaml_runtime0.Array.init]
  ;;

  let iarray_map t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.Iarray.iter]
    | Map -> [%expr Ppx_hardcaml_runtime0.Iarray.map]
  ;;

  let iarray_init t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.Iarray.for_]
    | Map -> [%expr Ppx_hardcaml_runtime0.Iarray.init]
  ;;

  let list_map t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.List.iter]
    | Map -> [%expr Ppx_hardcaml_runtime0.List.map]
  ;;

  let list_map2_exn t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime0.List.iter2_exn]
    | Map -> [%expr Ppx_hardcaml_runtime0.List.map2_exn]
  ;;
end

let expand_map_label_list iter_or_map var loc ident typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr [%e Iter_or_map.list_map iter_or_map loc] [%e ident] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr
      [%e Iter_or_map.list_map iter_or_map loc] [%e ident] ~f:(fun _e -> [%e mapid] _e ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

let expand_map_label_array iter_or_map var loc ident typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr [%e Iter_or_map.array_map iter_or_map loc] [%e ident] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr
      [%e Iter_or_map.array_map iter_or_map loc] [%e ident] ~f:(fun _e ->
        [%e mapid] _e ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

let expand_map_label_iarray iter_or_map var loc ident typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr [%e Iter_or_map.iarray_map iter_or_map loc] [%e ident] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr
      [%e Iter_or_map.iarray_map iter_or_map loc] [%e ident] ~f:(fun _e ->
        [%e mapid] _e ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

module Record_field = struct
  type t =
    { label_declaration : label_declaration
    ; expression : expression
    }

  let expression t = t.expression
  let label_text t = t.label_declaration.pld_name.txt
end

let expand_map_label_expression (iter_or_map : Iter_or_map.t) var loc ptyp_desc ident =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var -> [%expr f [%e ident]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr [%e mapid] [%e ident] ~f]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map_label_list iter_or_map var loc ident ptyp_desc
  (* 'a array, 'a Module.t array *)
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map_label_array iter_or_map var loc ident ptyp_desc
  (* 'a iarray, 'a Module.t iarray *)
  | Ptyp_constr ({ txt = Lident "iarray"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map_label_iarray iter_or_map var loc ident ptyp_desc
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label: only supports abstract record labels"
      deriver
;;

let expand_map_label iter_or_map var ({ pld_name = { txt; loc }; _ } as label_declaration)
  : Record_field.t
  =
  let expand_inner_expression ptyp_desc x =
    expand_map_label_expression iter_or_map var loc ptyp_desc x
  in
  let ident = mkfield "x" txt in
  let expression =
    match label_declaration.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) ->
      [%expr
        [%e Iter_or_map.option_map iter_or_map loc]
          ~f:(fun x -> [%e expand_inner_expression ptyp_desc [%expr x]])
          [%e ident]]
    | ptyp_desc -> expand_inner_expression ptyp_desc ident
  in
  { label_declaration; expression }
;;

(*
   * Expand map2 label
*)

let expand_map2_label_list iter_or_map var loc ident0 ident1 typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr [%e Iter_or_map.list_map2_exn iter_or_map loc] [%e ident0] [%e ident1] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr
      [%e Iter_or_map.list_map2_exn iter_or_map loc]
        [%e ident0]
        [%e ident1]
        ~f:(fun _e0 _e1 -> [%e mapid] _e0 _e1 ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

let expand_map2_label_array_or_iarray
  iter_or_map
  var
  loc
  ident0
  ident1
  typ
  ~which_array_module
  =
  let array_init, array_get, array_len =
    match which_array_module with
    | `Array ->
      ( Iter_or_map.array_init iter_or_map loc
      , [%expr Ppx_hardcaml_runtime0.Array.get]
      , [%expr Ppx_hardcaml_runtime0.Array.length] )
    | `Iarray ->
      ( Iter_or_map.iarray_init iter_or_map loc
      , [%expr Ppx_hardcaml_runtime0.Iarray.get]
      , [%expr Ppx_hardcaml_runtime0.Iarray.length] )
  in
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr
      [%e array_init]
        ([%e array_len] [%e ident0])
        ~f:(fun _i -> f ([%e array_get] [%e ident0] _i) ([%e array_get] [%e ident1] _i))]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr
      [%e array_init]
        ([%e array_len] [%e ident0])
        ~f:(fun _i ->
          [%e mapid] ([%e array_get] [%e ident0] _i) ([%e array_get] [%e ident1] _i) ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

let expand_map2_label_expression
  (iter_or_map : Iter_or_map.t)
  var
  loc
  (ptyp_desc : Ppxlib.core_type_desc)
  ident0
  ident1
  =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var -> [%expr f [%e ident0] [%e ident1]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr [%e mapid] [%e ident0] [%e ident1] ~f]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map2_label_list iter_or_map var loc ident0 ident1 ptyp_desc
  (* 'a array, 'a Module.t array *)
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map2_label_array_or_iarray
      ~which_array_module:`Array
      iter_or_map
      var
      loc
      ident0
      ident1
      ptyp_desc
  (* 'a iarray, 'a Module.t iarray *)
  | Ptyp_constr ({ txt = Lident "iarray"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map2_label_array_or_iarray
      ~which_array_module:`Iarray
      iter_or_map
      var
      loc
      ident0
      ident1
      ptyp_desc
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map2_label: only supports abstract record labels"
      deriver
;;

let expand_map2_label
  iter_or_map
  var
  ({ pld_name = { txt; loc }; _ } as label_declaration)
  : Record_field.t
  =
  let expand_inner_expression ptyp_desc x0 x1 =
    expand_map2_label_expression iter_or_map var loc ptyp_desc x0 x1
  in
  let ident0 = mkfield "x0" txt in
  let ident1 = mkfield "x1" txt in
  let expression =
    match label_declaration.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) ->
      [%expr
        [%e Iter_or_map.option_map2_exn iter_or_map loc]
          ~f:(fun x0 x1 -> [%e expand_inner_expression ptyp_desc [%expr x0] [%expr x1]])
          [%e ident0]
          [%e ident1]]
    | ptyp_desc -> expand_inner_expression ptyp_desc ident0 ident1
  in
  { label_declaration; expression }
;;

(*
   * Expand to_list label
*)

let expand_to_list_label_list var loc ident typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var -> ident
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let to_list_id = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "to_list"))) in
    [%expr
      Ppx_hardcaml_runtime0.List.concat
        (Ppx_hardcaml_runtime0.List.map [%e ident] ~f:(fun _e -> [%e to_list_id] _e))]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_list: only supports abstract record labels"
      deriver
;;

let expand_to_list_label_array var loc ident desc =
  expand_to_list_label_list
    var
    loc
    [%expr Ppx_hardcaml_runtime0.Array.to_list [%e ident]]
    desc
;;

let expand_to_list_label_iarray var loc ident desc =
  expand_to_list_label_list
    var
    loc
    [%expr Ppx_hardcaml_runtime0.Iarray.to_list [%e ident]]
    desc
;;

let expand_to_list_label_expression var loc ptyp_desc ident =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var -> [%expr [ [%e ident] ]]
  (* 'a *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ v ]) when is_ptyp_var_with_name v var ->
    [%expr Base.Option.to_list [%e ident]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    pexp_apply
      ~loc
      (pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "to_list"))))
      [ Nolabel, ident ]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_to_list_label_list var loc ident ptyp_desc
  (* 'a array, 'a Module.t array *)
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_to_list_label_array var loc ident ptyp_desc
  (* 'a iarray, 'a Module.t iarray *)
  | Ptyp_constr ({ txt = Lident "iarray"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_to_list_label_iarray var loc ident ptyp_desc
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_to_list_label: only supports abstract record labels"
      deriver
;;

let expand_to_list_label var ({ pld_name = { txt; loc }; _ } as label_declaration) =
  let ident = mkfield "x" txt in
  let expand_inner_expression ptyp_desc x =
    expand_to_list_label_expression var loc ptyp_desc x
  in
  match label_declaration.pld_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) ->
    [%expr
      match [%e ident] with
      | None -> []
      | Some x -> [%e expand_inner_expression ptyp_desc [%expr x]]]
  | ptyp_desc -> expand_inner_expression ptyp_desc ident
;;

let build_expr_list labels =
  let loc = Location.none in
  List.fold_right
    labels
    ~f:(fun expr acc ->
      pexp_construct
        ~loc
        (Located.mk ~loc (Lident "::"))
        (Some (pexp_tuple ~loc [ expr; acc ])))
    ~init:(pexp_construct ~loc (Located.mk ~loc (Lident "[]")) None)
;;

(*
   * Expand ast label
*)

let expand_ast_label
  opts
  var
  ({ pld_name = { txt; loc; _ }; pld_type; _ } as label_declaration)
  =
  let expand_expr ptyp_desc =
    let rtlname = get_rtlname ~loc txt label_declaration
    and rtlprefix = get_rtlprefix ~loc opts label_declaration
    and rtlsuffix =
      get_rtlsuffix ~loc opts label_declaration
      (* and rtlmangle = get_rtlmangle ~loc opts label_declaration *)
    in
    let signal () =
      let rtlident = mk_rtlident ~loc rtlname rtlprefix rtlsuffix in
      let bits = get_bits ~loc label_declaration in
      [%expr Signal { bits = [%e bits]; rtlname = [%e rtlident] }]
    in
    let module_ mname =
      let ast = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "ast"))) in
      let mname =
        let mname = Longident.flatten_exn mname |> String.concat ~sep:"." in
        pexp_constant ~loc (Pconst_string (mname, loc, None))
      in
      [%expr Module { name = [%e mname]; ast = [%e ast] }]
    in
    let sequence kind =
      let length = get_length ~loc label_declaration in
      [%expr Some { kind = [%e kind]; length = [%e length] }]
    in
    let type_, sequence =
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
      (* 'a *)
      | Ptyp_var (v, _) when String.equal v var -> signal (), [%expr None]
      (* 'a Module.t *)
      | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var
        -> module_ mname, [%expr None]
      (* 'a list *)
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ v ]) when is_ptyp_var_with_name v var
        -> signal (), sequence [%expr List]
      (* 'a Module.t list *)
      | Ptyp_constr
          ( { txt = Lident "list"; _ }
          , [ { ptyp_desc = Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]); _ } ] )
        when is_ptyp_var_with_name v var -> module_ mname, sequence [%expr List]
      (* 'a array *)
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ v ]) when is_ptyp_var_with_name v var
        -> signal (), sequence [%expr Array]
      (* 'a Module.t array *)
      | Ptyp_constr
          ( { txt = Lident "array"; _ }
          , [ { ptyp_desc = Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]); _ } ] )
        when is_ptyp_var_with_name v var -> module_ mname, sequence [%expr Array]
      (* 'a iarray *)
      | Ptyp_constr ({ txt = Lident "iarray"; _ }, [ v ]) when is_ptyp_var_with_name v var
        -> signal (), sequence [%expr Iarray]
      (* 'a Module.t iarray *)
      | Ptyp_constr
          ( { txt = Lident "iarray"; _ }
          , [ { ptyp_desc = Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]); _ } ] )
        when is_ptyp_var_with_name v var -> module_ mname, sequence [%expr Iarray]
      (* Default *)
      | _ ->
        raise_errorf
          ~loc
          "[%s] expand_doc_label: only supports abstract record labels"
          deriver
    in
    let field_name = field_name ~loc txt in
    let doc =
      match get_doc ~loc label_declaration with
      | None -> [%expr None]
      | Some doc -> [%expr Some [%e pexp_constant ~loc (Pconst_string (doc, loc, None))]]
    in
    [%expr
      { Ppx_hardcaml_runtime.Interface.Ast.Field.name = [%e field_name]
      ; type_ = [%e type_]
      ; sequence = [%e sequence]
      ; doc = [%e doc]
      }]
  in
  expand_expr pld_type.ptyp_desc
;;

(*
   * PPX deriving
*)

let pexp_sequenceN ~loc exprs =
  match List.rev exprs with
  | [] -> [%expr ()]
  | [ e ] -> e
  | last :: es -> List.fold es ~init:last ~f:(fun ac e -> pexp_sequence ~loc e ac)
;;

let record_fields (iter_or_map : Iter_or_map.t) ~loc fields =
  match iter_or_map with
  | Iter -> pexp_sequenceN ~loc (List.map fields ~f:Record_field.expression)
  | Map ->
    pexp_let
      ~loc
      Nonrecursive
      (List.map fields ~f:(fun record_field ->
         value_binding
           ~loc
           ~pat:(pvar ~loc (Record_field.label_text record_field))
           ~expr:record_field.expression))
      (pexp_record
         ~loc
         (List.map fields ~f:(fun record_field ->
            let id = Located.mk ~loc (Lident (Record_field.label_text record_field)) in
            id, pexp_ident ~loc id))
         None)
;;

let str_of_type ~options ({ ptype_loc = loc; _ } as type_decl) =
  let only_param =
    match type_decl.ptype_params with
    | [ (param, _) ] -> Some param.ptyp_desc
    | _ -> None
  in
  match
    ( type_decl.ptype_kind
    , Option.map only_param ~f:Ppxlib_jane.Shim.Core_type_desc.of_parsetree )
  with
  | Ptype_record labels, Some (Ptyp_var (var, _)) ->
    let str_port_names_and_widths_labels =
      List.map labels ~f:(expand_port_names_and_widths_label options var)
    in
    let str_port_names_and_widths =
      pexp_record ~loc str_port_names_and_widths_labels None
    in
    let str_map iter_or_map =
      let fields = List.map labels ~f:(expand_map_label iter_or_map var) in
      [%expr fun x ~f -> [%e record_fields iter_or_map ~loc fields]]
    in
    let str_map2 iter_or_map =
      let fields = List.map labels ~f:(expand_map2_label iter_or_map var) in
      [%expr fun x0 x1 ~f -> [%e record_fields iter_or_map ~loc fields]]
    in
    let str_to_list_labels = List.map labels ~f:(expand_to_list_label var) in
    let str_to_list_args = build_expr_list str_to_list_labels in
    let str_to_list =
      [%expr fun x -> Ppx_hardcaml_runtime0.List.concat [%e str_to_list_args]]
    in
    let str_ast_labels () = List.map labels ~f:(expand_ast_label options var) in
    let str_ast () = build_expr_list (str_ast_labels ()) in
    [ pstr_value
        ~loc
        Nonrecursive
        ([ value_binding
             ~loc
             ~pat:(pvar ~loc "port_names_and_widths")
             ~expr:str_port_names_and_widths
         ; value_binding ~loc ~pat:(pvar ~loc "map2") ~expr:(str_map2 Map)
         ]
         @
         if options.derive_from_map2
         then []
         else
           [ value_binding ~loc ~pat:(pvar ~loc "iter") ~expr:(str_map Iter)
           ; value_binding ~loc ~pat:(pvar ~loc "iter2") ~expr:(str_map2 Iter)
           ; value_binding ~loc ~pat:(pvar ~loc "map") ~expr:(str_map Map)
           ; value_binding ~loc ~pat:(pvar ~loc "to_list") ~expr:str_to_list
           ]
           @
           if options.ast
           then [ value_binding ~loc ~pat:(pvar ~loc "ast") ~expr:(str_ast ()) ]
           else [])
    ]
    @
    if options.pre
    then
      if options.derive_from_map2
      then
        (* [derive_from_map2] also generates the interface, so doesn't work here. *)
        raise_errorf
          ~loc
          "[%s] cannot specify 'pre' and 'derive_from_map2' together"
          deriver
      else []
    else
      [ (if options.derive_from_map2
         then
           [%stri
             include Ppx_hardcaml_runtime.Derive_interface_from_map2 (struct
                 type nonrec 'a t = 'a t

                 let equal = equal
                 let equal__local = equal__local
                 let sexp_of_t = sexp_of_t
                 let port_names_and_widths = port_names_and_widths
                 let map2 = map2
               end)]
         else
           [%stri
             include Ppx_hardcaml_runtime.Interface.Make (struct
                 type nonrec 'a t = 'a t

                 let equal = equal
                 let equal__local = equal__local
                 let sexp_of_t = sexp_of_t
                 let port_names_and_widths = port_names_and_widths
                 let iter = iter
                 let iter2 = iter2
                 let map = map
                 let map2 = map2
                 let to_list = to_list
               end)])
      ]
  | _ -> raise_errorf ~loc "[%s] str_of_type: only supports record types" deriver
;;

let sig_of_type ~ast ~pre ({ ptype_loc = loc; _ } as type_decl) =
  match type_decl.ptype_kind, type_decl.ptype_params with
  | Ptype_record _, [ ({ ptyp_desc = Ptyp_var _; _ }, _) ] ->
    let intf = [%sigi: include Ppx_hardcaml_runtime.Interface.S with type 'a t := 'a t] in
    if pre
    then []
    else if ast
    then [ intf; [%sigi: val ast : Ppx_hardcaml_runtime.Interface.Ast.t] ]
    else [ intf ]
  | _, _ -> raise_errorf ~loc "[%s] sig_of_type: only supports record types" deriver
;;

let declare_let_binding_extension ~name ~generate_naming_function =
  let pattern =
    (* Matches let bindings. The __' also captures the location of the [rec] flag, if
       present. The second __ captures the bindings, and the final __ captures the [rhs]
       of the let binding. *)
    Ast_pattern.(single_expr_payload (pexp_let __' __ __))
  in
  Extension.declare_with_path_arg
    name
    Expression
    pattern
    (fun ~loc ~path:_ ~arg recursive_flag bindings rhs ->
       (* We don't support recursive let bindings *)
       (match recursive_flag.txt with
        | Recursive ->
          Location.raise_errorf ~loc:recursive_flag.loc "[let rec] not supported."
        | Nonrecursive -> ());
       (* Wrap all the bindings in a naming function. Turns:
          {v
             let x = 0
             and y = 1
             in rhs
          v}
          into:
          {v
             let x = (generated_naming_function) 0
             and y = (generated_naming_function) 1
             in rhs
          v}
          We only support simple bindings like the above right now. Bindings like:

          let { x; y } = something in rhs

          aren't supported. *)
       let bindings =
         List.map bindings ~f:(fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; _ } ->
           (* The [pvb_pat] must be a simple assignment to a name right now. Maybe we
              can add support for structure unpacking later. *)
           let raise_binding_not_supported () =
             Location.raise_errorf
               ~loc:pvb_pat.ppat_loc
               "This form of let binding is not currently supported"
           in
           let loc = { pvb_loc with loc_ghost = true } in
           let jane_pattern_desc =
             Ppxlib_jane.Shim.Pattern_desc.of_parsetree pvb_pat.ppat_desc
             (* This ppx is built to operate with Jane Street's parse
             tree, so we have to use [Ppxlib_jane.Shim] to convert to/from the upstream
             parse tree (which notably doesn't support labeled tuples). *)
           in
           match jane_pattern_desc with
           | Ppat_var { txt; loc = _ }
           | Ppat_constraint ({ ppat_desc = Ppat_var { txt; loc = _ }; _ }, _, _) ->
             let vb =
               value_binding
                 ~loc:pvb_loc
                 ~pat:pvb_pat
                 ~expr:
                   [%expr [%e generate_naming_function ~arg ~loc ~name:txt] [%e pvb_expr]]
             in
             { vb with pvb_attributes }
           | Ppat_tuple (pat_list, Closed) ->
             (* {[ let ~a:c, b = expr in ]}
                transforms to
                {[ let ~a:c, b = (fun ~a:c, b -> ~a:(name c "c"), (name b "b")) expr in ]}
                 We require that all tuple components have the same type.
             *)
             let pat_list =
               List.map pat_list ~f:(function
                 (* We transform the locations of the patterns into ghost locations,
                  otherwise we get an error about pattern locations overlapping.
                  I'm not too sure that this doesn't cause any harmful side effects... *)
                 | label, p ->
                 label, { p with ppat_loc = { p.ppat_loc with loc_ghost = true } })
             in
             let tuple_naming_func =
               let tuple_component_naming_fn_calls =
                 List.map pat_list ~f:(function _, var ->
                   (match var.ppat_desc with
                    | Ppat_var { txt; loc = _ } ->
                      [%expr
                        [%e generate_naming_function ~arg ~loc ~name:txt]
                          [%e evar ~loc txt]]
                    | _ -> raise_binding_not_supported ()))
               in
               let names_with_expressions =
                 (* If the tuple is labelled, this pairs the labels with the naming expressions. *)
                 List.map2_exn
                   pat_list
                   tuple_component_naming_fn_calls
                   (* [name] here is [None] for regular tuple component,
                   [Some "a"] for ~a:c labelled component. Therefore when creating the
                   labelled tuple expression, we can use the same [name] from the pattern. *)
                   ~f:(fun (name, _) expr -> name, expr)
               in
               let body : expression =
                 { pexp_loc_stack = []
                 ; pexp_attributes = []
                 ; pexp_loc = loc
                 ; pexp_desc =
                     Ppxlib_jane.Shim.Expression_desc.(
                       Pexp_tuple names_with_expressions |> to_parsetree ~loc)
                     (* Convert the created descriptions back to upstream. *)
                 }
               in
               let arg_pat : pattern =
                 { ppat_loc_stack = pvb_pat.ppat_loc_stack
                 ; ppat_attributes = pvb_pat.ppat_attributes
                 ; ppat_loc = loc
                 ; ppat_desc =
                     Ppxlib_jane.Shim.Pattern_desc.(
                       Ppat_tuple (pat_list, Closed) |> to_parsetree ~loc)
                 }
                 (* [arg_pat] is effectively the same thing as [pvb_pat], just with
                 ghost locations *)
               in
               [%expr fun [%p arg_pat] -> [%e body]]
             in
             let vb =
               value_binding
                 ~loc:pvb_loc
                 ~pat:pvb_pat
                 ~expr:[%expr [%e tuple_naming_func] [%e pvb_expr]]
             in
             { vb with pvb_attributes }
           | _ -> raise_binding_not_supported ())
       in
       pexp_let ~loc Nonrecursive bindings rhs)
;;

(* Assumes [name], [scope], and [thing_to_name] are variables in context. *)
let name_intf_expression ~module_of_type_of_expression_being_named ~loc =
  let ppx_auto_name =
    String.concat
      ~sep:"."
      (Longident.flatten_exn module_of_type_of_expression_being_named
       @ [ "__ppx_auto_name" ])
  in
  [%expr [%e evar ~loc ppx_auto_name] thing_to_name (Hardcaml.Scope.name scope name)]
;;

let hardcaml_signal = Longident.parse "Signal"
let always_variable = Longident.parse "Always.Variable"

let hardcaml_name () =
  declare_let_binding_extension
    ~name:"hw"
    ~generate_naming_function:(fun ~arg ~loc ~name ->
      let module_of_type_of_expression_being_named =
        Option.value_map arg ~default:hardcaml_signal ~f:(fun { loc = _; txt } -> txt)
      in
      [%expr
        fun thing_to_name ->
          let name = [%e estring ~loc name] in
          [%e name_intf_expression ~module_of_type_of_expression_being_named ~loc]])
;;

let hardcaml_name_collection ~name ~mapi_function =
  declare_let_binding_extension
    ~name:("hw_" ^ name)
    ~generate_naming_function:(fun ~arg ~loc ~name ->
      let module_of_type_of_expression_being_named =
        Option.value_map arg ~default:hardcaml_signal ~f:(fun { loc = _; txt } -> txt)
      in
      [%expr
        [%e mapi_function loc] ~f:(fun idx thing_to_name ->
          let name =
            [%e estring ~loc name] ^ "$" ^ Ppx_hardcaml_runtime0.Int.to_string idx
          in
          [%e name_intf_expression ~module_of_type_of_expression_being_named ~loc])])
;;

let hardcaml_name_list () =
  hardcaml_name_collection ~name:"list" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.List.mapi])
;;

let hardcaml_name_array () =
  hardcaml_name_collection ~name:"array" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.Array.mapi])
;;

let hardcaml_name_iarray () =
  hardcaml_name_collection ~name:"iarray" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.Iarray.mapi])
;;

let raise_hw_var_doesn't_support_intfs ~loc ~hw_var_variant =
  Location.raise_errorf
    ~loc
    "[hw_var%s] does not take a module argument. It is only used with plain \
     [Variable.t]s - use [let%%hw%s.Your_type_here.Of_always] instead"
    hw_var_variant
    hw_var_variant
;;

(* Assumes [scope], [thing_to_name], [name] are variables in context *)
let name_always_variable_expr ~loc =
  name_intf_expression ~module_of_type_of_expression_being_named:always_variable ~loc
;;

let hardcaml_name_var () =
  declare_let_binding_extension
    ~name:"hw_var"
    ~generate_naming_function:(fun ~arg ~loc ~name ->
      match arg with
      | None ->
        [%expr
          fun (thing_to_name : Always.Variable.t) ->
            let name = [%e estring ~loc name] in
            [%e name_always_variable_expr ~loc]]
      | Some _ -> raise_hw_var_doesn't_support_intfs ~loc ~hw_var_variant:"")
;;

let hardcaml_name_var_collection ~name ~mapi_function =
  declare_let_binding_extension
    ~name:("hw_var_" ^ name)
    ~generate_naming_function:(fun ~arg ~loc ~name ->
      match arg with
      | None ->
        [%expr
          [%e mapi_function loc] ~f:(fun idx (thing_to_name : Always.Variable.t) ->
            let name =
              [%e estring ~loc name] ^ "$" ^ Ppx_hardcaml_runtime0.Int.to_string idx
            in
            [%e name_always_variable_expr ~loc])]
      | Some _ -> raise_hw_var_doesn't_support_intfs ~loc ~hw_var_variant:("_" ^ name))
;;

let hardcaml_name_var_list () =
  hardcaml_name_var_collection ~name:"list" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.List.mapi])
;;

let hardcaml_name_var_array () =
  hardcaml_name_var_collection ~name:"array" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.Array.mapi])
;;

let hardcaml_name_var_iarray () =
  hardcaml_name_var_collection ~name:"iarray" ~mapi_function:(fun loc ->
    [%expr Ppx_hardcaml_runtime0.Iarray.mapi])
;;

let () =
  let hardcaml_internal =
    Deriving.add
      "hardcaml_internal"
      ~str_type_decl:
        (Deriving.Generator.make
           Deriving.Args.(
             empty
             +> arg "rtlprefix" Ast_pattern.__
             +> arg "rtlsuffix" Ast_pattern.__
             +> arg "rtlmangle" Ast_pattern.__
             +> flag "ast"
             +> flag "derive_from_map2"
             +> flag "pre")
           (fun ~loc
             ~path:_
             (_, type_declarations)
             rtlprefix
             rtlsuffix
             rtlmangle
             ast
             derive_from_map2
             pre ->
             let options =
               { rtlprefix
               ; rtlsuffix
               ; rtlmangle =
                   parse_rtlmangle ~loc (Option.value rtlmangle ~default:[%expr true])
               ; ast
               ; derive_from_map2
               ; pre
               }
             in
             List.concat_map type_declarations ~f:(fun decl -> str_of_type ~options decl)))
      ~sig_type_decl:
        (Deriving.Generator.make
           Deriving.Args.(empty +> flag "ast" +> flag "pre")
           (fun ~loc:_ ~path:_ (_, type_declarations) ast pre ->
             List.concat_map type_declarations ~f:(sig_of_type ~ast ~pre)))
  in
  (* Ordering of the derivers of the alias below matters. Empirically, the
     derivers are expanded in reverse order of the list.
  *)
  Deriving.add_alias
    deriver
    [ hardcaml_internal; Ppx_sexp_conv.sexp_of; Ppx_compare.equal_local ]
  |> Deriving.ignore;
  Driver.register_transformation
    "hardcaml_naming"
    ~rules:
      [ Context_free.Rule.extension (hardcaml_name ())
      ; Context_free.Rule.extension (hardcaml_name_list ())
      ; Context_free.Rule.extension (hardcaml_name_array ())
      ; Context_free.Rule.extension (hardcaml_name_iarray ())
      ; Context_free.Rule.extension (hardcaml_name_var ())
      ; Context_free.Rule.extension (hardcaml_name_var_list ())
      ; Context_free.Rule.extension (hardcaml_name_var_array ())
      ; Context_free.Rule.extension (hardcaml_name_var_iarray ())
      ]
;;
