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
  }

let parse_rtlmangle expr ~loc =
  match expr with
  | [%expr true] -> Some [%expr "_"]
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
  | Some pre, None -> [%expr Ppx_hardcaml_runtime.concat [ [%e pre]; [%e name] ]]
  | None, Some suf -> [%expr Ppx_hardcaml_runtime.concat [ [%e name]; [%e suf] ]]
  | Some pre, Some suf ->
    [%expr Ppx_hardcaml_runtime.concat [ [%e pre]; [%e name]; [%e suf] ]] [@metaloc loc]
;;

let mangle_name ~loc name mangle =
  match mangle with
  | Some separator ->
    [%expr Ppx_hardcaml_runtime.concat [ [%e name]; [%e separator]; _n ]]
  | None -> [%expr _n]
;;

(*
 * Code generation utility functions
 *)

let expand_array_init ~loc vname label_declaration =
  let nbits = get_bits ~loc label_declaration in
  let length = get_length ~loc label_declaration in
  [%expr
    Ppx_hardcaml_runtime.Array.init [%e length] ~f:(fun _i ->
      ( Ppx_hardcaml_runtime.concat [ [%e vname]; Ppx_hardcaml_runtime.Int.to_string _i ]
      , [%e nbits] ))]
;;

let expand_array_init_str ~loc vname mapid mid label_declaration =
  let length = get_length ~loc label_declaration in
  [%expr
    Ppx_hardcaml_runtime.Array.init [%e length] ~f:(fun _i ->
      [%e mapid] [%e pexp_ident ~loc mid] ~f:(fun (_n, _b) -> [%e vname], _b))]
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
  = function
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    let rtlident = mk_rtlident ~loc name prefix suffix in
    expand_array_init ~loc rtlident label_declaration
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
    let mid = { txt = Ldot (mname, "port_names_and_widths"); loc } in
    let mangled =
      [%expr
        Ppx_hardcaml_runtime.concat
          [ [%e mangle_name ~loc name mangle]; Ppx_hardcaml_runtime.Int.to_string _i ]]
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

let expand_port_names_and_widths_label_list
  var
  loc
  label_declaration
  name
  prefix
  suffix
  mangle
  desc
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
  [%expr Ppx_hardcaml_runtime.Array.to_list [%e ainit]]
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
  match ptyp_desc with
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    let nbits = get_bits ~loc label_declaration
    and rtlident = mk_rtlident ~loc rtlname rtlprefix rtlsuffix in
    pexp_tuple ~loc [ rtlident; nbits ]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
    let mid = { txt = Ldot (mname, "port_names_and_widths"); loc } in
    let mangled = mangle_name ~loc rtlname rtlmangle in
    let rtlident = mk_rtlident ~loc mangled rtlprefix rtlsuffix in
    let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "map"))) in
    [%expr [%e mapid] [%e pexp_ident ~loc mid] ~f:(fun (_n, _b) -> [%e rtlident], _b)]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_port_names_and_widths_label_list
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
    expand_port_names_and_widths_label_array
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
    | ptyp_desc -> expand_inner_expression ptyp_desc
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
    | Iter -> [%expr Ppx_hardcaml_runtime.option_iter2_exn]
    | Map -> [%expr Ppx_hardcaml_runtime.option_map2_exn]
  ;;

  let array_map t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime.Array.iter]
    | Map -> [%expr Ppx_hardcaml_runtime.Array.map]
  ;;

  let array_init t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime.Array.for_]
    | Map -> [%expr Ppx_hardcaml_runtime.Array.init]
  ;;

  let list_map t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime.List.iter]
    | Map -> [%expr Ppx_hardcaml_runtime.List.map]
  ;;

  let list_map2_exn t loc =
    match t with
    | Iter -> [%expr Ppx_hardcaml_runtime.List.iter2_exn]
    | Map -> [%expr Ppx_hardcaml_runtime.List.map2_exn]
  ;;
end

let expand_map_label_list iter_or_map var loc ident = function
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    [%expr [%e Iter_or_map.list_map iter_or_map loc] [%e ident] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
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

let expand_map_label_array iter_or_map var loc ident = function
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    [%expr [%e Iter_or_map.array_map iter_or_map loc] [%e ident] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
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

module Record_field = struct
  type t =
    { label_declaration : label_declaration
    ; expression : expression
    }

  let expression t = t.expression
  let label_text t = t.label_declaration.pld_name.txt
end

let expand_map_label_expression (iter_or_map : Iter_or_map.t) var loc ptyp_desc ident =
  match ptyp_desc with
  (* 'a *)
  | Ptyp_var v when String.equal v var -> [%expr f [%e ident]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
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

let expand_map2_label_list iter_or_map var loc ident0 ident1 = function
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    [%expr [%e Iter_or_map.list_map2_exn iter_or_map loc] [%e ident0] [%e ident1] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
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

let expand_map2_label_array iter_or_map var loc ident0 ident1 = function
  (* 'a *)
  | Ptyp_var v when String.equal v var ->
    [%expr
      [%e Iter_or_map.array_init iter_or_map loc]
        (Array.length [%e ident0])
        ~f:(fun _i ->
          f
            (Ppx_hardcaml_runtime.Array.get [%e ident0] _i)
            (Ppx_hardcaml_runtime.Array.get [%e ident1] _i))]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr
      [%e Iter_or_map.array_init iter_or_map loc]
        (Array.length [%e ident0])
        ~f:(fun _i ->
          [%e mapid]
            (Ppx_hardcaml_runtime.Array.get [%e ident0] _i)
            (Ppx_hardcaml_runtime.Array.get [%e ident1] _i)
            ~f)]
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
  match ptyp_desc with
  (* 'a *)
  | Ptyp_var v when String.equal v var -> [%expr f [%e ident0] [%e ident1]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr [%e mapid] [%e ident0] [%e ident1] ~f]
  (* 'a list, 'a Module.t list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map2_label_list iter_or_map var loc ident0 ident1 ptyp_desc
  (* 'a array, 'a Module.t array *)
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc; _ } ]) ->
    expand_map2_label_array iter_or_map var loc ident0 ident1 ptyp_desc
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

let expand_to_list_label_list var loc ident = function
  (* 'a *)
  | Ptyp_var v when String.equal v var -> ident
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
    let to_list_id = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "to_list"))) in
    [%expr
      Ppx_hardcaml_runtime.List.concat
        (Ppx_hardcaml_runtime.List.map [%e ident] ~f:(fun _e -> [%e to_list_id] _e))]
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
    [%expr Ppx_hardcaml_runtime.Array.to_list [%e ident]]
    desc
;;

let expand_to_list_label_expression var loc ptyp_desc ident =
  match ptyp_desc with
  (* 'a *)
  | Ptyp_var v when String.equal v var -> [%expr [ [%e ident] ]]
  (* 'a *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var -> [%expr Base.Option.to_list [%e ident]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
    when String.equal v var ->
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
      match ptyp_desc with
      (* 'a *)
      | Ptyp_var v when String.equal v var -> signal (), [%expr None]
      (* 'a Module.t *)
      | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
        when String.equal v var -> module_ mname, [%expr None]
      (* 'a list *)
      | Ptyp_constr ({ txt = Lident "list"; _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
        when String.equal v var -> signal (), sequence [%expr List]
      (* 'a Module.t list *)
      | Ptyp_constr
          ( { txt = Lident "list"; _ }
          , [ { ptyp_desc =
                  Ptyp_constr
                    ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
              ; _
              }
            ] )
        when String.equal v var -> module_ mname, sequence [%expr List]
      (* 'a array *)
      | Ptyp_constr ({ txt = Lident "array"; _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
        when String.equal v var -> signal (), sequence [%expr Array]
      (* 'a Module.t array *)
      | Ptyp_constr
          ( { txt = Lident "array"; _ }
          , [ { ptyp_desc =
                  Ptyp_constr
                    ({ txt = Ldot (mname, _); _ }, [ { ptyp_desc = Ptyp_var v; _ } ])
              ; _
              }
            ] )
        when String.equal v var -> module_ mname, sequence [%expr Array]
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
  match type_decl.ptype_kind, type_decl.ptype_params with
  | Ptype_record labels, [ ({ ptyp_desc = Ptyp_var var; _ }, _) ] ->
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
      [%expr fun x -> Ppx_hardcaml_runtime.List.concat [%e str_to_list_args]]
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
         ; value_binding ~loc ~pat:(pvar ~loc "iter") ~expr:(str_map Iter)
         ; value_binding ~loc ~pat:(pvar ~loc "iter2") ~expr:(str_map2 Iter)
         ; value_binding ~loc ~pat:(pvar ~loc "map") ~expr:(str_map Map)
         ; value_binding ~loc ~pat:(pvar ~loc "map2") ~expr:(str_map2 Map)
         ; value_binding ~loc ~pat:(pvar ~loc "to_list") ~expr:str_to_list
         ]
         @
         if options.ast
         then [ value_binding ~loc ~pat:(pvar ~loc "ast") ~expr:(str_ast ()) ]
         else [])
    ; [%stri
        include Ppx_hardcaml_runtime.Interface.Make (struct
          type nonrec 'a t = 'a t

          let sexp_of_t = sexp_of_t
          let port_names_and_widths = port_names_and_widths
          let iter = iter
          let iter2 = iter2
          let map = map
          let map2 = map2
          let to_list = to_list
        end)]
    ]
  | _ -> raise_errorf ~loc "[%s] str_of_type: only supports record types" deriver
;;

let sig_of_type ~ast ({ ptype_loc = loc; _ } as type_decl) =
  match type_decl.ptype_kind, type_decl.ptype_params with
  | Ptype_record _, [ ({ ptyp_desc = Ptyp_var _; _ }, _) ] ->
    let intf = [%sigi: include Ppx_hardcaml_runtime.Interface.S with type 'a t := 'a t] in
    if ast
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
      List.map bindings ~f:(fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        (* The [pvb_pat] must be a simple assignment to a name right now. Maybe we
              can add support for structure unpacking later. *)
        let loc = { pvb_loc with loc_ghost = true } in
        match pvb_pat.ppat_desc with
        | Ppat_var { txt; loc = _ } ->
          { pvb_pat
          ; pvb_expr =
              [%expr [%e generate_naming_function ~arg ~loc ~name:txt] [%e pvb_expr]]
          ; pvb_attributes
          ; pvb_loc
          }
        | _ ->
          Location.raise_errorf
            ~loc:pvb_pat.ppat_loc
            "This form of let binding is not currently supported")
    in
    pexp_let ~loc Nonrecursive bindings rhs)
;;

let hardcaml_name () =
  declare_let_binding_extension
    ~name:"hw"
    ~generate_naming_function:(fun ~arg ~loc ~name ->
    match arg with
    | None ->
      [%expr
        fun signal_to_name ->
          Hardcaml.Scope.naming scope signal_to_name [%e estring ~loc name]]
    | Some { loc = _; txt = module_of_type_of_expression_being_named } ->
      let apply_names =
        String.concat
          ~sep:"."
          (Longident.flatten_exn module_of_type_of_expression_being_named
           @ [ "apply_names" ])
      in
      [%expr
        fun intf_to_name ->
          (* Ignore the result of 'apply_names'. Of_always.apply_names returns a unit,
               but Of_signal.apply_names just returns a Signal that we want to ignore. It
               is assumed that a scope named [scope] is present when calling this
               fucntion. *)
          let (_ : _) =
            [%e evar ~loc apply_names]
              ~prefix:[%e estring ~loc (name ^ "$")]
              ~naming_op:(Hardcaml.Scope.naming scope)
              intf_to_name
          in
          intf_to_name])
;;

let hardcaml_name_var () =
  declare_let_binding_extension
    ~name:"hw_var"
    ~generate_naming_function:(fun ~arg ~loc ~name ->
    match arg with
    | None ->
      [%expr
        fun (variable_to_name : Hardcaml.Always.Variable.t) ->
          let (_ : Hardcaml.Signal.t) =
            Hardcaml.Scope.naming scope variable_to_name.value [%e estring ~loc name]
          in
          variable_to_name]
    | Some _ ->
      Location.raise_errorf
        ~loc
        "[hw_var] does not take a module argument. It is only used with plain \
         [Variable.t]s - use [let%%hw.Your_type_here.Of_always] instead")
;;

let () =
  let get_bool_option ~loc option = Option.bind option ~f:(parse_rtlmangle ~loc) in
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
             +> flag "ast")
           (fun ~loc ~path:_ (_, type_declarations) rtlprefix rtlsuffix rtlmangle ast ->
             let options =
               { rtlprefix; rtlsuffix; rtlmangle = get_bool_option ~loc rtlmangle; ast }
             in
             List.concat_map type_declarations ~f:(fun decl -> str_of_type ~options decl)))
      ~sig_type_decl:
        (Deriving.Generator.make
           Deriving.Args.(empty +> flag "ast")
           (fun ~loc:_ ~path:_ (_, type_declarations) ast ->
             List.concat_map type_declarations ~f:(sig_of_type ~ast)))
  in
  (* Ordering of the derivers of the alias below matters. Empirically, the
     derivers are expanded in reverse order of the list.
  *)
  Deriving.add_alias deriver [ hardcaml_internal; Ppx_sexp_conv.sexp_of ]
  |> Deriving.ignore;
  Driver.register_transformation
    "hardcaml_naming"
    ~rules:
      [ Context_free.Rule.extension (hardcaml_name ())
      ; Context_free.Rule.extension (hardcaml_name_var ())
      ]
;;
