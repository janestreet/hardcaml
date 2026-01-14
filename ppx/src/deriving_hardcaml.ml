open Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let deriver = "hardcaml"
let raise_errorf = Location.raise_errorf

let parse_rtlmangle expr ~loc =
  match expr with
  | [%expr true] -> Some [%expr "$"]
  | [%expr false] -> None
  | e -> Some e
;;

let field_name ~loc txt = pexp_constant ~loc (Pconst_string (txt, loc, None))

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

let expand_names_and_widths_init ~loc ~collection vname label_declaration =
  let nbits = Label_attribute.get_bits_with_default ~loc label_declaration in
  let length = Label_attribute.get_length ~loc label_declaration in
  let init = Collection.init collection loc in
  [%expr
    [%e init] [%e length] ~f:(fun _i ->
      ( Ppx_hardcaml_runtime0.concat
          [ [%e vname]; "_"; Ppx_hardcaml_runtime0.Int.to_string _i ]
      , [%e nbits] ))]
;;

let expand_names_and_widths_init_with_mangle
  ~loc
  ~collection
  vname
  mapid
  mid
  label_declaration
  =
  let length = Label_attribute.get_length ~loc label_declaration in
  let init = Collection.init collection loc in
  [%expr
    [%e init] [%e length] ~f:(fun _i ->
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

let expand_port_names_and_widths_label_array_like
  ~collection
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
    expand_names_and_widths_init ~loc ~collection rtlident label_declaration
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    ->
    let mid = { txt = Ldot (mname, "port_names_and_widths"); loc } in
    let mangled =
      [%expr
        Ppx_hardcaml_runtime0.concat
          [ [%e mangle_name ~loc name mangle]
          ; "_"
          ; Ppx_hardcaml_runtime0.Int.to_string _i
          ]]
    in
    let rtlident = mk_rtlident ~loc mangled prefix suffix in
    let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "map"))) in
    expand_names_and_widths_init_with_mangle
      ~loc
      ~collection
      rtlident
      mapid
      mid
      label_declaration
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_port_names_and_widths_label_array_like: only supports abstract record \
       labels"
      deriver
;;

let expand_port_names_and_widths_expresion
  opts
  var
  ({ pld_name = { txt; loc; _ }; _ } as label_declaration)
  ptyp_desc
  =
  let rtlname = Label_attribute.get_rtlname ~loc txt label_declaration
  and rtlprefix = Label_attribute.get_rtlprefix ~loc ~opts label_declaration
  and rtlsuffix = Label_attribute.get_rtlsuffix ~loc ~opts label_declaration
  and rtlmangle = Label_attribute.get_rtlmangle ~loc ~opts label_declaration in
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    let nbits = Label_attribute.get_bits_with_default ~loc label_declaration
    and rtlident = mk_rtlident ~loc rtlname rtlprefix rtlsuffix in
    pexp_tuple ~loc [ rtlident; nbits ]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    ->
    let nbits = Label_attribute.get_bits_opt ~loc label_declaration in
    let port_names_and_widths =
      match nbits with
      | None ->
        (* The normal case is that you don't specify number of bits when using an
           interface in an interface, since interfaces have a fixed width already. *)
        { txt = Ldot (mname, "port_names_and_widths"); loc } |> pexp_ident ~loc
      | Some nbits ->
        (* For interfaces like With_valid.t, the width is set dynamically based on the
           nbits attribute. *)
        let fn =
          { txt = Ldot (mname, "port_names_and_widths_dynamic"); loc } |> pexp_ident ~loc
        in
        [%expr [%e fn] ~nbits:[%e nbits]]
    in
    let mangled = mangle_name ~loc rtlname rtlmangle in
    let rtlident = mk_rtlident ~loc mangled rtlprefix rtlsuffix in
    let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (mname, "map"))) in
    [%expr [%e mapid] [%e port_names_and_widths] ~f:(fun (_n, _b) -> [%e rtlident], _b)]
  (* 'a list/array/iarray, 'a Module.t list/array/iarray *)
  | Ptyp_constr ({ txt = Lident collection; _ }, [ { ptyp_desc; _ } ])
    when Collection.is_supported collection ->
    expand_port_names_and_widths_label_array_like
      ~collection:(Collection.of_string collection)
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
      "[%s] expand_port_names_and_widths_expression: only supports abstract record labels"
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
      let exists = Label_attribute.get_exists ~loc label_declaration in
      [%expr if [%e exists] then Some [%e expand_inner_expression ptyp_desc] else None]
    | ptyp_desc ->
      if Label_attribute.has_exists ~loc label_declaration
      then raise_errorf ~loc "[%s] exists attribute only supported in [option]" deriver;
      expand_inner_expression ptyp_desc
  in
  Located.mk ~loc (Lident txt), expr
;;

let expand_signal_attribute_label_collection
  ~collection
  ~attr
  ~getter
  ~type_
  var
  loc
  label_declaration
  typ
  =
  let expand_collection_init s =
    let length = Label_attribute.get_length ~loc label_declaration in
    let init = Collection.init collection loc in
    [%expr [%e init] [%e length] ~f:(fun _i -> [%e s])]
  in
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    let attribute_value = [%expr ([%e getter ~loc label_declaration] : [%t type_])] in
    expand_collection_init attribute_value
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    ->
    let attribute_values = { txt = Ldot (mname, attr); loc } in
    let attribute_values = pexp_ident ~loc attribute_values in
    expand_collection_init attribute_values
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_signal_attribute_label_collection: only supports abstract record \
       labels"
      deriver
;;

let expand_signal_attribute_expresion
  ~attr
  ~getter
  ~type_
  var
  ({ pld_name = { loc; _ }; _ } as label_declaration)
  ptyp_desc
  =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr ([%e getter ~loc label_declaration] : [%t type_])]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); loc }, [ v ]) when is_ptyp_var_with_name v var
    -> pexp_ident ~loc { txt = Ldot (mname, attr); loc }
  (* 'a list/array/iarray, 'a Module.t list/array/iarray *)
  | Ptyp_constr ({ txt = Lident collection; _ }, [ { ptyp_desc; _ } ])
    when Collection.is_supported collection ->
    expand_signal_attribute_label_collection
      ~attr
      ~getter
      ~type_
      ~collection:(Collection.of_string collection)
      var
      loc
      label_declaration
      ptyp_desc
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_signal_attribute_expresion: only supports abstract record labels"
      deriver
;;

let expand_signal_attribute_label
  ~attr
  ~getter
  ~type_
  var
  ({ pld_name = { txt; loc }; _ } as label_declaration)
  =
  let expand_inner_expression ptyp_desc =
    expand_signal_attribute_expresion ~attr ~getter ~type_ var label_declaration ptyp_desc
  in
  let expr =
    match label_declaration.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ { ptyp_desc; _ } ]) ->
      let exists = Label_attribute.get_exists ~loc label_declaration in
      [%expr if [%e exists] then Some [%e expand_inner_expression ptyp_desc] else None]
    | ptyp_desc ->
      if Label_attribute.has_exists ~loc label_declaration
      then raise_errorf ~loc "[%s] exists attribute only supported in [option]" deriver;
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

let add_nontail_attribute e loc =
  { e with
    pexp_attributes =
      [ { attr_name = { txt = "nontail"; loc }; attr_payload = PStr []; attr_loc = loc } ]
  }
;;

let expand_map_label_collection ~collection iter_or_map var loc ident typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    add_nontail_attribute
      [%expr [%e Collection.map collection ~iter_or_map loc] [%e ident] ~f]
      loc
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr
      [%e Collection.map collection ~iter_or_map loc] [%e ident] ~f:(fun _e ->
        [%e mapid] _e ~f [@nontail])]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_collection: only supports abstract record labels"
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
  | Ptyp_var (v, _) when String.equal v var -> [%expr f [%e ident] [@nontail]]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name iter_or_map)))
    in
    [%expr [%e mapid] [%e ident] ~f [@nontail]]
  (* 'a list/array/iarray, 'a Module.t list/array/iarray *)
  | Ptyp_constr ({ txt = Lident collection; _ }, [ { ptyp_desc; _ } ])
    when Collection.is_supported collection ->
    expand_map_label_collection
      ~collection:(Collection.of_string collection)
      iter_or_map
      var
      loc
      ident
      ptyp_desc
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
      let inner_expression =
        add_nontail_attribute (expand_inner_expression ptyp_desc [%expr x]) loc
      in
      [%expr
        [%e Iter_or_map.option_map iter_or_map loc]
          ~f:(fun x -> [%e inner_expression])
          [%e ident]]
    | ptyp_desc -> expand_inner_expression ptyp_desc ident
  in
  { label_declaration; expression }
;;

(*
   * Expand map2 label
*)

let expand_map2_label_collection ~collection iter_or_map var loc ident0 ident1 typ =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ with
  (* 'a *)
  | Ptyp_var (v, _) when String.equal v var ->
    [%expr [%e Collection.map2 collection ~iter_or_map loc] [%e ident0] [%e ident1] ~f]
  (* 'a Module.t *)
  | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var ->
    let mapid =
      pexp_ident ~loc (Located.mk ~loc (Ldot (mname, Iter_or_map.name2 iter_or_map)))
    in
    [%expr
      [%e Collection.map2 collection ~iter_or_map loc]
        [%e ident0]
        [%e ident1]
        ~f:(fun _e0 _e1 -> [%e mapid] _e0 _e1 ~f)]
  (* Default *)
  | _ ->
    raise_errorf
      ~loc
      "[%s] expand_map_label_collection: only supports abstract record labels"
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
  (* 'a list/array/iarray, 'a Module.t list/array/iarray *)
  | Ptyp_constr ({ txt = Lident collection; _ }, [ { ptyp_desc; _ } ])
    when Collection.is_supported collection ->
    expand_map2_label_collection
      ~collection:(Collection.of_string collection)
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
          ~f:(fun x0 x1 ->
            [%e
              add_nontail_attribute
                (expand_inner_expression ptyp_desc [%expr x0] [%expr x1])
                loc])
          [%e ident0]
          [%e ident1]]
    | ptyp_desc -> expand_inner_expression ptyp_desc ident0 ident1
  in
  { label_declaration; expression }
;;

(*
   * Expand to_list label
*)

let expand_to_list_label_collection ~collection var loc ident typ =
  let to_list = Collection.to_list collection loc in
  let ident = [%expr [%e to_list] [%e ident]] in
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
    (* 'a list/array/iarray, 'a Module.t list/array/iarray *)
  | Ptyp_constr ({ txt = Lident collection; _ }, [ { ptyp_desc; _ } ])
    when Collection.is_supported collection ->
    expand_to_list_label_collection
      ~collection:(Collection.of_string collection)
      var
      loc
      ident
      ptyp_desc
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
    let rtlname = Label_attribute.get_rtlname ~loc txt label_declaration
    and rtlprefix = Label_attribute.get_rtlprefix ~loc ~opts label_declaration
    and rtlsuffix =
      Label_attribute.get_rtlsuffix ~loc ~opts label_declaration
      (* and rtlmangle = Label_attribute.get_rtlmangle ~loc opts label_declaration *)
    in
    let signal () =
      let rtlident = mk_rtlident ~loc rtlname rtlprefix rtlsuffix in
      let bits = Label_attribute.get_bits_with_default ~loc label_declaration in
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
      let length = Label_attribute.get_length ~loc label_declaration in
      [%expr Some { kind = [%e kind]; length = [%e length] }]
    in
    let type_, sequence =
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ptyp_desc with
      (* 'a *)
      | Ptyp_var (v, _) when String.equal v var -> signal (), [%expr None]
      (* 'a Module.t *)
      | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var
        -> module_ mname, [%expr None]
      (* 'a list/array/iarray *)
      | Ptyp_constr ({ txt = Lident collection; _ }, [ v ])
        when is_ptyp_var_with_name v var && Collection.is_supported collection ->
        signal (), sequence Collection.(to_module_expr (of_string collection) loc)
      (* 'a Module.t list/array/iarray *)
      | Ptyp_constr
          ( { txt = Lident collection; _ }
          , [ { ptyp_desc = Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]); _ } ] )
        when is_ptyp_var_with_name v var && Collection.is_supported collection ->
        module_ mname, sequence Collection.(to_module_expr (of_string collection) loc)
      (* Default *)
      | _ ->
        raise_errorf
          ~loc
          "[%s] expand_doc_label: only supports abstract record labels"
          deriver
    in
    let field_name = field_name ~loc txt in
    let doc =
      match Label_attribute.get_doc ~loc label_declaration with
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
  | Iter ->
    let num_fields = List.length fields in
    pexp_sequenceN
      ~loc
      (List.mapi fields ~f:(fun i f ->
         let e = Record_field.expression f in
         if i = num_fields - 1 then add_nontail_attribute e loc else e))
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
    let wave_formats_attr = "wave_formats" in
    let str_port_wave_formats =
      pexp_record
        ~loc
        (List.map
           labels
           ~f:
             (expand_signal_attribute_label
                ~attr:wave_formats_attr
                ~getter:Label_attribute.get_wave_format
                ~type_:[%type: Ppx_hardcaml_runtime.Wave_format.t]
                var))
        None
    in
    let str_map iter_or_map =
      let fields = List.map labels ~f:(expand_map_label iter_or_map var) in
      [%expr fun x ~(f @ local) -> [%e record_fields iter_or_map ~loc fields]]
    in
    let str_map2 iter_or_map =
      let fields = List.map labels ~f:(expand_map2_label iter_or_map var) in
      [%expr fun x0 x1 ~(f @ local) -> [%e record_fields iter_or_map ~loc fields]]
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
         @ (if options.pre
            then []
            else
              [ value_binding
                  ~loc
                  ~pat:(pvar ~loc wave_formats_attr)
                  ~expr:str_port_wave_formats
              ])
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
    else if options.derive_from_map2
    then
      [%str
        include Ppx_hardcaml_runtime.Derive_interface_from_map2 (struct
            type nonrec 'a t = 'a t

            let equal = equal
            let equal__local = equal__local
            let compare = compare
            let compare__local = compare__local
            let sexp_of_t = sexp_of_t
            let port_names_and_widths = port_names_and_widths
            let map2 = map2
            let wave_formats = wave_formats
          end)]
    else
      [%str
        include Ppx_hardcaml_runtime.Interface.Make_with_wave_formats (struct
            type nonrec 'a t = 'a t

            let equal = equal
            let equal__local = equal__local
            let compare = compare
            let compare__local = compare__local
            let sexp_of_t = sexp_of_t
            let port_names_and_widths = port_names_and_widths
            let iter = iter
            let iter2 = iter2
            let map = map
            let map2 = map2
            let to_list = to_list
            let wave_formats = wave_formats
          end)]
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

let register () =
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
               { Interface_options.rtlprefix
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
  (* Ordering of the derivers of the alias below matters. Empirically, the derivers are
     expanded in reverse order of the list.
  *)
  Deriving.add_alias
    deriver
    [ hardcaml_internal
    ; Ppx_sexp_conv.sexp_of
    ; Ppx_compare.equal_local
    ; Ppx_compare.compare_local
    ]
  |> Deriving.ignore
;;
