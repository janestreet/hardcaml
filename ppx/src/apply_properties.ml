open! Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let raise_binding_not_supported ~loc =
  Location.raise_errorf ~loc "This form of let binding is not currently supported"
;;

let pattern_name_exn pattern =
  let jane_pattern_desc = Ppxlib_jane.Shim.Pattern_desc.of_parsetree pattern.ppat_desc in
  match jane_pattern_desc with
  | Ppat_var { txt; loc = _ }
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt; loc = _ }; _ }, _, _) -> txt
  | _ -> raise_binding_not_supported ~loc:pattern.ppat_loc
;;

let property_directive_type ~loc =
  ptyp_constr ~loc (Located.mk ~loc (Longident.parse "Hardcaml.Property.Directive.t")) []
;;

let property_group_type ~loc =
  ptyp_constr
    ~loc
    (Located.mk ~loc (Longident.parse "Hardcaml.Property_manager.Group.t"))
    []
;;

let directive_pattern ~loc name =
  ppat_constraint ~loc (pvar ~loc name) (property_directive_type ~loc)
;;

let group_pattern ~loc name =
  ppat_constraint ~loc (pvar ~loc name) (property_group_type ~loc)
;;

let rec tuple_arity_of_tail_expression expr =
  match
    Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc:expr.pexp_loc
  with
  | Pexp_let (_, Nonrecursive, _ :: _, body) -> tuple_arity_of_tail_expression body
  | Pexp_extension (_, PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ]) ->
    tuple_arity_of_tail_expression expr
  | Pexp_tuple labeled_exprs -> List.length labeled_exprs
  | _ ->
    Location.raise_errorf
      ~loc:expr.pexp_loc
      "[let%%hw_persuade] requires the body to end in either [(directive, property)] or \
       [(group_name, directive, property)]"
;;

let add_property_expression ~loc ~name ~group_name ~directive =
  match group_name with
  | None ->
    [%expr
      Hardcaml_persuade.add_to_scope
        scope
        [%e estring ~loc name]
        [%e evar ~loc directive]
        [%e evar ~loc name]]
  | Some group_name ->
    [%expr
      Hardcaml_persuade.add_to_scope
        ~group:[%e evar ~loc group_name]
        scope
        [%e estring ~loc name]
        [%e evar ~loc directive]
        [%e evar ~loc name]]
;;

let pair_pattern ~loc ~group_name ~directive ~property =
  match group_name with
  | None -> ppat_tuple ~loc [ directive_pattern ~loc directive; pvar ~loc property ]
  | Some group_name ->
    ppat_tuple
      ~loc
      [ group_pattern ~loc group_name
      ; directive_pattern ~loc directive
      ; pvar ~loc property
      ]
;;

let expand_binding ~loc { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; _ } rhs =
  let loc = { loc with loc_ghost = true } in
  let pvb_loc = { pvb_loc with loc_ghost = true } in
  let name = pattern_name_exn pvb_pat in
  let pair = "__ppx_hardcaml_persuade_pair" in
  let group_name = "__ppx_hardcaml_persuade_group_name" in
  let directive = "__ppx_hardcaml_persuade_directive" in
  let property = "__ppx_hardcaml_persuade_property" in
  let group_name =
    match tuple_arity_of_tail_expression pvb_expr with
    | 2 -> None
    | 3 -> Some group_name
    | arity ->
      Location.raise_errorf
        ~loc:pvb_expr.pexp_loc
        "[let%%hw_persuade] expected a 2-tuple or 3-tuple, got a %d-tuple"
        arity
  in
  pexp_let
    ~loc
    Nonrecursive
    [ value_binding
        ~loc:pvb_loc
        ~pat:(pvar ~loc:pvb_loc pair)
        ~expr:
          [%expr
            let open Hardcaml_persuade in
            [%e pvb_expr]]
    ]
    (pexp_let
       ~loc
       Nonrecursive
       [ value_binding
           ~loc:pvb_loc
           ~pat:(pair_pattern ~loc ~group_name ~directive ~property)
           ~expr:(evar ~loc pair)
       ]
       (pexp_let
          ~loc
          Nonrecursive
          [ { (value_binding ~loc:pvb_loc ~pat:pvb_pat ~expr:(evar ~loc property)) with
              pvb_attributes
            }
          ]
          (pexp_sequence
             ~loc
             (add_property_expression ~loc ~name ~group_name ~directive)
             rhs)))
;;

let hardcaml_persuade () =
  let pattern = Ast_pattern.(single_expr_payload (pexp_let __' __ __)) in
  Extension.declare
    "hw_persuade"
    Expression
    pattern
    (fun ~loc ~path:_ recursive_flag bindings rhs ->
       (match recursive_flag.txt with
        | Recursive ->
          Location.raise_errorf ~loc:recursive_flag.loc "[let rec] not supported."
        | Nonrecursive -> ());
       List.fold_right bindings ~init:rhs ~f:(fun binding rhs ->
         expand_binding ~loc binding rhs))
;;

let register () =
  Driver.register_transformation
    "hardcaml_properties"
    ~rules:[ Context_free.Rule.extension (hardcaml_persuade ()) ]
;;
