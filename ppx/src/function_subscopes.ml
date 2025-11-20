open Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let wrap_with_subscope ~loc ~name expr =
  (* Wraps a function body with subscope creation. Only supports functions.

     Transforms:
     {v
        let%subscope my_function arg1 arg2 = body
     v}
     into:
     {v
        let my_function arg1 arg2 =
          let scope = Hardcaml.Scope.sub_scope scope "my_function" in
          body
     v}

     The [scope] variable is captured from the surrounding context using normal OCaml
     scoping rules. If a function has a [scope] parameter, that parameter shadows the
     outer scope and is used. If no [scope] exists, OCaml's type checker will error. *)
  match Ppxlib_jane.Shim.Pexp_function.of_parsetree expr.pexp_desc ~loc with
  | Some (params, constraint_, body) ->
    let subscope_binding =
      value_binding
        ~loc
        ~pat:(ppat_var ~loc (Located.mk ~loc "scope"))
        ~expr:[%expr Hardcaml.Scope.sub_scope scope [%e estring ~loc name]]
    in
    let wrap_body body_expr = pexp_let ~loc Nonrecursive [ subscope_binding ] body_expr in
    let wrapped_body : Ppxlib_jane.Shim.Pexp_function.function_body =
      match body with
      | Pfunction_body body_expr -> Pfunction_body (wrap_body body_expr)
      | Pfunction_cases (cases, cases_loc, attrs) ->
        (* Transform into: let scope = ... in function | ... *)
        let function_expr =
          { expr with
            pexp_desc =
              Ppxlib_jane.Shim.Pexp_function.to_parsetree
                ~params:[]
                ~constraint_:Ppxlib_jane.Shim.Pexp_function.Function_constraint.none
                ~body:(Pfunction_cases (cases, cases_loc, attrs))
          }
        in
        Pfunction_body (wrap_body function_expr)
    in
    { expr with
      pexp_desc =
        Ppxlib_jane.Shim.Pexp_function.to_parsetree
          ~params
          ~constraint_
          ~body:wrapped_body
    }
  | None ->
    Location.raise_errorf ~loc "[let%%subscope] only supports function definitions"
;;

let hardcaml_subscope_structure () =
  let pattern =
    (* Matches a structure item containing a let binding *)
    Ast_pattern.(pstr (pstr_value nonrecursive __ ^:: nil))
  in
  Extension.declare_inline "subscope" Structure_item pattern (fun ~loc ~path:_ bindings ->
    let bindings =
      List.map bindings ~f:(fun ({ pvb_pat; pvb_expr; _ } as vb) ->
        let binding_name =
          match pvb_pat.ppat_desc with
          | Ppat_var { txt; _ } -> txt
          | _ ->
            Location.raise_errorf
              ~loc:pvb_pat.ppat_loc
              "[let%%subscope] requires a function name binding"
        in
        { vb with pvb_expr = wrap_with_subscope ~loc ~name:binding_name pvb_expr })
    in
    [ pstr_value ~loc Nonrecursive bindings ])
;;

let hardcaml_subscope_expression () =
  let pattern =
    (* Matches let bindings in expressions: let x = e1 in e2 *)
    Ast_pattern.(single_expr_payload (pexp_let nonrecursive __ __))
  in
  Extension.declare "subscope" Expression pattern (fun ~loc ~path:_ bindings body ->
    let bindings =
      List.map bindings ~f:(fun ({ pvb_pat; pvb_expr; _ } as vb) ->
        let binding_name =
          match pvb_pat.ppat_desc with
          | Ppat_var { txt; _ } -> txt
          | _ ->
            Location.raise_errorf
              ~loc:pvb_pat.ppat_loc
              "[let%%subscope] requires a function name binding"
        in
        { vb with pvb_expr = wrap_with_subscope ~loc ~name:binding_name pvb_expr })
    in
    pexp_let ~loc Nonrecursive bindings body)
;;

let register () =
  Driver.register_transformation
    "hardcaml_subscope"
    ~rules:
      [ Context_free.Rule.extension (hardcaml_subscope_structure ())
      ; Context_free.Rule.extension (hardcaml_subscope_expression ())
      ]
;;
