open Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let declare_let_binding_extension ~name ~generate_naming_function =
  let pattern =
    (* Matches let bindings. The __' also captures the location of the [mutable] and [rec]
       flags, if present. The third __ captures the bindings, and the final __ captures
       the [rhs] of the let binding. *)
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

          let [{ x; y }] = something in rhs

          aren't supported. *)
       let bindings =
         List.map bindings ~f:(fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; _ } ->
           (* The [pvb_pat] must be a simple assignment to a name right now. Maybe we can
              add support for structure unpacking later. *)
           let raise_binding_not_supported () =
             Location.raise_errorf
               ~loc:pvb_pat.ppat_loc
               "This form of let binding is not currently supported"
           in
           let loc = { pvb_loc with loc_ghost = true } in
           let jane_pattern_desc =
             Ppxlib_jane.Shim.Pattern_desc.of_parsetree pvb_pat.ppat_desc
             (* This ppx is built to operate with Jane Street's parse tree, so we have to
                use [Ppxlib_jane.Shim] to convert to/from the upstream parse tree (which
                notably doesn't support labeled tuples). *)
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
             (* {[
                  let ~a:c, b = expr in
                ]}
                transforms to
                {[
                  let ~a:c, b = (fun ~a:c, b -> ~a:(name c "c"), (name b "b")) expr in
                ]}
                We require that all tuple components have the same type.
             *)
             let pat_list =
               List.map pat_list ~f:(function
                 (* We transform the locations of the patterns into ghost locations,
                    otherwise we get an error about pattern locations overlapping. I'm not
                    too sure that this doesn't cause any harmful side effects... *)
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
                 (* If the tuple is labelled, this pairs the labels with the naming
                    expressions. *)
                 List.map2_exn
                   pat_list
                   tuple_component_naming_fn_calls
                   (* [name] here is [None] for regular tuple component, [Some "a"] for
                      ~a:c labelled component. Therefore when creating the labelled tuple
                      expression, we can use the same [name] from the pattern. *)
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
                 (* [arg_pat] is effectively the same thing as [pvb_pat], just with ghost
                    locations *)
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

let hardcaml_name_collection collection =
  let name = Collection.to_string collection in
  let mapi_function = Collection.mapi collection ~iter_or_map:Map in
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

let hardcaml_name_var_collection collection =
  let name = Collection.to_string collection in
  let mapi_function = Collection.mapi collection ~iter_or_map:Map in
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

let register () =
  Driver.register_transformation
    "hardcaml_naming"
    ~rules:
      [ Context_free.Rule.extension (hardcaml_name ())
      ; Context_free.Rule.extension (hardcaml_name_collection List)
      ; Context_free.Rule.extension (hardcaml_name_collection Array)
      ; Context_free.Rule.extension (hardcaml_name_collection Iarray)
      ; Context_free.Rule.extension (hardcaml_name_var ())
      ; Context_free.Rule.extension (hardcaml_name_var_collection List)
      ; Context_free.Rule.extension (hardcaml_name_var_collection Array)
      ; Context_free.Rule.extension (hardcaml_name_var_collection Iarray)
      ]
;;
