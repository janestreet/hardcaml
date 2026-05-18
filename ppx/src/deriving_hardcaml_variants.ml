(* Generates Hardcaml interfaces for variant types where each case wraps an inner
   interface type. For example:

   {[
     type 'a t =
       | Foo of 'a Foo.t
       | Bar of 'a Bar.t
     [@@deriving hardcaml_variants]
   ]}

   The Make functor validates that all interface operations (map2, etc.) are performed on
   values matching the configured kind. *)

open Base
open Ppxlib
open Ppxlib.Ast_builder.Default

let deriver = "hardcaml_variants"
let raise_errorf = Location.raise_errorf

let is_ptyp_var_with_name typ name =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc with
  | Ptyp_var (v, _) when String.equal v name -> true
  | _ -> false
;;

module Case = struct
  type t =
    { name : string
    ; loc : location
    ; mname : Longident.t
    }

  (* [Foo] -> "foo" *)
  let label_name c = String.uncapitalize c.name

  (* [Foo] -> "foo_exn" *)
  let extractor_fn_name c = label_name c ^ "_exn"

  (* [Foo] -> "Foo" *)
  let reexport_name c = c.name

  (* [Foo] -> ['a Foo.t]; for use in function signatures *)
  let payload_type ~loc c =
    ptyp_constr ~loc (Located.mk ~loc (Ldot (c.mname, "t"))) [ ptyp_var ~loc "a" ]
  ;;
end

(* Construct the [Kind.t] declaration. In the example at the top of this file, this would
   look like [type t = Foo | Bar [@@deriving sexp]]. *)
let kind_variant_type_decl ~loc cases =
  let constrs =
    List.map cases ~f:(fun c ->
      constructor_declaration
        ~loc:c.Case.loc
        ~name:(Located.mk ~loc:c.Case.loc c.Case.name)
        ~args:(Pcstr_tuple [])
        ~res:None)
  in
  { (type_declaration
       ~loc
       ~name:(Located.mk ~loc "t")
       ~params:[]
       ~cstrs:[]
       ~kind:(Ptype_variant constrs)
       ~private_:Public
       ~manifest:None)
    with
    ptype_attributes =
      [ { attr_name = Located.mk ~loc "deriving"
        ; attr_payload = PStr [ [%stri sexp] ]
        ; attr_loc = loc
        }
      ]
  }
;;

(* Module signature for re-exporting the inner interface. For example:
   [ | Foo of 'a Foo.t ] becomes [module Foo : Interface.S with type 'a t = 'a Foo.t]. *)
let case_interface_module_sig ~loc c =
  let interface_s =
    Located.mk ~loc (Ldot (Ldot (Lident "Ppx_hardcaml_runtime", "Interface"), "S"))
  in
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:(Located.mk ~loc (Some (Case.reexport_name c)))
       ~type_:
         (pmty_with
            ~loc
            (pmty_ident ~loc interface_s)
            [ Pwith_type
                ( Located.mk ~loc (Lident "t")
                , type_declaration
                    ~loc
                    ~name:(Located.mk ~loc "t")
                    ~params:[ ptyp_var ~loc "a", (NoVariance, NoInjectivity) ]
                    ~cstrs:[]
                    ~kind:Ptype_abstract
                    ~private_:Public
                    ~manifest:(Some (Case.payload_type ~loc c)) )
            ]))
;;

(* Signature for an _exn extractor, i.e. [val foo_exn : 'a t -> 'a Foo.t]. *)
let case_exn_val_sig ~loc c =
  psig_value
    ~loc
    (value_description
       ~loc
       ~name:(Located.mk ~loc (Case.extractor_fn_name c))
       ~type_:[%type: 'a t -> [%t Case.payload_type ~loc c]]
       ~prim:[])
;;

let extract_case ~var cd =
  match cd.pcd_args with
  | Pcstr_tuple [ tuple_arg ] ->
    let core_type = Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type tuple_arg in
    (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
     | Ptyp_constr ({ txt = Ldot (mname, _); _ }, [ v ]) when is_ptyp_var_with_name v var
       -> Some { Case.name = cd.pcd_name.txt; loc = cd.pcd_loc; mname }
     | _ -> None)
  | _ -> None
;;

let validate_constructors ~loc ~var cds =
  let cases =
    List.map cds ~f:(fun cd ->
      match extract_case ~var cd with
      | Some c -> c
      | None ->
        raise_errorf
          ~loc:cd.pcd_loc
          "[%s] each variant case must have exactly one argument of type 'a Module.t"
          deriver)
  in
  if List.is_empty cases
  then raise_errorf ~loc "[%s] variant must have at least one case" deriver;
  cases
;;

(* Helpers to build constructor patterns/expressions *)
let constr_pat ~loc name arg = ppat_construct ~loc (Located.mk ~loc (Lident name)) arg
let constr_exp ~loc name arg = pexp_construct ~loc (Located.mk ~loc (Lident name)) arg

let kind_pat ~loc name =
  ppat_construct ~loc (Located.mk ~loc (Ldot (Lident "Kind", name))) None
;;

(* Implements [_exn] extractors *)
let expand_exn_fn ~loc c =
  let pat = constr_pat ~loc c.Case.name (Some [%pat? x]) in
  let msg =
    pexp_constant ~loc (Pconst_string ("expected " ^ c.Case.name ^ ", got", loc, None))
  in
  value_binding
    ~loc
    ~pat:(pvar ~loc (Case.extractor_fn_name c))
    ~expr:
      [%expr
        fun t ->
          match t with
          | [%p pat] -> x
          | _ -> raise_s [%message [%e msg] (t : _ t)]]
;;

(* Declares
   [val map_variants : 'a t -> foo:('a Foo.t -> 'b Foo.t) -> bar:('a Bar.t -> 'b Bar.t) -> ... -> 'b t] *)
let map_variants_val_sig ~loc cases =
  let result_type =
    ptyp_constr ~loc (Located.mk ~loc (Lident "t")) [ ptyp_var ~loc "b" ]
  in
  let input_type =
    ptyp_constr ~loc (Located.mk ~loc (Lident "t")) [ ptyp_var ~loc "a" ]
  in
  let case_arg_type c =
    let inner_a = Case.payload_type ~loc c in
    let inner_b =
      ptyp_constr ~loc (Located.mk ~loc (Ldot (c.Case.mname, "t"))) [ ptyp_var ~loc "b" ]
    in
    ptyp_arrow ~loc Nolabel inner_a inner_b
  in
  let fn_type =
    List.fold_right cases ~init:result_type ~f:(fun c acc ->
      ptyp_arrow ~loc (Labelled (Case.label_name c)) (case_arg_type c) acc)
  in
  let full_type = ptyp_arrow ~loc Nolabel input_type fn_type in
  psig_value
    ~loc
    (value_description
       ~loc
       ~name:(Located.mk ~loc "map_variants")
       ~type_:full_type
       ~prim:[])
;;

(* Implements [map_variants] *)
let expand_map_variants ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let label = Case.label_name c in
      let pat = constr_pat ~loc c.Case.name (Some [%pat? x]) in
      let fn_call = pexp_apply ~loc (evar ~loc label) [ Nolabel, [%expr x] ] in
      let result = constr_exp ~loc c.Case.name (Some fn_call) in
      case ~lhs:pat ~guard:None ~rhs:result)
  in
  let match_expr = pexp_match ~loc [%expr t] match_cases in
  let body_with_labels =
    List.fold_right cases ~init:match_expr ~f:(fun c acc ->
      pexp_fun
        ~loc
        (Labelled (Case.label_name c))
        None
        (pvar ~loc (Case.label_name c))
        acc)
  in
  let full_expr = pexp_fun ~loc Nolabel None [%pat? t] body_with_labels in
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding ~loc ~pat:(pvar ~loc "map_variants") ~expr:full_expr ]
;;

(* Implements Kind module *)
let expand_kind_module ~loc cases =
  pstr_module
    ~loc
    (module_binding
       ~loc
       ~name:(Located.mk ~loc (Some "Kind"))
       ~expr:
         (pmod_structure
            ~loc
            [ pstr_type ~loc Recursive [ kind_variant_type_decl ~loc cases ] ]))
;;

(* Re-exports a module from a case, i.e. [ | Foo of 'a Foo.t ] becomes [module Foo = Foo]. *)
let expand_module_reexport ~loc c =
  pstr_module
    ~loc
    (module_binding
       ~loc
       ~name:(Located.mk ~loc (Some (Case.reexport_name c)))
       ~expr:(pmod_ident ~loc (Located.mk ~loc c.Case.mname)))
;;

(* Defines the [module type S] outputted by [Make] *)
let expand_module_type_s ~loc cases =
  let module_sigs = List.map cases ~f:(case_interface_module_sig ~loc) in
  let exn_sigs = List.map cases ~f:(case_exn_val_sig ~loc) in
  pstr_modtype
    ~loc
    (module_type_declaration
       ~loc
       ~name:(Located.mk ~loc "S")
       ~type_:
         (Some
            (pmty_signature
               ~loc
               ([ [%sigi: val kind : Kind.t]
                ; [%sigi: include Ppx_hardcaml_runtime.Interface.S with type 'a t = 'a t]
                ; [%sigi: val validate : 'a t -> unit]
                ]
                @ module_sigs
                @ exn_sigs
                @ [ map_variants_val_sig ~loc cases ]))))
;;

(* Generate the [validate] function which checks whether the passed in argument matches
   the kind specified in [Make] *)
let expand_validate ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let kpat = kind_pat ~loc c.Case.name in
      let tpat = constr_pat ~loc c.Case.name (Some [%pat? _]) in
      case ~lhs:(ppat_tuple ~loc [ kpat; tpat ]) ~guard:None ~rhs:[%expr ()])
  in
  let wildcard =
    case
      ~lhs:[%pat? _, _]
      ~guard:None
      ~rhs:[%expr raise_s [%message "mismatched tag" (kind : Kind.t) (t : _ t)]]
  in
  [%expr fun t -> [%e pexp_match ~loc [%expr kind, t] (match_cases @ [ wildcard ])]]
;;

(* map2: apply f to matching constructors *)
let expand_map2 ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let apat = constr_pat ~loc c.Case.name (Some [%pat? a]) in
      let bpat = constr_pat ~loc c.Case.name (Some [%pat? b]) in
      let map2id = pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "map2"))) in
      let call =
        pexp_apply
          ~loc
          map2id
          [ Labelled "f", [%expr f]; Nolabel, [%expr a]; Nolabel, [%expr b] ]
      in
      let result = constr_exp ~loc c.Case.name (Some call) in
      case ~lhs:(ppat_tuple ~loc [ apat; bpat ]) ~guard:None ~rhs:result)
  in
  let wildcard =
    case
      ~lhs:[%pat? _, _]
      ~guard:None
      ~rhs:[%expr raise_s [%message "map2 expects matching tags" (a : _ t) (b : _ t)]]
  in
  [%expr
    fun a b ~(f @ local) ->
      validate a;
      validate b;
      [%e pexp_match ~loc [%expr a, b] (match_cases @ [ wildcard ])]]
;;

(* iter2: apply f to matching constructors *)
let expand_iter2 ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let apat = constr_pat ~loc c.Case.name (Some [%pat? a]) in
      let bpat = constr_pat ~loc c.Case.name (Some [%pat? b]) in
      let iter2id = pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "iter2"))) in
      let call =
        pexp_apply
          ~loc
          iter2id
          [ Labelled "f", [%expr f]; Nolabel, [%expr a]; Nolabel, [%expr b] ]
      in
      case ~lhs:(ppat_tuple ~loc [ apat; bpat ]) ~guard:None ~rhs:call)
  in
  let wildcard =
    case
      ~lhs:[%pat? _, _]
      ~guard:None
      ~rhs:[%expr raise_s [%message "iter2 expects matching tags" (a : _ t) (b : _ t)]]
  in
  [%expr
    fun a b ~(f @ local) ->
      validate a;
      validate b;
      [%e pexp_match ~loc [%expr a, b] (match_cases @ [ wildcard ])]]
;;

(* map: apply f to a single value *)
let expand_map ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let pat = constr_pat ~loc c.Case.name (Some [%pat? x]) in
      let mapid = pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "map"))) in
      let call = pexp_apply ~loc mapid [ Labelled "f", [%expr f]; Nolabel, [%expr x] ] in
      let result = constr_exp ~loc c.Case.name (Some call) in
      case ~lhs:pat ~guard:None ~rhs:result)
  in
  [%expr
    fun a ~(f @ local) ->
      validate a;
      [%e pexp_match ~loc [%expr a] match_cases]]
;;

(* iter: apply f to a single value *)
let expand_iter ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let pat = constr_pat ~loc c.Case.name (Some [%pat? x]) in
      let iterid = pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "iter"))) in
      let call = pexp_apply ~loc iterid [ Labelled "f", [%expr f]; Nolabel, [%expr x] ] in
      case ~lhs:pat ~guard:None ~rhs:call)
  in
  [%expr
    fun a ~(f @ local) ->
      validate a;
      [%e pexp_match ~loc [%expr a] match_cases]]
;;

(* to_list: pass through to inner module *)
let expand_to_list ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let pat = constr_pat ~loc c.Case.name (Some [%pat? x]) in
      let to_list_id =
        pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "to_list")))
      in
      case ~lhs:pat ~guard:None ~rhs:(pexp_apply ~loc to_list_id [ Nolabel, [%expr x] ]))
  in
  [%expr
    fun t ->
      validate t;
      [%e pexp_match ~loc [%expr t] match_cases]]
;;

(* port_names_and_widths: pass through to inner module *)
let expand_port_names_and_widths ~loc cases =
  let match_cases =
    List.map cases ~f:(fun c ->
      let kpat = kind_pat ~loc c.Case.name in
      let pnw =
        pexp_ident ~loc (Located.mk ~loc (Ldot (c.Case.mname, "port_names_and_widths")))
      in
      let result = constr_exp ~loc c.Case.name (Some pnw) in
      case ~lhs:kpat ~guard:None ~rhs:result)
  in
  pexp_match ~loc [%expr kind] match_cases
;;

(* Make functor - always re-exports all inner modules since Make is a fresh structure *)
let expand_make_functor ~loc cases =
  let module_reexports = List.map cases ~f:(expand_module_reexport ~loc) in
  let t_module =
    pstr_module
      ~loc
      (module_binding
         ~loc
         ~name:(Located.mk ~loc (Some "T"))
         ~expr:
           (pmod_structure
              ~loc
              [ [%stri
                  type nonrec 'a t = 'a t
                  [@@deriving equal ~localize, compare ~localize, sexp_of]]
              ; pstr_value
                  ~loc
                  Nonrecursive
                  [ value_binding
                      ~loc
                      ~pat:(pvar ~loc "validate")
                      ~expr:(expand_validate ~loc cases)
                  ]
              ; pstr_value
                  ~loc
                  Nonrecursive
                  [ value_binding
                      ~loc
                      ~pat:(pvar ~loc "port_names_and_widths")
                      ~expr:(expand_port_names_and_widths ~loc cases)
                  ; value_binding
                      ~loc
                      ~pat:(pvar ~loc "map2")
                      ~expr:(expand_map2 ~loc cases)
                  ; value_binding
                      ~loc
                      ~pat:(pvar ~loc "iter2")
                      ~expr:(expand_iter2 ~loc cases)
                  ; value_binding
                      ~loc
                      ~pat:(pvar ~loc "map")
                      ~expr:(expand_map ~loc cases)
                  ; value_binding
                      ~loc
                      ~pat:(pvar ~loc "iter")
                      ~expr:(expand_iter ~loc cases)
                  ; value_binding
                      ~loc
                      ~pat:(pvar ~loc "to_list")
                      ~expr:(expand_to_list ~loc cases)
                  ]
              ; [%stri
                  include Ppx_hardcaml_runtime.Interface.Make (struct
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
                    end)]
              ]))
  in
  let exn_rebinds =
    List.map cases ~f:(fun c ->
      let name = Case.extractor_fn_name c in
      [%stri
        let [%p pvar ~loc name] = [%e pexp_ident ~loc (Located.mk ~loc (Lident name))]])
  in
  let map_variants_rebind = [%stri let map_variants = map_variants] in
  pstr_module
    ~loc
    (module_binding
       ~loc
       ~name:(Located.mk ~loc (Some "Make"))
       ~expr:
         (pmod_functor
            ~loc
            (Ppxlib_jane.Shim.Functor_parameter.to_parsetree
               (Named
                  ( Located.mk ~loc (Some "Config")
                  , pmty_signature ~loc [ [%sigi: val kind : Kind.t] ]
                  , [] )))
            (pmod_structure
               ~loc
               ([ [%stri include Config] ]
                @ module_reexports
                @ [ t_module; [%stri include T] ]
                @ exn_rebinds
                @ [ map_variants_rebind ]))))
;;

(* Generate structure items for a variant type *)
let str_of_type ~loc ~var cds =
  let cases = validate_constructors ~loc ~var cds in
  [ pstr_value ~loc Nonrecursive (List.map cases ~f:(expand_exn_fn ~loc))
  ; expand_map_variants ~loc cases
  ; expand_kind_module ~loc cases
  ; expand_module_type_s ~loc cases
  ; expand_make_functor ~loc cases
  ]
;;

let sig_kind_module ~loc cases =
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:(Located.mk ~loc (Some "Kind"))
       ~type_:
         (pmty_signature
            ~loc
            [ psig_type ~loc Recursive [ kind_variant_type_decl ~loc cases ] ]))
;;

let sig_module_type_s ~loc cases =
  let module_sigs = List.map cases ~f:(case_interface_module_sig ~loc) in
  let exn_sigs = List.map cases ~f:(case_exn_val_sig ~loc) in
  psig_modtype
    ~loc
    (module_type_declaration
       ~loc
       ~name:(Located.mk ~loc "S")
       ~type_:
         (Some
            (pmty_signature
               ~loc
               ([ [%sigi: val kind : Kind.t]
                ; [%sigi: include Ppx_hardcaml_runtime.Interface.S with type 'a t = 'a t]
                ; [%sigi: val validate : 'a t -> unit]
                ]
                @ module_sigs
                @ exn_sigs
                @ [ map_variants_val_sig ~loc cases ]))))
;;

let sig_make_functor ~loc =
  psig_module
    ~loc
    (module_declaration
       ~loc
       ~name:(Located.mk ~loc (Some "Make"))
       ~type_:
         (pmty_functor
            ~loc
            (Ppxlib_jane.Shim.Functor_parameter.to_parsetree
               (Named
                  ( Located.mk ~loc None
                  , pmty_signature ~loc [ [%sigi: val kind : Kind.t] ]
                  , [] )))
            (pmty_ident ~loc (Located.mk ~loc (Lident "S")))))
;;

(* Generate signature items for a variant type *)
let sig_of_type ~loc ~var cds =
  let cases = validate_constructors ~loc ~var cds in
  List.map cases ~f:(case_exn_val_sig ~loc)
  @ [ map_variants_val_sig ~loc cases
    ; sig_kind_module ~loc cases
    ; sig_module_type_s ~loc cases
    ; sig_make_functor ~loc
    ]
;;

let str_of_type_decl type_decl =
  let only_param =
    match type_decl.ptype_params with
    | [ (param, _) ] -> Some param.ptyp_desc
    | _ -> None
  in
  match
    ( type_decl.ptype_kind
    , Option.map only_param ~f:Ppxlib_jane.Shim.Core_type_desc.of_parsetree )
  with
  | Ptype_variant constructors, Some (Ptyp_var (var, _)) ->
    str_of_type ~loc:type_decl.ptype_loc ~var constructors
  | _ -> raise_errorf ~loc:type_decl.ptype_loc "[%s] only supports variant types" deriver
;;

let sig_of_type_decl type_decl =
  match
    ( type_decl.ptype_kind
    , Option.map (List.hd type_decl.ptype_params) ~f:(fun (p, _) ->
        Ppxlib_jane.Shim.Core_type_desc.of_parsetree p.ptyp_desc) )
  with
  | Ptype_variant cds, Some (Ptyp_var (var, _)) ->
    sig_of_type ~loc:type_decl.ptype_loc ~var cds
  | _ -> raise_errorf ~loc:type_decl.ptype_loc "[%s] only supports variant types" deriver
;;

let register () =
  let hardcaml_variants_internal =
    Deriving.add
      "hardcaml_variants_internal"
      ~str_type_decl:
        (Deriving.Generator.make
           Deriving.Args.empty
           (fun ~loc:_ ~path:_ (_, type_declarations) ->
              List.concat_map type_declarations ~f:str_of_type_decl))
      ~sig_type_decl:
        (Deriving.Generator.make
           Deriving.Args.empty
           (fun ~loc:_ ~path:_ (_, type_declarations) ->
              List.concat_map type_declarations ~f:sig_of_type_decl))
  in
  (* Ordering of the derivers of the alias below matters. Empirically, the derivers are
     expanded in reverse order of the list. *)
  Deriving.add_alias
    deriver
    [ hardcaml_variants_internal
    ; Ppx_sexp_conv.sexp_of
    ; Ppx_compare.equal_local
    ; Ppx_compare.compare_local
    ]
  |> Deriving.ignore
;;
