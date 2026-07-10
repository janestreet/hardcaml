open! Core0

module Exact = struct
  module Uid = Uid_builder.Make ()

  let `New gen_uid, `Reset reset_uid = Uid.generator ()

  type t =
    { uid : Uid.t
    ; name : string
    ; loc : Source_code_position.t
    }
  [@@deriving sexp_of, fields ~getters]

  let get_name ({ name; _ } : t) = name

  let%template equal { uid = uid1; _ } { uid = uid2; _ } = (Uid.equal [@mode m]) uid1 uid2
  [@@mode m = (local, global)]
  ;;

  let create_domain ~(loc : [%call_pos]) name = { uid = gen_uid (); name; loc }

  module Expert = struct
    let reset_uid = reset_uid
    let create_domain = create_domain
  end
end

module Runtime = struct
  type t =
    | Constant
    | Unknown
    | Exact of Exact.t
  [@@deriving sexp_of, equal ~localize]

  let get_name = function
    | Constant -> "constant"
    | Unknown -> "unknown"
    | Exact t -> Exact.get_name t
  ;;

  let exact_exn = function
    | Exact e -> e
    | Constant | Unknown -> failwith "[exact_exn] can only be called on Exact"
  ;;
end

type unique_runtime_domains =
  { num_unknowns : int
  ; exact_domains : Exact.t list
  }

let get_unique_runtime_domains (ts : Runtime.t list) =
  let tbl = Hashtbl.create (module Exact.Uid) in
  let num_unknowns = ref 0 in
  List.iter ts ~f:(fun t ->
    match t with
    | Constant -> ()
    | Unknown -> Int.incr num_unknowns
    | Exact exact -> Hashtbl.set tbl ~key:(Exact.uid exact) ~data:exact);
  { num_unknowns = !num_unknowns; exact_domains = Hashtbl.data tbl }
;;

module Uid = Uid_builder.Make ()

let `New gen_uid, `Reset _ = Uid.generator ()

type t =
  { uid : Uid.t
  ; name : string
  ; loc : Source_code_position.t
  }
[@@deriving sexp_of, fields ~getters]

let equal { uid = uid1; _ } { uid = uid2; _ } = Uid.equal uid1 uid2
let create_with_uid ~(loc : [%call_pos]) name uid = { uid; name; loc }
let create ~(loc : [%call_pos]) name = create_with_uid ~loc name (gen_uid ())
let get_name ({ name; _ } : t) = name

let generate_fresh_exact_domain ~(loc : [%call_pos]) (t : _) =
  Exact.create_domain ~loc (t.name ^ "_gen")
;;

let dedup_by_uid (ts : t list) =
  let tbl = Hashtbl.create (module Uid) in
  List.iter ts ~f:(fun t -> Hashtbl.set tbl ~key:(uid t) ~data:t);
  Hashtbl.data tbl
;;

type construct_mapping_error =
  | Maps_to_unknown
  | Maps_to_multiple of Exact.t list
[@@deriving sexp_of]

type maps_to =
  | Exact of Exact.t
  | All_constants
[@@deriving sexp_of]

type mapped_clock_domain =
  { specification_domain : t
  ; maps_to : (maps_to, construct_mapping_error) Result.t
  }
[@@deriving sexp_of]

let construct_mapping_from_spec_to_exact (alist : (t * Runtime.t) list) =
  let spec_to_runtimes = Hashtbl.create (module Uid) in
  let spec_uid_to_spec = Hashtbl.create (module Uid) in
  List.iter alist ~f:(fun (spec, exact) ->
    Hashtbl.set spec_uid_to_spec ~key:(uid spec) ~data:spec;
    Hashtbl.add_multi spec_to_runtimes ~key:(uid spec) ~data:exact);
  spec_to_runtimes
  |> Hashtbl.to_alist
  |> List.filter_map ~f:(fun (spec_uid, exact) ->
    let { num_unknowns; exact_domains } = get_unique_runtime_domains exact in
    let specification_domain = Hashtbl.find_exn spec_uid_to_spec spec_uid in
    let%map.Option maps_to =
      match num_unknowns with
      | 0 ->
        (match exact_domains with
         | [] ->
           (* In this branch, all the runtime domains associated to this particular clock
              domain spec happens to be a constant.
           *)
           Some (Ok All_constants)
         | [ hd ] -> Some (Ok (Exact hd))
         | exact_domains -> Some (Error (Maps_to_multiple exact_domains)))
      | _ -> Some (Error Maps_to_unknown)
    in
    spec_uid, { specification_domain; maps_to })
  |> Uid.Map.of_alist_exn
;;

module Expert = struct
  let create_with_uid = create_with_uid
end
