open! Core0

module Group = struct
  type t =
    { uid : int
    ; name : string [@compare.ignore] [@equal.ignore] [@hash.ignore]
    }
  [@@deriving compare, equal, hash, fields ~getters]

  (* The [uid] is an implementation detail; identify a group by its name when displaying. *)
  let sexp_of_t t = sexp_of_string t.name
  let counter = ref 0

  let create name =
    let uid = !counter in
    Int.incr counter;
    { uid; name }
  ;;

  (* A group which, when enabled, matches every property regardless of its own group. *)
  let all = create "all"
  let default = create "default"
  let contract = create "contract"
end

module Trace_properties = struct
  type t =
    { enable_sva : bool
    ; enable_ltl : bool
    ; enable_assertions : bool
    ; enabled_property_groups : Group.t list
    }
  [@@deriving sexp_of]

  let none =
    { enable_sva = false
    ; enable_ltl = false
    ; enable_assertions = false
    ; enabled_property_groups = []
    }
  ;;

  (* Trace every property that has been defined, regardless of its group. This is achieved
     by enabling the special [Group.all] group (see [group_is_enabled]). *)
  let all =
    { enable_sva = true
    ; enable_ltl = true
    ; enable_assertions = true
    ; enabled_property_groups = [ Group.all ]
    }
  ;;

  (* Trace all property types within the default group. *)
  let default =
    { enable_sva = true
    ; enable_ltl = true
    ; enable_assertions = true
    ; enabled_property_groups = [ Group.default ]
    }
  ;;

  let create
    ?(enable_sva = false)
    ?(enable_ltl = false)
    ?(enable_assertions = false)
    ?(groups = [ Group.default ])
    ()
    =
    { enable_sva; enable_ltl; enable_assertions; enabled_property_groups = groups }
  ;;

  let any_enabled t = t.enable_sva || t.enable_ltl || t.enable_assertions
  let for_simulation t = t.enable_assertions || t.enable_ltl

  let group_is_enabled t group =
    (* [Group.all] in the enabled set matches every property, regardless of the group it
       was tagged with. *)
    List.mem t.enabled_property_groups Group.all ~equal:Group.equal
    || List.mem t.enabled_property_groups group ~equal:Group.equal
  ;;
end

type 'a entry =
  { group : Group.t
  ; data : 'a
  }
[@@deriving sexp_of]

type t =
  { (* simple assertions - used with cyclesim *)
    assertions : (string, Signal.t entry) Hashtbl.t
  ; (* ltl properties - used with cyclesim *)
    ltl : (string, Property.LTL.t entry) Hashtbl.t
  ; (* sva properties - used in rtl generation *)
    sva : (string, Signal.t Property.Sva.t entry) Hashtbl.t
  }
[@@deriving sexp_of]

let create () =
  { assertions = Hashtbl.create (module String)
  ; ltl = Hashtbl.create (module String)
  ; sva = Hashtbl.create (module String)
  }
;;

(* Return a new manager containing only the properties whose type is enabled and whose
   group is in the set of enabled groups. *)
let filter t (trace_properties : Trace_properties.t) =
  let filter_table enable table =
    let out = Hashtbl.create (module String) in
    if enable
    then
      Hashtbl.iteri table ~f:(fun ~key ~data:entry ->
        if Trace_properties.group_is_enabled trace_properties entry.group
        then Hashtbl.set out ~key ~data:entry);
    out
  in
  { assertions = filter_table trace_properties.enable_assertions t.assertions
  ; ltl = filter_table trace_properties.enable_ltl t.ltl
  ; sva = filter_table trace_properties.enable_sva t.sva
  }
;;

(* assertions *)

let add_assertion ?(group = Group.default) t name assertion =
  Hashtbl.set t.assertions ~key:name ~data:{ group; data = assertion }
;;

let get_assertions t = Hashtbl.map t.assertions ~f:(fun entry -> entry.data)

(* ltl properties *)

let add_ltl ?(group = Group.default) t name property =
  Hashtbl.set t.ltl ~key:name ~data:{ group; data = property }
;;

let get_ltl_properties t = Hashtbl.map t.ltl ~f:(fun entry -> entry.data)

let get_ltl_atomic_propositions t =
  Hashtbl.fold
    t.ltl
    ~init:(Set.empty (module Signal.Type.Signal_compared_by_uid))
    ~f:(fun ~key:_ ~data:{ data = ltl; group = _ } set ->
      List.fold
        ~init:set
        (Property.LTL.Formula.atomic_propositions ltl.formula)
        ~f:(fun set signal -> Set.add set signal))
;;

(* sva properties *)

let add_sva ?(group = Group.default) t name property =
  (* Some hackery - RTL names must not contains spaces or backslashes - we usually raise,
     but make an exception for property names *)
  let name =
    String.map name ~f:(function
      | ' ' | '\\' -> '_'
      | c -> c)
  in
  if Hashtbl.mem t.sva name
  then
    raise_s
      [%message
        "Properties names are not properly mangled - please make them unique"
          (name : string)];
  Hashtbl.set t.sva ~key:name ~data:{ group; data = property }
;;

let get_sva_properties t = Hashtbl.map t.sva ~f:(fun entry -> entry.data)

let get_sva_signals_impl ~include_clocked t =
  Hashtbl.fold
    t.sva
    ~init:(Set.empty (module Signal.Type.Signal_compared_by_uid))
    ~f:(fun ~key:_ ~data:{ data = sva; group = _ } set ->
      [ Property.Sva.atomic_propositions sva
      ; (match sva.clocked with
         | Some clocked when include_clocked ->
           List.concat [ [ clocked.clock ]; Option.to_list clocked.disable_iff ]
         | None | Some _ -> [])
      ]
      |> List.concat
      |> List.fold ~init:set ~f:(fun set signal -> Set.add set signal))
;;

let get_sva_atomic_propositions t = get_sva_signals_impl t ~include_clocked:false
let get_sva_signals t = get_sva_signals_impl t ~include_clocked:true

let map_sva_properties t ~f =
  { t with sva = Hashtbl.map t.sva ~f:(fun entry -> { entry with data = f entry.data }) }
;;
