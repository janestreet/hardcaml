open! Core0

module Bits = struct
  include Bits

  let hash_fold_t state t = Bits.to_string t |> String.hash_fold_t state
end

module Structure_kind = struct
  (* Two signals are equal if they have same structure kind and their children are equal. *)
  type t =
    | Empty
    | Const of Bits.t
    | Op of Signal.Type.Op.t
    | Mux
    | Cases
    | Cat
    | Not
    | Wire of string list
    | Select of int * int
    | Mem_read_port
    | Dont_dedup of Signal.Type.Uid.t
  [@@deriving sexp_of, compare ~localize, hash]

  let equal a b = compare a b = 0
end

let structure_kind (signal : Signal.t) =
  match signal with
  | Signal.Type.Empty -> Structure_kind.Empty
  | Const { constant; _ } -> Structure_kind.Const constant
  | Op2 { op; _ } -> Structure_kind.Op op
  | Mux _ -> Structure_kind.Mux
  | Cases _ -> Structure_kind.Cases
  | Cat _ -> Structure_kind.Cat
  | Not _ -> Structure_kind.Not
  | Wire _ -> Structure_kind.Wire (Signal.names signal)
  | Select { high; low; _ } -> Structure_kind.Select (high, low)
  | Reg _ -> Structure_kind.Dont_dedup (Signal.uid signal)
  | Multiport_mem _ -> Structure_kind.Dont_dedup (Signal.uid signal)
  | Mem_read_port _ -> Structure_kind.Mem_read_port
  | Inst _ -> Structure_kind.Dont_dedup (Signal.uid signal)
;;

module Children = Signal.Type.Make_deps (struct
    let fold t ~init ~f =
      match structure_kind t with
      | Structure_kind.Dont_dedup _ -> init
      | _ -> Signal.Type.Deps.fold t ~init ~f
    ;;
  end)

let give_signal_fresh_id signal ~new_id =
  Signal.Type.map_info signal ~f:(fun info -> { info with uid = new_id () })
;;

let map_children_and_give_fresh_id signal ~f ~new_id =
  Signal.Type.map_dependant signal ~f |> give_signal_fresh_id ~new_id
;;

let find_by_signal_uid memo s = Hashtbl.find_exn memo (Signal.uid s)

let signal_hash memo signal =
  Hashtbl.find_or_add memo (Signal.uid signal) ~default:(fun () ->
    let children_hashes = Children.map signal ~f:(find_by_signal_uid memo) in
    [%hash: Structure_kind.t * int list] (structure_kind signal, children_hashes))
;;

let create_wire_with_fresh_id ~width ~new_id =
  Signal.wire width |> give_signal_fresh_id ~new_id
;;

let make_canonical_signal canonical sequential_wires signal ~new_id =
  match structure_kind signal with
  | Structure_kind.Dont_dedup _ ->
    let wire = create_wire_with_fresh_id ~width:(Signal.width signal) ~new_id in
    Hashtbl.add_exn sequential_wires ~key:(Signal.uid signal) ~data:(signal, wire);
    wire
  | Structure_kind.Mem_read_port ->
    let transformed =
      map_children_and_give_fresh_id
        signal
        ~f:(fun child -> Hashtbl.find_exn canonical (Signal.uid child))
        ~new_id
    in
    (* wrap all Mem_read_ports in wires, so we can later replace them in next stage *)
    let wire = create_wire_with_fresh_id ~width:(Signal.width transformed) ~new_id in
    Signal.( <-- ) wire transformed;
    wire
  | _ ->
    map_children_and_give_fresh_id
      signal
      ~f:(fun child -> Hashtbl.find_exn canonical (Signal.uid child))
      ~new_id
;;

let is_anonymous signal = Signal.is_empty signal || List.is_empty (Signal.names signal)

(* Note that named signals should never be equivalent. It is not sufficient to check for
   Signal naming equality. If two signals are named the same thing, the simulation trace
   should render them as two different signals, ie: [XYZ] and [XYZ_0].
*)
let rec shallow_equal a b =
  is_anonymous a
  && is_anonymous b
  && Structure_kind.equal (structure_kind a) (structure_kind b)
  &&
  match a, b with
  | Wire { driver = Some r_a; _ }, Wire { driver = Some r_b; _ } ->
    (* special case wires, due to special treatment of Mem_read_port... *)
    shallow_equal r_a r_b
  | _ ->
    [%equal: Signal.Type.Uid.t list]
      (Children.rev_map a ~f:Signal.uid)
      (Children.rev_map b ~f:Signal.uid)
;;

let transform_sequential_signal canonical signal ~new_id =
  let get_canonical signal = Hashtbl.find_exn canonical (Signal.uid signal) in
  let rewrite_instantiation (instantiation : _ Signal.Type.Inst.Instantiation.t) =
    let inputs =
      List.map instantiation.inputs ~f:(fun { name; input_signal } ->
        { Signal.Type.Inst.Input.name; input_signal = get_canonical input_signal })
    in
    { instantiation with inputs }
  in
  let rewrite_register = Signal.Type.Register.map ~f:get_canonical in
  let rewrite_signal_info (info : Signal.Type.Info.t) = { info with uid = new_id () } in
  let rewrite_write_port = Write_port.map ~f:get_canonical in
  match signal with
  | Signal.Type.Reg { info; register; d } ->
    Signal.Type.Reg
      { info = rewrite_signal_info info
      ; register = rewrite_register register
      ; d = get_canonical d
      }
  | Multiport_mem { info; size; write_ports; initialize_to } ->
    Multiport_mem
      { info = rewrite_signal_info info
      ; size
      ; write_ports = Array.map write_ports ~f:rewrite_write_port
      ; initialize_to
      }
  | Inst { info; instantiation } ->
    Inst
      { info = rewrite_signal_info info
      ; instantiation = rewrite_instantiation instantiation
      }
  | _ ->
    let sexp_of_finite_signal signal =
      Signal.Type.sexp_of_signal_recursive ~depth:5 (signal : Signal.t)
    in
    raise_s
      [%message
        "Unexpected signal type. Only [Dont_dedup]-type signals are expected here"
          (signal : finite_signal)]
;;

let rec unwrap_wire s =
  match s with
  | None -> None
  | Some (Signal.Type.Wire { driver; _ } as s) when List.is_empty (Signal.names s) ->
    unwrap_wire driver
  | _ -> s
;;

let fix_mem_read_ports signals =
  (* We wrap all sequential signals in wires. However, Mem_read_port expects to contain
     Multiport_mem and not Multiport_mem wrapped in wire. Rewrite the wire out. *)
  List.iter signals ~f:(function
    | Signal.Type.Wire ({ driver; _ } as w) ->
      (* every Mem_read_port signal is pointed to by a single wire *)
      (match driver with
       | Some
           (Signal.Type.Mem_read_port
             { info; memory = Wire { driver = mem_ref; _ }; read_address }) ->
         let memory =
           match unwrap_wire mem_ref with
           | Some (Signal.Type.Multiport_mem _ as unwrapped) -> unwrapped
           | Some _ | None -> assert false
         in
         w.driver <- Some (Mem_read_port { info; memory; read_address })
       | Some _ | None -> ())
    | _ -> ())
;;

let compress_wires signals =
  Signal_graph.create signals
  |> Signal_graph.iter ~f:(function
    | Signal.Type.Wire ({ driver; _ } as w) -> w.driver <- unwrap_wire driver
    | _ -> ())
;;

let canonicalize signals =
  let hash_memo = Hashtbl.create (module Signal.Type.Uid) in
  let canonical = Hashtbl.create (module Signal.Type.Uid) in
  let canonical_by_hash = Hashtbl.create (module Int) in
  (* we replace all sequential elements with wires, to be able to transform them later *)
  let sequential_wires = Hashtbl.create (module Signal.Type.Uid) in
  let `New new_id, `Reset _ = Signal.Type.Uid.generator () in
  List.iter
    (Signal_graph.topological_sort_exn
       ~deps:(module Children)
       (Signal_graph.create signals))
    ~f:(fun signal ->
      let my_hash = signal_hash hash_memo signal in
      let signal_with_canonical_children =
        make_canonical_signal canonical sequential_wires signal ~new_id
      in
      let canonical_signal =
        match structure_kind signal with
        | Dont_dedup _ -> signal_with_canonical_children
        | _ ->
          let cannonical_signals_with_matching_hash =
            Hashtbl.find_multi canonical_by_hash my_hash
          in
          (match
             List.find
               cannonical_signals_with_matching_hash
               ~f:(shallow_equal signal_with_canonical_children)
           with
           | None ->
             Hashtbl.add_multi
               canonical_by_hash
               ~key:my_hash
               ~data:signal_with_canonical_children;
             signal_with_canonical_children
           | Some canonical_signal -> canonical_signal)
      in
      Hashtbl.add_exn canonical ~key:(Signal.uid signal) ~data:canonical_signal);
  Hashtbl.iter sequential_wires ~f:(fun (signal, wire) ->
    Signal.( <-- ) wire (transform_sequential_signal canonical signal ~new_id));
  fix_mem_read_ports (Hashtbl.data canonical);
  compress_wires (Hashtbl.data canonical);
  canonical
;;

let deduplicate circuit =
  let all_original_signals =
    Circuit.signal_graph circuit |> Signal_graph.filter ~f:(Fn.const true)
  in
  let canonical =
    canonicalize (all_original_signals @ Map.data (Circuit.assertions circuit))
  in
  let outputs = Circuit.outputs circuit in
  let assertions =
    Map.map (Circuit.assertions circuit) ~f:(fun signal ->
      Hashtbl.find_exn canonical (Signal.uid signal))
    |> Assertion_manager.of_signals
  in
  Circuit.create_exn
    ~config:{ Circuit.Config.default with assertions = Some assertions }
    ~name:(Circuit.name circuit)
    (List.map outputs ~f:(fun signal -> Hashtbl.find_exn canonical (Signal.uid signal)))
;;

module For_testing = struct
  let signal_hash = signal_hash
end
