open! Base

module Bits = struct
  include Bits

  let hash_fold_t state t = Bits.to_string t |> String.hash_fold_t state
end

module Structure_kind = struct
  (* Two signals are equal if they have same structure kind and their children are equal. *)
  type t =
    | Empty
    | Const of Bits.t
    | Op of Signal.Type.signal_op
    | Mux
    | Cat
    | Not
    | Wire of string list
    | Select of int * int
    | Mem_read_port
    | Dont_dedup of Signal.Uid.t
  [@@deriving sexp_of, compare, hash]

  let equal a b = compare a b = 0
end

let structure_kind (signal : Signal.t) =
  match signal with
  | Signal.Type.Empty -> Structure_kind.Empty
  | Const { constant; _ } -> Structure_kind.Const constant
  | Op2 { op; _ } -> Structure_kind.Op op
  | Mux _ -> Structure_kind.Mux
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

let map_children signal ~f =
  match signal with
  | Signal.Type.Empty -> signal
  | Const _ -> signal
  | Op2 { signal_id; op; arg_a; arg_b } ->
    let arg_a = f arg_a in
    let arg_b = f arg_b in
    Op2 { signal_id = { signal_id with s_id = Signal.Type.new_id () }; op; arg_a; arg_b }
  | Mux { signal_id; select; cases } ->
    let select = f select in
    let cases = List.map cases ~f in
    Mux { signal_id = { signal_id with s_id = Signal.Type.new_id () }; select; cases }
  | Cat { signal_id; args } ->
    let args = List.map args ~f in
    Cat { signal_id = { signal_id with s_id = Signal.Type.new_id () }; args }
  | Not { signal_id; arg } ->
    let arg = f arg in
    Not { signal_id = { signal_id with s_id = Signal.Type.new_id () }; arg }
  | Wire { signal_id; driver } ->
    Wire
      { signal_id = { signal_id with s_id = Signal.Type.new_id () }
      ; driver = ref (f !driver)
      }
  | Select { signal_id; arg; high; low } ->
    let arg = f arg in
    Select { signal_id = { signal_id with s_id = Signal.Type.new_id () }; arg; high; low }
  | Multiport_mem _ | Reg _ -> assert false
  | Mem_read_port { signal_id; memory; read_address } ->
    Mem_read_port
      { signal_id = { signal_id with s_id = Signal.Type.new_id () }
      ; memory = f memory
      ; read_address = f read_address
      }
  | Inst _ -> signal
;;

let find_by_signal_uid memo s = Hashtbl.find_exn memo (Signal.uid s)

let signal_hash memo signal =
  Hashtbl.find_or_add memo (Signal.uid signal) ~default:(fun () ->
    let children_hashes = Children.map signal ~f:(find_by_signal_uid memo) in
    [%hash: Structure_kind.t * int list] (structure_kind signal, children_hashes))
;;

let make_canonical_signal canonical sequential_wires signal =
  match structure_kind signal with
  | Structure_kind.Dont_dedup _ ->
    let wire = Signal.wire (Signal.width signal) in
    Hashtbl.add_exn sequential_wires ~key:(Signal.uid signal) ~data:(signal, wire);
    wire
  | Structure_kind.Mem_read_port ->
    let transformed =
      map_children signal ~f:(fun child -> Hashtbl.find_exn canonical (Signal.uid child))
    in
    (* wrap all Mem_read_ports in wires, so we can later replace them in next stage *)
    Signal.wireof transformed
  | _ ->
    map_children signal ~f:(fun child -> Hashtbl.find_exn canonical (Signal.uid child))
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
  | Wire { driver = r_a; _ }, Wire { driver = r_b; _ } ->
    (* special case wires, due to special treatment of Mem_read_port... *)
    shallow_equal !r_a !r_b
  | _ ->
    [%equal: Signal.Uid.t list]
      (Children.rev_map a ~f:Signal.uid)
      (Children.rev_map b ~f:Signal.uid)
;;

let transform_sequential_signal canonical signal =
  let get_canonical signal = Hashtbl.find_exn canonical (Signal.uid signal) in
  let rewrite_instantiation (instantiation : Signal.Type.instantiation) =
    let inst_inputs =
      List.map instantiation.inst_inputs ~f:(fun (name, s) -> name, get_canonical s)
    in
    { instantiation with inst_inputs }
  in
  let rewrite_register register =
    let { Reg_spec.reg_clock
        ; reg_clock_edge
        ; reg_reset
        ; reg_reset_edge
        ; reg_reset_value
        ; reg_clear
        ; reg_clear_level
        ; reg_clear_value
        ; reg_enable
        }
      =
      register
    in
    { Reg_spec.reg_clock = get_canonical reg_clock
    ; reg_clock_edge
    ; reg_reset = get_canonical reg_reset
    ; reg_reset_edge
    ; reg_reset_value = get_canonical reg_reset_value
    ; reg_clear = get_canonical reg_clear
    ; reg_clear_level
    ; reg_clear_value = get_canonical reg_clear_value
    ; reg_enable = get_canonical reg_enable
    }
  in
  let rewrite_signal_id (signal_id : Signal.Type.signal_id) =
    { signal_id with s_id = Signal.Type.new_id () }
  in
  let rewrite_write_port
    { Write_port.write_clock; write_address; write_enable; write_data }
    =
    { Write_port.write_clock = get_canonical write_clock
    ; write_address = get_canonical write_address
    ; write_enable = get_canonical write_enable
    ; write_data = get_canonical write_data
    }
  in
  match signal with
  | Signal.Type.Reg { signal_id; register; d } ->
    Signal.Type.Reg
      { signal_id = rewrite_signal_id signal_id
      ; register = rewrite_register register
      ; d = get_canonical d
      }
  | Multiport_mem { signal_id; size; write_ports } ->
    Multiport_mem
      { signal_id = rewrite_signal_id signal_id
      ; size
      ; write_ports = Array.map write_ports ~f:rewrite_write_port
      }
  | Inst { signal_id; extra_uid; instantiation } ->
    Inst
      { signal_id = rewrite_signal_id signal_id
      ; extra_uid
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
  | Signal.Type.Wire { driver; _ } when List.is_empty (Signal.names s) ->
    unwrap_wire !driver
  | _ -> s
;;

let fix_mem_read_ports signals =
  (* We wrap all sequential signals in wires. However, Mem_read_port expects to contain
     Multiport_mem and not Multiport_mem wrapped in wire. Rewrite the wire out. *)
  List.iter signals ~f:(function
    | Signal.Type.Wire { driver; _ } ->
      (* every Mem_read_port signal is pointed to by a single wire *)
      (match !driver with
       | Signal.Type.Mem_read_port
           { signal_id; memory = Wire { driver = mem_ref; _ }; read_address } ->
         let memory =
           match unwrap_wire !mem_ref with
           | Signal.Type.Multiport_mem _ as unwrapped -> unwrapped
           | _ -> assert false
         in
         driver := Mem_read_port { signal_id; memory; read_address }
       | _ -> ())
    | _ -> ())
;;

let compress_wires signals =
  Signal_graph.create signals
  |> Signal_graph.iter ~f:(function
       | Signal.Type.Wire { driver; _ } -> driver := unwrap_wire !driver
       | _ -> ())
;;

let canonicalize signals =
  let hash_memo = Hashtbl.create (module Signal.Uid) in
  let canonical = Hashtbl.create (module Signal.Uid) in
  let canonical_by_hash = Hashtbl.create (module Int) in
  (* we replace all sequential elements with wires, to be able to transform them later *)
  let sequential_wires = Hashtbl.create (module Signal.Uid) in
  List.iter
    (Signal_graph.topological_sort_exn
       ~deps:(module Children)
       (Signal_graph.create signals))
    ~f:(fun signal ->
      let my_hash = signal_hash hash_memo signal in
      let signal_with_canonical_children =
        make_canonical_signal canonical sequential_wires signal
      in
      let canonical_signal =
        let cannonical_signals_with_matching_hash =
          Hashtbl.find_multi canonical_by_hash my_hash
        in
        match
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
        | Some canonical_signal -> canonical_signal
      in
      Hashtbl.add_exn canonical ~key:(Signal.uid signal) ~data:canonical_signal);
  Hashtbl.iter sequential_wires ~f:(fun (signal, wire) ->
    Signal.( <== ) wire (transform_sequential_signal canonical signal));
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
