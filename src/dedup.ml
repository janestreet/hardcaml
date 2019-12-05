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
    | Op of Signal.signal_op
    | Wire of string list
    | Select of int * int
    | Mem_read_port
    | Sequential of Signal.Uid.t
  [@@deriving sexp_of, compare, hash]

  let equal a b = compare a b = 0
end

let structure_kind (signal : Signal.t) =
  match signal with
  | Signal.Empty -> Structure_kind.Empty
  | Signal.Const (_, c) -> Structure_kind.Const c
  | Signal.Op (_, op) -> Structure_kind.Op op
  | Signal.Wire (_, _) -> Structure_kind.Wire (Signal.names signal)
  | Signal.Select (_, a, b) -> Structure_kind.Select (a, b)
  | Signal.Reg (_, _) -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Mem (_, _, _, _) -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Multiport_mem (_, _, _) -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Mem_read_port (_, _, _) -> Structure_kind.Mem_read_port
  | Signal.Inst (_, _, _) -> Structure_kind.Sequential (Signal.uid signal)
;;

let children (signal : Signal.t) =
  match structure_kind signal with
  | Structure_kind.Sequential _ -> []
  | _ ->
    (match signal with
     | Wire (_, r) -> [ !r ]
     | Mem_read_port (_, mem, r) -> [ mem; r ]
     | _ -> Signal.deps signal)
;;

let signal_id_map_children s ~f =
  { s with Signal.s_id = Signal.new_id (); s_deps = List.map s.Signal.s_deps ~f }
;;

let map_children signal ~f =
  match signal with
  | Signal.Empty -> signal
  | Signal.Const (_, _) -> signal
  | Signal.Op (s, op) -> Signal.Op (signal_id_map_children s ~f, op)
  | Signal.Wire (s, c) -> Signal.Wire (signal_id_map_children s ~f, ref (f !c))
  | Signal.Select (s, a, b) -> Signal.Select (signal_id_map_children s ~f, a, b)
  | Signal.Multiport_mem _ | Signal.Reg _ -> assert false
  | Signal.Mem_read_port (s, mem, r) ->
    Signal.Mem_read_port
      ( { s with s_id = Signal.new_id (); s_deps = List.map s.Signal.s_deps ~f }
      , f mem
      , f r )
  | Signal.Mem (_, _, _, _) -> failwith "Mem is unsupported"
  | Signal.Inst (_, _, _) -> signal
;;

let find_by_signal_uid memo s = Hashtbl.find_exn memo (Signal.uid s)

let signal_hash memo signal =
  Hashtbl.find_or_add memo (Signal.uid signal) ~default:(fun () ->
    let children_hashes = List.map (children signal) ~f:(find_by_signal_uid memo) in
    [%hash: Structure_kind.t * int list] (structure_kind signal, children_hashes))
;;

let make_canonical_signal canonical sequential_wires signal =
  match structure_kind signal with
  | Structure_kind.Sequential _ ->
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

let rec shallow_equal a b =
  Structure_kind.equal (structure_kind a) (structure_kind b)
  &&
  match a, b with
  | Wire (_, r_a), Wire (_, r_b) ->
    (* special case wires, due to special treatment of Mem_read_port... *)
    shallow_equal !r_a !r_b
  | _ ->
    [%equal: Signal.Uid.t list]
      (children a |> List.map ~f:Signal.uid)
      (children b |> List.map ~f:Signal.uid)
;;

let transform_sequential_signal canonical signal =
  let get_canonical signal = Hashtbl.find_exn canonical (Signal.uid signal) in
  match signal with
  | Signal.Reg (s, reg_info) ->
    let { Signal.reg_clock
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
      reg_info
    in
    Signal.Reg
      ( { s with
          s_id = Signal.new_id ()
        ; s_deps = List.map s.Signal.s_deps ~f:get_canonical
        }
      , { Signal.reg_clock = get_canonical reg_clock
        ; reg_clock_edge
        ; reg_reset = get_canonical reg_reset
        ; reg_reset_edge
        ; reg_reset_value = get_canonical reg_reset_value
        ; reg_clear = get_canonical reg_clear
        ; reg_clear_level
        ; reg_clear_value = get_canonical reg_clear_value
        ; reg_enable = get_canonical reg_enable
        } )
  | Signal.Multiport_mem (s, size, write_ports) ->
    let write_ports =
      Array.map
        write_ports
        ~f:(fun { Signal.write_clock : Signal.t
                ; write_address : Signal.t
                ; write_enable : Signal.t
                ; write_data : Signal.t
                }
             ->
               { Signal.write_clock = get_canonical write_clock
               ; write_address = get_canonical write_address
               ; write_enable = get_canonical write_enable
               ; write_data = get_canonical write_data
               })
    in
    Signal.Multiport_mem
      ({ s with s_deps = List.map s.Signal.s_deps ~f:get_canonical }, size, write_ports)
  | _ ->
    let sexp_of_finite_signal signal =
      Signal.sexp_of_signal_recursive ~depth:5 (signal : Signal.t)
    in
    raise_s
      [%message
        "Unexpected signal type. Only sequential signals are expected here"
          (signal : finite_signal)]
;;

let rec unwrap_wire s =
  match s with
  | Signal.Wire (_, r) when List.is_empty (Signal.names s) -> unwrap_wire !r
  | _ -> s
;;

let fix_mem_read_ports signals =
  (* We wrap all sequential signals in wires. However, Mem_read_port expects to contain
     Multiport_mem and not Multiport_mem wrapped in wire. Rewrite the wire out. *)
  List.iter signals ~f:(function
    | Signal.Wire (_, r) ->
      (* every Mem_read_port signal is pointed to by a single wire *)
      (match !r with
       | Signal.Mem_read_port (s, Wire (_, mem_ref), addr) ->
         let mem =
           match unwrap_wire !mem_ref with
           | Signal.Multiport_mem (_, _, _) as unwrapped -> unwrapped
           | _ -> assert false
         in
         r := Mem_read_port ({ s with s_deps = [ mem; addr ] }, mem, addr)
       | _ -> ())
    | _ -> ())
;;

let compress_wires signals =
  Signal_graph.create signals
  |> Signal_graph.iter ~f:(function
    | Signal.Wire (_, r) -> r := unwrap_wire !r
    | _ -> ())
;;

let canonicalize signals =
  let hash_memo = Hashtbl.create (module Signal.Uid) in
  let canonical = Hashtbl.create (module Signal.Uid) in
  let canonical_by_hash = Hashtbl.create (module Int) in
  (* we replace all sequential elements with wires, to be able to transform them later *)
  let sequential_wires = Hashtbl.create (module Signal.Uid) in
  List.iter
    (Signal_graph.topological_sort ~deps:children (Signal_graph.create signals))
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
  let canonical = canonicalize all_original_signals in
  let outputs = Circuit.outputs circuit in
  Circuit.create_exn
    ~name:(Circuit.name circuit)
    (List.map outputs ~f:(fun signal -> Hashtbl.find_exn canonical (Signal.uid signal)))
;;
