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
    | Mux
    | Cat
    | Not
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
  | Signal.Const { constant; _ } -> Structure_kind.Const constant
  | Signal.Op2 { op; _ } -> Structure_kind.Op op
  | Signal.Mux _ -> Structure_kind.Mux
  | Signal.Cat _ -> Structure_kind.Cat
  | Signal.Not _ -> Structure_kind.Not
  | Signal.Wire _ -> Structure_kind.Wire (Signal.names signal)
  | Signal.Select { high; low; _ } -> Structure_kind.Select (high, low)
  | Signal.Reg _ -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Mem _ -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Multiport_mem _ -> Structure_kind.Sequential (Signal.uid signal)
  | Signal.Mem_read_port _ -> Structure_kind.Mem_read_port
  | Signal.Inst _ -> Structure_kind.Sequential (Signal.uid signal)
;;

let children (signal : Signal.t) =
  match structure_kind signal with
  | Structure_kind.Sequential _ -> []
  | _ ->
    (match signal with
     | Wire { driver; _ } -> [ !driver ]
     | Mem_read_port { memory; read_address; _ } -> [ memory; read_address ]
     | _ -> Signal.deps signal)
;;

let signal_id_map_children s ~f =
  { s with Signal.s_id = Signal.new_id (); s_deps = List.map s.Signal.s_deps ~f }
;;

let map_children signal ~f =
  match signal with
  | Signal.Empty -> signal
  | Signal.Const _ -> signal
  | Signal.Op2 { signal_id; op; arg_a; arg_b } ->
    let arg_a = f arg_a in
    let arg_b = f arg_b in
    Signal.Op2
      { signal_id = { signal_id with s_id = Signal.new_id (); s_deps = [ arg_a; arg_b ] }
      ; op
      ; arg_a
      ; arg_b
      }
  | Signal.Mux { signal_id; select; cases } ->
    let select = f select in
    let cases = List.map cases ~f in
    Signal.Mux
      { signal_id = { signal_id with s_id = Signal.new_id (); s_deps = select :: cases }
      ; select
      ; cases
      }
  | Signal.Cat { signal_id; args } ->
    let args = List.map args ~f in
    Signal.Cat
      { signal_id = { signal_id with s_id = Signal.new_id (); s_deps = args }; args }
  | Signal.Not { signal_id; arg } ->
    let arg = f arg in
    Signal.Not
      { signal_id = { signal_id with s_id = Signal.new_id (); s_deps = [ arg ] }; arg }
  | Signal.Wire { signal_id; driver } ->
    Signal.Wire
      { signal_id = signal_id_map_children signal_id ~f; driver = ref (f !driver) }
  | Signal.Select { signal_id; arg; high; low } ->
    let arg = f arg in
    Signal.Select
      { signal_id = { signal_id with s_id = Signal.new_id (); s_deps = [ arg ] }
      ; arg
      ; high
      ; low
      }
  | Signal.Multiport_mem _ | Signal.Reg _ -> assert false
  | Signal.Mem_read_port { signal_id; memory; read_address } ->
    Signal.Mem_read_port
      { signal_id =
          { signal_id with
            s_id = Signal.new_id ()
          ; s_deps = List.map signal_id.Signal.s_deps ~f
          }
      ; memory = f memory
      ; read_address = f read_address
      }
  | Signal.Mem _ -> failwith "Mem is unsupported"
  | Signal.Inst _ -> signal
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
  | Wire { driver = r_a; _ }, Wire { driver = r_b; _ } ->
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
  | Signal.Reg
      { signal_id
      ; register =
          { Signal.reg_clock
          ; reg_clock_edge
          ; reg_reset
          ; reg_reset_edge
          ; reg_reset_value
          ; reg_clear
          ; reg_clear_level
          ; reg_clear_value
          ; reg_enable
          }
      ; d
      } ->
    let d = get_canonical d in
    let register =
      { Signal.reg_clock = get_canonical reg_clock
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
    Signal.Reg
      { signal_id =
          { signal_id with
            s_id = Signal.new_id ()
          ; s_deps =
              [ d
              ; register.reg_clock
              ; register.reg_reset
              ; register.reg_reset_value
              ; register.reg_clear
              ; register.reg_clear_value
              ; register.reg_enable
              ]
          }
      ; register
      ; d
      }
  | Signal.Multiport_mem { signal_id; size; write_ports } ->
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
      { signal_id =
          { signal_id with s_deps = List.map signal_id.Signal.s_deps ~f:get_canonical }
      ; size
      ; write_ports
      }
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
  | Signal.Wire { driver; _ } when List.is_empty (Signal.names s) -> unwrap_wire !driver
  | _ -> s
;;

let fix_mem_read_ports signals =
  (* We wrap all sequential signals in wires. However, Mem_read_port expects to contain
     Multiport_mem and not Multiport_mem wrapped in wire. Rewrite the wire out. *)
  List.iter signals ~f:(function
    | Signal.Wire { driver; _ } ->
      (* every Mem_read_port signal is pointed to by a single wire *)
      (match !driver with
       | Signal.Mem_read_port
           { signal_id; memory = Wire { driver = mem_ref; _ }; read_address } ->
         let memory =
           match unwrap_wire !mem_ref with
           | Signal.Multiport_mem _ as unwrapped -> unwrapped
           | _ -> assert false
         in
         driver
         := Mem_read_port
              { signal_id = { signal_id with s_deps = [ memory; read_address ] }
              ; memory
              ; read_address
              }
       | _ -> ())
    | _ -> ())
;;

let compress_wires signals =
  Signal_graph.create signals
  |> Signal_graph.iter ~f:(function
    | Signal.Wire { driver; _ } -> driver := unwrap_wire !driver
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
