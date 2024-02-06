open Base

module Aliases = struct
  type t = Signal.Uid.t Hashtbl.M(Signal.Uid).t

  let resolve_alias (t : t) uid = Hashtbl.find t uid
  let is_alias (t : t) uid = Hashtbl.mem t uid
end

type t =
  { schedule : Signal.t list
  ; regs : Signal.t list
  ; mems : Signal.t list
  ; consts : Signal.t list
  ; inputs : Signal.t list
  ; aliases : Aliases.t
  }
[@@deriving fields ~getters]

let find_elements circuit =
  Signal_graph.depth_first_search
    (Circuit.signal_graph circuit)
    ~init:([], [], [], [], [])
    ~f_before:(fun (regs, mems, consts, inputs, comb_signals) signal ->
    if Signal.is_empty signal
    then regs, mems, consts, inputs, comb_signals
    else if Signal.Type.is_reg signal
    then signal :: regs, mems, consts, inputs, comb_signals
    else if Signal.Type.is_const signal
    then regs, mems, signal :: consts, inputs, comb_signals
    else if Circuit.is_input circuit signal
    then regs, mems, consts, signal :: inputs, comb_signals
    else if Signal.Type.is_mem signal
    then regs, signal :: mems, consts, inputs, comb_signals
    else regs, mems, consts, inputs, signal :: comb_signals)
;;

let rec unwrap_signal (signal : Signal.t) =
  match signal with
  | Wire { driver; _ } ->
    if Signal.is_empty !driver
    then None
    else (
      match unwrap_signal !driver with
      | Some _ as ret -> ret
      | None -> Some !driver)
  | _ -> Some signal
;;

let unwrap_wire signal =
  match signal with
  | Signal.Type.Wire _ -> unwrap_signal signal
  | _ -> None
;;

let create_aliases signal_graph =
  let table = Hashtbl.create (module Signal.Uid) in
  Signal_graph.iter signal_graph ~f:(fun signal ->
    match unwrap_wire signal with
    | None -> ()
    | Some unwrapped ->
      Hashtbl.set ~key:(Signal.uid signal) ~data:(Signal.uid unwrapped) table);
  table
;;

(* Specialised signal dependencies that define a graph that breaks cycles through
   sequential elements. This is done by removing the input edges of registers and
   memories (excluding the read address, since hardcaml memories are read
   asynchronously).

   Instantiations do not allow cycles from output to input ports, which is a valid
   assumption for the simulator, but not in general.

   Note that all signals in the graph cannot be reached from just the outputs of a
   circuit using these dependencies. The (discarded) inputs to all registers and
   memories must also be included. *)
let scheduling_deps =
  (module Signal_graph.Deps_for_simulation_scheduling : Signal.Type.Deps)
;;

let create circuit =
  let regs, mems, consts, inputs, comb_signals = find_elements circuit in
  let outputs = Circuit.outputs circuit in
  let signal_graph = Signal_graph.create outputs in
  let aliases = create_aliases signal_graph in
  let schedule =
    if List.is_empty outputs
    then []
    else Signal_graph.topological_sort_exn ~deps:scheduling_deps signal_graph
  in
  let schedule_set =
    List.concat [ mems; comb_signals ]
    |> List.map ~f:Signal.uid
    |> Set.of_list (module Signal.Uid)
  in
  let schedule =
    List.filter schedule ~f:(fun signal -> Set.mem schedule_set (Signal.uid signal))
  in
  { schedule; regs; mems; consts; inputs; aliases }
;;
