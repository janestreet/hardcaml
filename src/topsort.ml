open Base

(* This is a "vendored" version of [Topological_sort] specialized for Signals, and
   respecting the input node ordering. If we can get the new functionality backported it
   can be removed. *)

module Node = struct
  type t = Signal.t [@@deriving sexp_of]

  let compare a b = Signal.Uid.compare (Signal.uid a) (Signal.uid b)
  let hash a = Signal.Uid.hash (Signal.uid a)
end

module Edge = struct
  type 'a t =
    { from : 'a
    ; to_ : 'a
    }
  [@@deriving compare, sexp_of]

  let map t ~f = { from = f t.from; to_ = f t.to_ }
end

module Node_info = struct
  exception Cycle of Node.t list

  type state =
    | Unvisited
    | Visiting
    | Visited
  [@@deriving sexp_of]

  type t =
    { node : Node.t
    ; mutable state : state
    ; mutable outgoing : (t[@sexp.opaque]) list
    }
  [@@deriving fields ~getters, sexp_of]

  let create node = { node; state = Unvisited; outgoing = [] }
  let add_edge { Edge.from; to_ } = from.outgoing <- to_ :: from.outgoing

  (* [visit t ~visiting ~visited] visits all nodes reachable from [t], returning all
     newly visited nodes added to the front of [visited] in topological order.
     [visiting] is the list of nodes with [state = Visiting]. *)
  let rec visit t ~visiting ~visited =
    match t.state with
    | Visited -> visited
    | Visiting ->
      let cycle =
        match List.findi visiting ~f:(fun _ node -> phys_equal node t.node) with
        | None -> assert false
        | Some (i, _) -> List.rev (List.take visiting (i + 1))
      in
      raise (Cycle cycle)
    | Unvisited ->
      t.state <- Visiting;
      let visiting = t.node :: visiting in
      let visited =
        List.fold t.outgoing ~init:visited ~f:(fun visited t ->
          visit t ~visiting ~visited)
      in
      t.state <- Visited;
      t.node :: visited
  ;;
end

let sort ~(nodes : Signal.t list) ~(edges : Signal.t Edge.t list) =
  let info_by_node = Hashtbl.create (module Node) in
  let node_info node =
    Hashtbl.find_or_add info_by_node node ~default:(fun () -> Node_info.create node)
  in
  List.iter nodes ~f:(fun node -> ignore (node_info node : Node_info.t));
  List.iter edges ~f:(fun edge -> Node_info.add_edge (Edge.map edge ~f:node_info));
  let node_visit_order =
    (* Repeated nodes are not filtered by this - that's OK for our simulator use case. *)
    List.map nodes ~f:(Hashtbl.find_exn info_by_node)
  in
  let result =
    match
      List.fold node_visit_order ~init:[] ~f:(fun visited node_info ->
        Node_info.visit node_info ~visiting:[] ~visited)
    with
    | visited -> Ok visited
    | exception Node_info.Cycle cycle -> Error cycle
  in
  match result with
  | Ok _ as ok -> ok
  | Error cycle -> Error (`Cycle cycle)
;;
