open! Core0

module Instance = struct
  type t = { global_ call_stack : Call_stack.t }
  [@@deriving sexp_of, hash, compare ~localize]

  let create circuit =
    { call_stack =
        Circuit.caller_id circuit |> Option.value_map ~default:[] ~f:Caller_id.call_stack
    }
  ;;
end

module T = struct
  type t =
    { name : String.t
    ; example : Circuit.t
    ; instances : (Instance.t, int) Hashtbl.t
    ; coverage : (Signal_graph.Normalized_signal_uid.t, Signal_coverage.t) Hashtbl.t
    }
  [@@deriving fields ~getters]

  let total_cases t = Hashtbl.length t.coverage
  let covered_cases t = Hashtbl.count t.coverage ~f:Signal_coverage.fully_covered

  let unexpectedly_observed_cases t =
    Hashtbl.fold t.coverage ~init:0 ~f:(fun ~key:_ ~data:s count ->
      count + Signal_coverage.unexpectedly_observed_cases s)
  ;;
end

include Coverage.Make (T)
include T

let add_instance t circuit =
  let new_name = Circuit.name circuit in
  if not (String.equal new_name t.name)
  then raise_s [%message "Circuits with different names" t.name new_name];
  Hashtbl.update t.instances (Instance.create circuit) ~f:(function
    | None -> 1
    | Some count -> count + 1)
;;

let create circuit =
  let t =
    { example = circuit
    ; name = Circuit.name circuit
    ; instances = Hashtbl.create (module Instance)
    ; coverage = Hashtbl.create (module Signal_graph.Normalized_signal_uid)
    }
  in
  add_instance t circuit;
  t
;;

let instance_counts t = Hashtbl.to_alist t.instances
let signal_coverage t = Hashtbl.data t.coverage

let add_if_structurally_equal t circuit =
  if Circuit.structural_compare t.example circuit
  then (
    add_instance t circuit;
    true)
  else false
;;

let maybe_find_or_create_signal_coverage t signal norm_id =
  Hashtbl.change t.coverage norm_id ~f:(function
    | Some coverage -> Some coverage
    | None -> Signal_coverage.maybe_create norm_id signal);
  Hashtbl.find t.coverage norm_id
;;
