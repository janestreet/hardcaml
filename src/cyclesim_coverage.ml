open Base

include struct
  open Core
  module Out_channel = Out_channel
end

module State = struct
  type t = { mutable coverage : Circuit_coverage.t list } [@@deriving fields ~getters]

  let global = lazy { coverage = [] }
  let global () = force global
  let global_coverage () = (global ()).coverage

  let add_global_coverage coverage =
    let global = global () in
    global.coverage <- coverage :: global.coverage
  ;;
end

let mark_coverage signal_coverage () =
  List.iter signal_coverage ~f:(fun coverage -> Coverage_instance.sample coverage)
;;

let find_or_create_coverage_for_circuit circuit =
  match
    List.find (State.global_coverage ()) ~f:(fun coverage ->
      Circuit_coverage.add_if_structurally_equal coverage circuit)
  with
  | Some coverage -> coverage
  | None ->
    let coverage = Circuit_coverage.create circuit in
    State.add_global_coverage coverage;
    coverage
;;

let create_coverage_for_sim sim circuit =
  let coverage = find_or_create_coverage_for_circuit circuit in
  Signal_graph.compute_normalized_uids (Circuit.signal_graph circuit)
  |> List.filter_map ~f:(fun (normalized_uid, signal) ->
    let%map.Option signal_coverage =
      Circuit_coverage.maybe_find_or_create_signal_coverage coverage signal normalized_uid
    in
    Coverage_instance.create sim signal signal_coverage)
;;

let wrap sim circuit =
  Cyclesim0.Private.modify
    sim
    [ Before, At_clock_edge, mark_coverage (create_coverage_for_sim sim circuit) ]
;;

let verbose_coverage =
  lazy (Sys.getenv "HARDCAML_CYCLESIM_COVERAGE_VERBOSE" |> Option.is_some)
;;

let should_track_coverage =
  lazy
    (Sys.getenv "HARDCAML_CYCLESIM_COVERAGE" |> Option.is_some || force verbose_coverage)
;;

let maybe_wrap sim circuit = if force should_track_coverage then wrap sim circuit else sim

let output_results () =
  let oc = Out_channel.create "coverage.txt" in
  Coverage_report.write oc (State.global_coverage ()) ~verbose:(force verbose_coverage);
  Out_channel.close oc
;;

let () =
  if force should_track_coverage
  then (
    Caller_id.set_mode Coverage_filtered_trace;
    Stdlib.at_exit (fun () -> output_results ()))
;;
