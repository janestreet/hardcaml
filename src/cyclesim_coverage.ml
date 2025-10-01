open! Core0

module Global_config = struct
  include Coverage_global_config
  include For_cyclesim_coverage
end

module Global_state = struct
  type t = { mutable coverage : Circuit_coverage.t list }

  let global = { coverage = [] }
  let coverage () = global.coverage
  let clear_coverage () = global.coverage <- []
  let add_coverage coverage = global.coverage <- coverage :: global.coverage

  let enable_and_reset_for_expect_test ?verbose () =
    Global_config.set_expect_test_mode ?verbose ();
    clear_coverage ()
  ;;
end

module For_cyclesim = struct
  let mark_coverage signal_coverage () =
    List.iter signal_coverage ~f:(fun coverage -> Coverage_instance.sample coverage)
  ;;

  let find_or_create_coverage_for_circuit circuit =
    match
      List.find (Global_state.coverage ()) ~f:(fun coverage ->
        Circuit_coverage.add_if_structurally_equal coverage circuit)
    with
    | Some coverage -> coverage
    | None ->
      let coverage = Circuit_coverage.create circuit in
      Global_state.add_coverage coverage;
      coverage
  ;;

  let create_coverage_for_sim sim circuit =
    let coverage = find_or_create_coverage_for_circuit circuit in
    Signal_graph.compute_normalized_uids (Circuit.signal_graph circuit)
    |> List.filter_map ~f:(fun (normalized_uid, signal) ->
      let%map.Option signal_coverage =
        Circuit_coverage.maybe_find_or_create_signal_coverage
          coverage
          signal
          normalized_uid
      in
      Coverage_instance.create sim signal signal_coverage)
  ;;

  let wrap sim circuit =
    Cyclesim0.Private.modify
      sim
      [ Before, At_clock_edge, mark_coverage (create_coverage_for_sim sim circuit) ]
  ;;

  let maybe_wrap sim circuit =
    if Global_config.coverage_enabled () then wrap sim circuit else sim
  ;;
end

let set_coverage_caller_id_mode () = Caller_id.set_mode Coverage_filtered_trace

module For_exe = struct
  let maybe_output_results () =
    if Global_config.exe_coverage_enabled ()
    then (
      let oc = Out_channel.create "coverage.txt" in
      Coverage_report.write
        oc
        (Global_state.coverage ())
        ~hide_unstable:false
        ~verbose:(Global_config.verbose ())
        ~compact:false;
      Out_channel.close oc)
  ;;

  let maybe_init () =
    if Global_config.exe_coverage_enabled ()
    then (
      set_coverage_caller_id_mode ();
      Stdlib.at_exit (fun () -> maybe_output_results ()))
  ;;
end

module For_expect_tests = struct
  let enable_and_maybe_reset () =
    Global_state.enable_and_reset_for_expect_test ();
    set_coverage_caller_id_mode ()
  ;;

  let output_results' ~compact =
    Coverage_report.write
      Out_channel.stdout
      (Global_state.coverage ())
      ~hide_unstable:true
      ~verbose:(Global_config.verbose ())
      ~compact
  ;;

  let output_results () = output_results' ~compact:false
  let output_compact_results () = output_results' ~compact:true
end

let () = For_exe.maybe_init ()
