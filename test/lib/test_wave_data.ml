open! Core
open Hardcaml

(* Build an event-based wave with the given (time, value) pairs. The actual data values
   are not consulted by [event_sequence_in_time_order], so we use [Bits.zero] for
   simplicity; this lets us focus the tests on time-ordering behaviour. *)
let make_wave ~name ~width ~times =
  let max_time = ref 0 in
  let bits = Wave_data_in_events.Bits.create width max_time in
  let store = Wave_data_in_events.Bits.event_store bits in
  List.iter times ~f:(fun time ->
    Wave_data_in_events.Bits.Event_store.insert store time (Bits.zero width);
    if time > !max_time then max_time := time);
  { Wave_data.Wave.name
  ; width
  ; typ = Wave_data.Type.Internal
  ; wave_format = Wave_format.Hex
  ; is_pseudo_clock = false
  ; wave_data = bits
  }
;;

let print_events waves =
  Wave_data.event_sequence_in_time_order waves
  |> Sequence.iter ~f:(fun { Wave_data.wave_index; event_index } ->
    let wave = waves.(wave_index) in
    let store = Wave_data_in_events.Bits.event_store wave.wave_data in
    let time = Wave_data_in_events.Bits.Event_store.get_time_at_index store event_index in
    print_s
      [%message
        "" ~wave:(wave.name : string) (wave_index : int) (event_index : int) (time : int)])
;;

let%expect_test "empty input" =
  print_events [||];
  [%expect {| |}]
;;

let%expect_test "all waves empty" =
  print_events
    [| make_wave ~name:"a" ~width:1 ~times:[]; make_wave ~name:"b" ~width:1 ~times:[] |];
  [%expect {| |}]
;;

let%expect_test "single wave" =
  print_events [| make_wave ~name:"a" ~width:1 ~times:[ 0; 3; 5; 9 ] |];
  [%expect
    {|
    ((wave a) (wave_index 0) (event_index 0) (time 0))
    ((wave a) (wave_index 0) (event_index 1) (time 3))
    ((wave a) (wave_index 0) (event_index 2) (time 5))
    ((wave a) (wave_index 0) (event_index 3) (time 9))
    |}]
;;

let%expect_test "interleaved waves" =
  print_events
    [| make_wave ~name:"a" ~times:[ 0; 4; 8 ] ~width:1
     ; make_wave ~name:"b" ~times:[ 1; 5; 9 ] ~width:1
     ; make_wave ~name:"c" ~times:[ 2; 6; 10 ] ~width:1
    |];
  [%expect
    {|
    ((wave a) (wave_index 0) (event_index 0) (time 0))
    ((wave b) (wave_index 1) (event_index 0) (time 1))
    ((wave c) (wave_index 2) (event_index 0) (time 2))
    ((wave a) (wave_index 0) (event_index 1) (time 4))
    ((wave b) (wave_index 1) (event_index 1) (time 5))
    ((wave c) (wave_index 2) (event_index 1) (time 6))
    ((wave a) (wave_index 0) (event_index 2) (time 8))
    ((wave b) (wave_index 1) (event_index 2) (time 9))
    ((wave c) (wave_index 2) (event_index 2) (time 10))
    |}]
;;

let%expect_test "ties between waves: ordering between equal-time events is unspecified \
                 but the times must be non-decreasing"
  =
  let waves =
    [| make_wave ~name:"a" ~times:[ 0; 5; 10 ] ~width:1
     ; make_wave ~name:"b" ~times:[ 0; 5; 10 ] ~width:1
    |]
  in
  let times =
    Wave_data.event_sequence_in_time_order waves
    |> Sequence.map ~f:(fun { Wave_data.wave_index; event_index } ->
      let store = Wave_data_in_events.Bits.event_store waves.(wave_index).wave_data in
      Wave_data_in_events.Bits.Event_store.get_time_at_index store event_index)
    |> Sequence.to_list
  in
  print_s [%sexp (times : int list)];
  [%expect {| (0 0 5 5 10 10) |}];
  let sorted = List.is_sorted times ~compare:Int.compare in
  print_s [%message (sorted : bool)];
  [%expect {| (sorted true) |}]
;;

let%expect_test "mix of empty and non-empty waves" =
  print_events
    [| make_wave ~name:"a" ~times:[] ~width:1
     ; make_wave ~name:"b" ~times:[ 1; 7 ] ~width:1
     ; make_wave ~name:"c" ~times:[] ~width:1
     ; make_wave ~name:"d" ~times:[ 0; 3; 6 ] ~width:1
    |];
  [%expect
    {|
    ((wave d) (wave_index 3) (event_index 0) (time 0))
    ((wave b) (wave_index 1) (event_index 0) (time 1))
    ((wave d) (wave_index 3) (event_index 1) (time 3))
    ((wave d) (wave_index 3) (event_index 2) (time 6))
    ((wave b) (wave_index 1) (event_index 1) (time 7))
    |}]
;;

let%expect_test "varying lengths and gaps" =
  print_events
    [| make_wave ~name:"short" ~times:[ 100 ] ~width:1
     ; make_wave ~name:"long" ~times:[ 0; 1; 2; 3; 4; 5; 200 ] ~width:1
     ; make_wave ~name:"middle" ~times:[ 50; 150 ] ~width:1
    |];
  [%expect
    {|
    ((wave long) (wave_index 1) (event_index 0) (time 0))
    ((wave long) (wave_index 1) (event_index 1) (time 1))
    ((wave long) (wave_index 1) (event_index 2) (time 2))
    ((wave long) (wave_index 1) (event_index 3) (time 3))
    ((wave long) (wave_index 1) (event_index 4) (time 4))
    ((wave long) (wave_index 1) (event_index 5) (time 5))
    ((wave middle) (wave_index 2) (event_index 0) (time 50))
    ((wave short) (wave_index 0) (event_index 0) (time 100))
    ((wave middle) (wave_index 2) (event_index 1) (time 150))
    ((wave long) (wave_index 1) (event_index 6) (time 200))
    |}]
;;

(* Property-style randomized check: against a reference implementation that just
   concatenates all (wave_index, event_index, time) tuples and sorts them by time. *)
let%expect_test "randomized: matches a reference sort" =
  let random_state = Random.State.make [| 42 |] in
  let max_failures = ref 0 in
  for _ = 1 to 100 do
    let num_waves = 1 + Random.State.int random_state 5 in
    let waves =
      Array.init num_waves ~f:(fun i ->
        let count = Random.State.int random_state 20 in
        let times =
          List.init count ~f:(fun _ -> Random.State.int random_state 50)
          |> List.dedup_and_sort ~compare:Int.compare
        in
        make_wave ~name:(sprintf "w%d" i) ~width:1 ~times)
    in
    let actual = Wave_data.event_sequence_in_time_order waves |> Sequence.to_list in
    let times_of (e : Wave_data.event) =
      let store = Wave_data_in_events.Bits.event_store waves.(e.wave_index).wave_data in
      Wave_data_in_events.Bits.Event_store.get_time_at_index store e.event_index
    in
    let actual_times = List.map actual ~f:times_of in
    let expected_times =
      Array.to_list waves
      |> List.concat_mapi ~f:(fun wave_index wave ->
        let store = Wave_data_in_events.Bits.event_store wave.wave_data in
        List.init
          (Wave_data_in_events.Bits.Event_store.length store)
          ~f:(fun event_index ->
            ( wave_index
            , event_index
            , Wave_data_in_events.Bits.Event_store.get_time_at_index store event_index ))
        |> List.map ~f:(fun (_, _, t) -> t))
      |> List.sort ~compare:Int.compare
    in
    if not ([%equal: int list] actual_times expected_times)
    then (
      incr max_failures;
      print_s [%message "mismatch" (actual_times : int list) (expected_times : int list)])
  done;
  print_s [%message (!max_failures : int)];
  [%expect {| (!max_failures 0) |}]
;;
