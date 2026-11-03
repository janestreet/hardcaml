open! Core
open Hardcaml

(* Build an event-based wave with the given (time, value) pairs. The actual data values
   are not consulted by [event_sequence_in_time_order], so we use [Bits.zero] for
   simplicity; this lets us focus the tests on time-ordering behaviour. *)
let make_wave ~name ~width ~times =
  let max_time = ref 0 in
  let bits = Wave_data_in_mutable_events.create width max_time in
  List.iter times ~f:(fun time ->
    Wave_data_in_mutable_events.insert bits time (Bits.zero width);
    if time > !max_time then max_time := time);
  { Wave_data.Wave.name
  ; width
  ; typ = Wave_data.Type.Internal
  ; wave_format = Wave_format.Hex
  ; wave_data = bits
  }
;;

let num_events wave = Wave_data_in_mutable_events.num_events wave.Wave_data.Wave.wave_data

let time_at_index wave event_index =
  Wave_data_in_mutable_events.get_time_at_index wave.Wave_data.Wave.wave_data event_index
;;

let event_sequence_in_time_order waves =
  Wave_data.event_sequence_in_time_order waves ~num_events ~time_at_index
;;

let print_events waves =
  event_sequence_in_time_order waves
  |> Sequence.iter ~f:(fun { Wave_data.wave_index; event_index } ->
    let wave = waves.(wave_index) in
    let time = time_at_index wave event_index in
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
    event_sequence_in_time_order waves
    |> Sequence.map ~f:(fun { Wave_data.wave_index; event_index } ->
      time_at_index waves.(wave_index) event_index)
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
    let actual = event_sequence_in_time_order waves |> Sequence.to_list in
    let times_of (e : Wave_data.event) =
      time_at_index waves.(e.wave_index) e.event_index
    in
    let actual_times = List.map actual ~f:times_of in
    let expected_times =
      Array.to_list waves
      |> List.concat_mapi ~f:(fun wave_index wave ->
        List.init
          (Wave_data_in_mutable_events.num_events wave.wave_data)
          ~f:(fun event_index ->
            ( wave_index
            , event_index
            , Wave_data_in_mutable_events.get_time_at_index wave.wave_data event_index ))
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

module%test [@name "Wave_data_in_mutable_events bin_io"] _ = struct
  let serialize t = Bin_prot.Writer.to_string Wave_data_in_mutable_events.bin_writer_t t
  let deserialize s = Bin_prot.Reader.of_string Wave_data_in_mutable_events.bin_reader_t s

  let test_case ~width events =
    let t = Wave_data_in_mutable_events.create width (ref 0) in
    List.iter events ~f:(fun (time, value) ->
      Wave_data_in_mutable_events.insert t time (Bits.of_unsigned_int ~width value));
    let serialized = serialize t in
    let serialized_length = String.length serialized in
    for _ = 1 to 5 do
      Wave_data_in_mutable_events.resize t
    done;
    let resized_serialized = serialize t in
    let resized_serialized_length = String.length resized_serialized in
    let serialized_equal_after_resize = String.equal serialized resized_serialized in
    let t = deserialize serialized in
    let roundtripped =
      List.init (Wave_data_in_mutable_events.num_events t) ~f:(fun index ->
        ( Wave_data_in_mutable_events.get_time_at_index t index
        , Wave_data_in_mutable_events.get_data_at_index t index |> Bits.to_unsigned_int ))
    in
    print_s
      [%message
        (width : int)
          ~length:(Wave_data_in_mutable_events.length t : int)
          ~num_events:(Wave_data_in_mutable_events.num_events t : int)
          (serialized_length : int)
          (resized_serialized_length : int)
          (serialized_equal_after_resize : bool)
          (roundtripped : (int * int) list)]
  ;;

  let%expect_test "roundtrip and ignore unused capacity" =
    test_case ~width:1 [ 0, 0; 3, 1; 8, 0 ];
    test_case ~width:4 [ 0, 0; 2, 3; 5, 15 ];
    test_case ~width:65 [ 0, 0; 1, 1; 4, 2 ];
    [%expect
      {|
      ((width 1) (length 9) (num_events 3) (serialized_length 18)
       (resized_serialized_length 18) (serialized_equal_after_resize true)
       (roundtripped ((0 0) (3 1) (8 0))))
      ((width 4) (length 6) (num_events 3) (serialized_length 18)
       (resized_serialized_length 18) (serialized_equal_after_resize true)
       (roundtripped ((0 0) (2 3) (5 15))))
      ((width 65) (length 5) (num_events 3) (serialized_length 58)
       (resized_serialized_length 58) (serialized_equal_after_resize true)
       (roundtripped ((0 0) (1 1) (4 2))))
      |}]
  ;;

  let%expect_test "an empty store deserialized from bin_io can still be inserted into" =
    (* A store deserialized from an empty payload has capacity 0; [resize] must grow it to
       a non-zero capacity or the next insert fails. *)
    let t = Wave_data_in_mutable_events.create 4 (ref 0) in
    let roundtripped = deserialize (serialize t) in
    print_s
      [%message "" ~capacity:(Wave_data_in_mutable_events.capacity roundtripped : int)];
    Wave_data_in_mutable_events.insert roundtripped 3 (Bits.of_unsigned_int ~width:4 5);
    print_s [%sexp (roundtripped : Wave_data_in_mutable_events.t)];
    [%expect
      {|
      (capacity 0)
      ((time (3)) (data (0101)) (length 1) (width 4) (max_time 3))
      |}]
  ;;

  let%expect_test "times are delta-encoded: size tracks gaps, not absolute magnitude" =
    let build ~base =
      let width = 8 in
      let t = Wave_data_in_mutable_events.create width (ref 0) in
      List.iteri [ 0; 5; 9 ] ~f:(fun value offset ->
        Wave_data_in_mutable_events.insert
          t
          (base + offset)
          (Bits.of_unsigned_int ~width value));
      t
    in
    let roundtripped_times t =
      let t = deserialize (serialize t) in
      List.init (Wave_data_in_mutable_events.num_events t) ~f:(fun index ->
        Wave_data_in_mutable_events.get_time_at_index t index)
    in
    let small = build ~base:0 in
    let large = build ~base:1_000_000 in
    (* The 5- and 4-cycle gaps encode to one byte each in both cases. Only the first
       (absolute) timestamp and [max_time] grow with magnitude, so the sizes differ by a
       small constant rather than scaling with every event. *)
    print_s
      [%message
        (roundtripped_times small : int list)
          (roundtripped_times large : int list)
          ~small_serialized_length:(String.length (serialize small) : int)
          ~large_serialized_length:(String.length (serialize large) : int)];
    [%expect
      {|
      (("roundtripped_times small" (0 5 9))
       ("roundtripped_times large" (1000000 1000005 1000009))
       (small_serialized_length 18) (large_serialized_length 26))
      |}]
  ;;
end

module%test [@name "Wave_data.For_file_serialization sharing"] _ = struct
  let serialize t =
    Bin_prot.Writer.to_string
      (Wave_data.For_file_serialization.bin_writer_t
         Wave_data_in_mutable_events.bin_writer_t)
      (Wave_data.For_file_serialization.of_waves
         (Wave_data.to_mutable_events t)
         ~unique_id:Wave_data_in_mutable_events.unique_id
         ~width:Wave_data_in_mutable_events.width)
  ;;

  let deserialize s =
    Wave_data.By_mutable_event
      (Bin_prot.Reader.of_string
         (Wave_data.For_file_serialization.bin_reader_t
            Wave_data_in_mutable_events.bin_reader_t)
         s
       |> Wave_data.For_file_serialization.to_waves
            ~width:Wave_data_in_mutable_events.width)
  ;;

  let make_data ~width ~events =
    let data = Wave_data_in_mutable_events.create width (ref 0) in
    List.iter events ~f:(fun (time, value) ->
      Wave_data_in_mutable_events.insert data time (Bits.of_unsigned_int ~width value));
    data
  ;;

  let make_wave ~name data =
    { Wave_data.Wave.name
    ; width = Wave_data_in_mutable_events.width data
    ; typ = Wave_data.Type.Internal
    ; wave_format = Wave_format.Hex
    ; wave_data = data
    }
  ;;

  let events = [ 0, 1; 3, 7; 9, 255 ]

  let%expect_test "aliased waves are serialized once, and sharing is restored on read" =
    let shared = make_data ~width:8 ~events in
    let aliased =
      Wave_data.By_mutable_event
        [| make_wave ~name:"a" shared
         ; make_wave ~name:"b" shared
         ; make_wave ~name:"c" shared
        |]
    in
    let unaliased =
      Wave_data.By_mutable_event
        (Array.of_list_map [ "a"; "b"; "c" ] ~f:(fun name ->
           make_wave ~name (make_data ~width:8 ~events)))
    in
    (* The aliased waveform serializes its wave data once; the equal-but-unaliased
       waveform pays for it three times. *)
    print_s
      [%message
        ""
          ~aliased_serialized_length:(String.length (serialize aliased) : int)
          ~unaliased_serialized_length:(String.length (serialize unaliased) : int)];
    [%expect {| ((aliased_serialized_length 38) (unaliased_serialized_length 74)) |}];
    let wave_datas t =
      match deserialize (serialize t) with
      | Wave_data.By_mutable_event waves ->
        Array.map waves ~f:(fun wave -> wave.wave_data)
      | By_mutable_event_four_state _ -> assert false
    in
    (* Round tripping preserves the contents of both waveforms, restores the sharing of
       the aliased one, and does not invent sharing for the unaliased one (data is only
       shared when it was known to be aliased upfront, never by comparing contents). *)
    let aliased_datas = wave_datas aliased in
    let unaliased_datas = wave_datas unaliased in
    (* aliased roundtrips *)
    assert (Wave_data.equal aliased (deserialize (serialize aliased)));
    (* unaliased roundtrips *)
    assert (Wave_data.equal unaliased (deserialize (serialize unaliased)));
    (* aliased roundtrip shares data *)
    assert (
      phys_equal aliased_datas.(0) aliased_datas.(1)
      && phys_equal aliased_datas.(0) aliased_datas.(2));
    (* unaliased roundtrip doesn't share data *)
    assert (
      not
        (phys_equal unaliased_datas.(0) unaliased_datas.(1)
         || phys_equal unaliased_datas.(0) unaliased_datas.(2)
         || phys_equal unaliased_datas.(1) unaliased_datas.(2)));
    [%expect {| |}]
  ;;

  let%expect_test "a wave whose width does not match its data is rejected" =
    let data = make_data ~width:8 ~events in
    let wave = { (make_wave ~name:"a" data) with width = 16 } in
    (* Rejected on write, where the bug is. *)
    Expect_test_helpers_base.require_does_raise (fun () ->
      serialize (Wave_data.By_mutable_event [| wave |]));
    [%expect
      {|
      ("Wave width does not match its wave data"
        (name       a)
        (wave_width 16)
        (data_width 8))
      |}];
    (* Still rejected on read, so a file written before the write-side check (or by hand)
       cannot smuggle a bad width in. The payload is built by serializing a consistent
       waveform and then patching the wave's [width] field, which is the byte holding [8]
       immediately after the length-prefixed name. The name is the last thing in the
       payload apart from the wave's four remaining single-byte fields, so we take the
       last occurrence. *)
    let good = make_wave ~name:"a" data in
    let serialized = serialize (Wave_data.By_mutable_event [| good |]) in
    let width_field_pos =
      String.substr_index_all serialized ~may_overlap:false ~pattern:"\001a"
      |> List.last_exn
      |> Int.( + ) 2
    in
    assert (Char.equal serialized.[width_field_pos] (Char.of_int_exn 8));
    let serialized =
      String.mapi serialized ~f:(fun index char ->
        if index = width_field_pos then Char.of_int_exn 16 else char)
    in
    Expect_test_helpers_base.require_does_raise (fun () -> deserialize serialized);
    [%expect
      {|
      ("Serialized wave width does not match its wave data"
       (name       a)
       (wave_width 16)
       (data_width 8))
      |}]
  ;;
end
