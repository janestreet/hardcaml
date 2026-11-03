open! Core
open Expect_test_helpers_base
open Hardcaml
module Events = Wave_data_in_mutable_events_four_state

let insert t time string =
  let value = Four_state_logic.of_string string in
  Events.insert
    t
    time
    ~bits:(Four_state_logic.bits value)
    ~mask:(Four_state_logic.mask value)
;;

let create_with_events ~width events =
  let t = Events.create width (ref 0) in
  List.iter events ~f:(fun (time, value) -> insert t time value);
  t
;;

let%expect_test "insert and display; out of order and overwriting inserts" =
  let t = create_with_events ~width:4 [ 0, "XXXX"; 10, "01ZX" ] in
  print_s [%sexp (t : Events.t)];
  [%expect
    {|
    ((time (0    10))
     (data (XXXX 01ZX))
     (num_events 2)
     (width      4)
     (max_time   10))
    |}];
  (* Insert between existing events. *)
  insert t 5 "1100";
  print_s [%sexp (t : Events.t)];
  [%expect
    {|
    ((time (0    5    10))
     (data (XXXX 1100 01ZX))
     (num_events 3)
     (width      4)
     (max_time   10))
    |}];
  (* Overwrite an existing event. *)
  insert t 5 "ZZ11";
  print_s [%sexp (t : Events.t)];
  [%expect
    {|
    ((time (0    5    10))
     (data (XXXX ZZ11 01ZX))
     (num_events 3)
     (width      4)
     (max_time   10))
    |}]
;;

let%expect_test "get by time returns both values" =
  let t = create_with_events ~width:2 [ 0, "XX"; 3, "0Z"; 7, "10" ] in
  let show time =
    let bits = Events.get t time in
    let mask = Events.get_mask t time in
    print_s [%message (time : int) (bits : Bits.t) (mask : Bits.t)]
  in
  show 0;
  [%expect
    {|
    ((time 0)
     (bits 11)
     (mask 11))
    |}];
  show 2;
  [%expect
    {|
    ((time 2)
     (bits 11)
     (mask 11))
    |}];
  show 3;
  [%expect
    {|
    ((time 3)
     (bits 00)
     (mask 01))
    |}];
  show 100;
  [%expect
    {|
    ((time 100)
     (bits 10)
     (mask 00))
    |}];
  require_does_raise (fun () ->
    let t = create_with_events ~width:2 [ 5, "01" ] in
    Events.get t 4);
  [%expect {| ("[Wave_data_in_mutable_events_four_state.get] Invalid time" (time 4)) |}];
  (* The error names the entry point that was actually called. *)
  require_does_raise (fun () ->
    let t = create_with_events ~width:2 [ 5, "01" ] in
    Events.get_mask t 4);
  [%expect
    {| ("[Wave_data_in_mutable_events_four_state.get_mask] Invalid time" (time 4)) |}]
;;

let%expect_test "a store with no events reads as all-X rather than raising" =
  let t = Events.create 2 (ref 0) in
  let bits = Events.get t 0 in
  let mask = Events.get_mask t 0 in
  print_s [%message (bits : Bits.t) (mask : Bits.t)];
  [%expect
    {|
    ((bits 11)
     (mask 11))
    |}]
;;

let%expect_test "equality is over contents; mask differences are inequal" =
  let t1 = create_with_events ~width:2 [ 0, "1X" ] in
  let t2 = create_with_events ~width:2 [ 0, "1X" ] in
  (* Same bits value as [t1] ('X' and '1' share bits=1), different mask value. *)
  let t3 = create_with_events ~width:2 [ 0, "11" ] in
  print_s
    [%message
      "" ~t1_equal_t2:(Events.equal t1 t2 : bool) ~t1_equal_t3:(Events.equal t1 t3 : bool)];
  [%expect
    {|
    ((t1_equal_t2 true)
     (t1_equal_t3 false))
    |}]
;;

let%expect_test "digestible string distinguishes mask-only differences" =
  let digest t =
    let bytes, length = Events.get_digestible_string t in
    Md5.digest_subbytes bytes ~pos:0 ~len:length |> Md5.to_hex
  in
  let x = create_with_events ~width:2 [ 0, "1X" ] in
  let one = create_with_events ~width:2 [ 0, "11" ] in
  let z = create_with_events ~width:2 [ 0, "1Z" ] in
  print_s
    [%message
      ""
        ~x_differs_from_one:(not (String.equal (digest x) (digest one)) : bool)
        ~x_differs_from_z:(not (String.equal (digest x) (digest z)) : bool)];
  [%expect
    {|
    ((x_differs_from_one true)
     (x_differs_from_z   true))
    |}]
;;

let%expect_test "bin_io roundtrip preserves contents and ignores unused capacity" =
  let t = create_with_events ~width:3 [ 0, "XXX"; 2, "01Z"; 5, "110" ] in
  let serialized = Bin_prot.Writer.to_string Events.bin_writer_t t in
  (* Growing capacity (without adding events) must not change the serialized form. *)
  Events.resize t;
  let serialized_after_resize = Bin_prot.Writer.to_string Events.bin_writer_t t in
  let roundtripped =
    Bin_prot.Reader.of_string Events.bin_reader_t serialized_after_resize
  in
  print_s
    [%message
      (String.length serialized : int)
        ~serialized_equal_after_resize:
          (String.equal serialized serialized_after_resize : bool)
        ~equal_after_roundtrip:(Events.equal t roundtripped : bool)
        ~unique_ids_differ:(Events.unique_id t <> Events.unique_id roundtripped : bool)
        (roundtripped : Events.t)];
  [%expect
    {|
    (("String.length serialized"    29)
     (serialized_equal_after_resize true)
     (equal_after_roundtrip         true)
     (unique_ids_differ             true)
     (roundtripped (
       (time (0   2   5))
       (data (XXX 01Z 110))
       (num_events 3)
       (width      3)
       (max_time   5))))
    |}]
;;

let make_wave ~name wave_data =
  { Wave_data.Wave.name
  ; width = Events.width wave_data
  ; typ = Wave_data.Type.Internal
  ; wave_format = Wave_format.Hex
  ; wave_data
  }
;;

let make_two_state_wave ~name wave_data =
  { Wave_data.Wave.name
  ; width = Wave_data_in_mutable_events.width wave_data
  ; typ = Wave_data.Type.Internal
  ; wave_format = Wave_format.Hex
  ; wave_data
  }
;;

let two_state_waveform names =
  let data = Wave_data_in_mutable_events.create 2 (ref 0) in
  List.iter
    [ 0, 1; 4, 2 ]
    ~f:(fun (time, value) ->
      Wave_data_in_mutable_events.insert data time (Bits.of_unsigned_int ~width:2 value));
  ( Wave_data.By_mutable_event
      (Array.of_list_map names ~f:(fun name -> make_two_state_wave ~name data))
  , data )
;;

let four_state_waveform names =
  let data = create_with_events ~width:2 [ 0, "XX"; 3, "1Z" ] in
  ( Wave_data.By_mutable_event_four_state
      (Array.of_list_map names ~f:(fun name -> make_wave ~name data))
  , data )
;;

let%expect_test "combine promotes the 2-state side when mixing formats" =
  (* Mixing formats must not raise: the 2-state side is promoted losslessly (all-zeros
     mask) and the result is 4-state, in argument order. This is e.g. the
     [waveform_viewer -property] flow: a 4-state waveform loaded from a VCD combined with
     2-state verdict waves. *)
  let show t =
    match (t : Wave_data.t) with
    | By_mutable_event _ -> raise_s [%message "expected a 4-state result"]
    | By_mutable_event_four_state waves ->
      Array.iter waves ~f:(fun wave ->
        print_s [%message wave.name ~_:(wave.wave_data : Events.t)])
  in
  let two_state, two_state_data = two_state_waveform [ "a" ] in
  let four_state, four_state_data = four_state_waveform [ "b" ] in
  let combined = Wave_data.combine two_state four_state in
  show combined;
  [%expect
    {|
    (a (
      (time (0  4))
      (data (01 10))
      (num_events 2)
      (width      2)
      (max_time   4)))
    (b (
      (time (0  3))
      (data (XX 1Z))
      (num_events 2)
      (width      2)
      (max_time   3)))
    |}];
  show (Wave_data.combine four_state two_state);
  [%expect
    {|
    (b (
      (time (0  3))
      (data (XX 1Z))
      (num_events 2)
      (width      2)
      (max_time   3)))
    (a (
      (time (0  4))
      (data (01 10))
      (num_events 2)
      (width      2)
      (max_time   4)))
    |}];
  (* The promoted side is a snapshot; the 4-state side still aliases its input. *)
  (match combined with
   | By_mutable_event_four_state waves ->
     print_s
       [%message
         (phys_same waves.(0).wave_data two_state_data : bool)
           (phys_equal waves.(1).wave_data four_state_data : bool)]
   | By_mutable_event _ -> assert false);
  [%expect
    {|
    (("phys_same (waves.(0)).wave_data two_state_data" false)
     ("phys_equal (waves.(1)).wave_data four_state_data" true))
    |}]
;;

let%expect_test "to_four_state promotes losslessly and to_two_state undoes it" =
  let two_state, _ = two_state_waveform [ "a" ] in
  let promoted = Wave_data.to_four_state two_state in
  print_s [%sexp (promoted : Wave_data.t)];
  [%expect
    {|
    (By_mutable_event_four_state ((
      (name        a)
      (width       2)
      (typ         Internal)
      (wave_format Hex)
      (wave_data (
        (time (0  4))
        (data (01 10))
        (num_events 2)
        (width      2)
        (max_time   4))))))
    |}];
  print_s [%message (Wave_data.equal (Wave_data.to_two_state promoted) two_state : bool)];
  [%expect {| ("Wave_data.equal (Wave_data.to_two_state promoted) two_state" true) |}]
;;

let%expect_test "combine concatenates waves of matching formats and aliases their data" =
  let names t =
    match (t : Wave_data.t) with
    | By_mutable_event waves -> Array.to_list waves |> List.map ~f:(fun w -> w.name)
    | By_mutable_event_four_state waves ->
      Array.to_list waves |> List.map ~f:(fun w -> w.name)
  in
  let two_state_a, two_state_a_data = two_state_waveform [ "a"; "b" ] in
  let two_state_c, two_state_c_data = two_state_waveform [ "c" ] in
  let two_state = Wave_data.combine two_state_a two_state_c in
  let four_state_x, four_state_x_data = four_state_waveform [ "x" ] in
  let four_state_y, four_state_y_data = four_state_waveform [ "y"; "z" ] in
  let four_state = Wave_data.combine four_state_x four_state_y in
  (* The result is a concatenation, in argument order, of waves that physically share
     their wave data with the inputs - [combine] copies nothing. *)
  let shares_data =
    match two_state, four_state with
    | By_mutable_event two_state, By_mutable_event_four_state four_state ->
      [ phys_equal two_state.(0).wave_data two_state_a_data
      ; phys_equal two_state.(1).wave_data two_state_a_data
      ; phys_equal two_state.(2).wave_data two_state_c_data
      ; phys_equal four_state.(0).wave_data four_state_x_data
      ; phys_equal four_state.(1).wave_data four_state_y_data
      ; phys_equal four_state.(2).wave_data four_state_y_data
      ]
    | _ -> assert false
  in
  print_s
    [%message
      ""
        ~two_state:(names two_state : string list)
        ~four_state:(names four_state : string list)
        (shares_data : bool list)];
  [%expect
    {|
    ((two_state  (a b c))
     (four_state (x y z))
     (shares_data (true true true true true true)))
    |}]
;;

let%expect_test "to_two_state is lossy: X and Z both read back as 0" =
  let data = create_with_events ~width:4 [ 0, "01XZ"; 3, "1010" ] in
  print_s
    [%sexp
      (Wave_data.to_two_state (By_mutable_event_four_state [| make_wave ~name:"a" data |])
       : Wave_data.t)];
  [%expect
    {|
    (By_mutable_event ((
      (name        a)
      (width       4)
      (typ         Internal)
      (wave_format Hex)
      (wave_data (
        (time (0    3))
        (data (0100 1010))
        (length   2)
        (width    4)
        (max_time 3))))))
    |}]
;;

let%expect_test "to_two_state preserves aliasing and each store's own length" =
  (* [shared] is traced under two names, so the two waves must still share one demoted
     allocation. [empty] has no events and its own [max_time] ref, as a waveform combined
     from another source would; demotion must not lengthen it to match [shared]. *)
  let shared = create_with_events ~width:2 [ 0, "XX"; 3, "1Z" ] in
  let empty = Events.create 2 (ref (-1)) in
  let four_state =
    Wave_data.By_mutable_event_four_state
      [| make_wave ~name:"a" shared
       ; make_wave ~name:"b" shared
       ; make_wave ~name:"empty" empty
      |]
  in
  let demoted =
    match Wave_data.to_two_state four_state with
    | By_mutable_event waves -> waves
    | By_mutable_event_four_state _ -> assert false
  in
  print_s
    [%message
      ""
        ~a_and_b_share_data:
          (phys_equal demoted.(0).wave_data demoted.(1).wave_data : bool)
        ~a_and_empty_share_data:
          (phys_equal demoted.(0).wave_data demoted.(2).wave_data : bool)
        ~source_lengths:(List.map [ shared; empty ] ~f:Events.length : int list)
        ~demoted_lengths:
          (Array.to_list demoted
           |> List.map ~f:(fun wave -> Wave_data_in_mutable_events.length wave.wave_data)
           : int list)];
  [%expect
    {|
    ((a_and_b_share_data     true)
     (a_and_empty_share_data false)
     (source_lengths (4 0))
     (demoted_lengths (4 4 0)))
    |}];
  (* The result is a snapshot: inserting into the source does not affect it. *)
  insert shared 9 "11";
  print_s
    [%message
      ""
        ~source_length:(Events.length shared : int)
        ~demoted_length:(Wave_data_in_mutable_events.length demoted.(0).wave_data : int)];
  [%expect
    {|
    ((source_length  10)
     (demoted_length 4))
    |}]
;;

let%expect_test "to_two_state is the identity on a 2-state waveform" =
  let two_state, _ = two_state_waveform [ "a" ] in
  print_s
    [%message
      "" ~same_waveform:(phys_equal two_state (Wave_data.to_two_state two_state) : bool)];
  [%expect {| (same_waveform true) |}]
;;

module%test [@name "widths mirroring the 2-state suite"] _ = struct
  let test_case ~width events =
    let t = create_with_events ~width events in
    let serialized = Bin_prot.Writer.to_string Events.bin_writer_t t in
    let roundtripped = Bin_prot.Reader.of_string Events.bin_reader_t serialized in
    print_s
      [%message
        (width : int)
          ~length:(Events.length t : int)
          ~num_events:(Events.num_events t : int)
          ~serialized_length:(String.length serialized : int)
          ~equal_after_roundtrip:(Events.equal t roundtripped : bool)
          (roundtripped : Events.t)]
  ;;

  let repeat n value = String.concat (List.init n ~f:(fun _ -> value))

  let%expect_test "width 1" =
    test_case ~width:1 [ 0, "0"; 3, "X"; 8, "Z" ];
    [%expect
      {|
      ((width                 1)
       (length                9)
       (num_events            3)
       (serialized_length     29)
       (equal_after_roundtrip true)
       (roundtripped (
         (time (0 3 8))
         (data (0 X Z))
         (num_events 3)
         (width      1)
         (max_time   8))))
      |}]
  ;;

  let%expect_test "width 64" =
    test_case
      ~width:64
      [ 0, repeat 64 "0"; 2, repeat 32 "X" ^ repeat 32 "1"; 5, repeat 64 "Z" ];
    [%expect
      {|
      ((width                 64)
       (length                6)
       (num_events            3)
       (serialized_length     61)
       (equal_after_roundtrip true)
       (roundtripped (
         (time (0 2 5))
         (data (
           0000000000000000000000000000000000000000000000000000000000000000
           XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX11111111111111111111111111111111
           ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ))
         (num_events 3)
         (width      64)
         (max_time   5))))
      |}]
  ;;

  let%expect_test "width 65 (multi word)" =
    test_case
      ~width:65
      [ 0, repeat 65 "0"; 1, "X" ^ repeat 64 "0"; 4, repeat 64 "1" ^ "Z" ];
    [%expect
      {|
      ((width                 65)
       (length                5)
       (num_events            3)
       (serialized_length     109)
       (equal_after_roundtrip true)
       (roundtripped (
         (time (0 1 4))
         (data (
           00000000000000000000000000000000000000000000000000000000000000000
           X0000000000000000000000000000000000000000000000000000000000000000
           1111111111111111111111111111111111111111111111111111111111111111Z))
         (num_events 3)
         (width      65)
         (max_time   4))))
      |}]
  ;;

  let%expect_test "digest differs when only the mask differs, at each width" =
    let digest t =
      let bytes, length = Events.get_digestible_string t in
      Md5.digest_subbytes bytes ~pos:0 ~len:length |> Md5.to_hex
    in
    (* Each pair has an identical [bits] value ('X' and '1' both have bits=1, 'Z' and '0'
       both have bits=0) and differs only in the mask. *)
    List.iter
      [ 1, "1", "X"; 64, repeat 64 "1", repeat 64 "X"; 65, repeat 65 "0", repeat 65 "Z" ]
      ~f:(fun (width, pure, impure) ->
        let digests_differ =
          not
            (String.equal
               (digest (create_with_events ~width [ 0, pure ]))
               (digest (create_with_events ~width [ 0, impure ])))
        in
        print_s [%message (width : int) (digests_differ : bool)]);
    [%expect
      {|
      ((width          1)
       (digests_differ true))
      ((width          64)
       (digests_differ true))
      ((width          65)
       (digests_differ true))
      |}]
  ;;
end

let%expect_test "insert before the first event" =
  let t = create_with_events ~width:4 [ 5, "1100"; 10, "01ZX" ] in
  insert t 1 "XXZZ";
  print_s [%sexp (t : Events.t)];
  [%expect
    {|
    ((time (1    5    10))
     (data (XXZZ 1100 01ZX))
     (num_events 3)
     (width      4)
     (max_time   10))
    |}]
;;

let%expect_test "an empty store deserialized from bin_io can still be inserted into" =
  (* A store deserialized from an empty payload has capacity 0; [resize] must grow it to a
     non-zero capacity or the next insert fails. *)
  let t = Events.create 4 (ref 0) in
  let roundtripped =
    Bin_prot.Reader.of_string
      Events.bin_reader_t
      (Bin_prot.Writer.to_string Events.bin_writer_t t)
  in
  print_s [%message "" ~capacity:(Events.capacity roundtripped : int)];
  insert roundtripped 3 "01XZ";
  print_s [%sexp (roundtripped : Events.t)];
  [%expect
    {|
    (capacity 0)
    ((time (3))
     (data (01XZ))
     (num_events 1)
     (width      4)
     (max_time   3))
    |}]
;;

let%expect_test "a wave whose width does not match its data is rejected on write" =
  let data = create_with_events ~width:2 [ 0, "XX" ] in
  let wave = { (make_wave ~name:"a" data) with width = 16 } in
  require_does_raise (fun () ->
    Wave_data.For_file_serialization.of_waves
      [| wave |]
      ~unique_id:Events.unique_id
      ~width:Events.width);
  [%expect
    {|
    ("Wave width does not match its wave data"
      (name       a)
      (wave_width 16)
      (data_width 2))
    |}]
;;
