open Core
open Hardcaml
open Signal
open Always

let outputs signals =
  List.mapi signals ~f:(fun i signal -> output [%string {|output-%{i#Int}|}] signal)
;;

let test_coverage create_outputs =
  Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
  let sim =
    Cyclesim.create (Circuit.create_exn ~name:"circuit" (outputs (create_outputs ())))
  in
  Cyclesim.cycle sim;
  Cyclesim_coverage_expect_test.output_results ()
;;

let create_out_var ?(named = false) i =
  let output = Variable.wire ~default:gnd () in
  if named then ignore (output.value -- [%string {|o%{i#Int}|}] : Signal.t);
  output
;;

let%expect_test "if - unnamed" =
  let create_in_out i = input [%string {|i%{i#Int}|}] 1, create_out_var i in
  test_coverage (fun () ->
    let i1, o1 = create_in_out 1 in
    let i2, o2 = create_in_out 2 in
    let i3, o3 = create_in_out 3 in
    let i4, o4 = create_in_out 4 in
    compile
      ([ if_ i1 [ o1 <--. 1 ] [ o1 <--. 2 ] ]
       @ elif i2 [ o2 <--. 3 ] [ o2 <--. 4 ]
       @ [ when_ i3 [ o3 <--. 5 ] ]
       @ [ unless i4 [ o4 <--. 6 ] ]);
    [ o1.value; o2.value; o3.value; o4.value ]);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 4
    muxes with full coverage: 0

    Mux with id: 3
        always if
            created at: <elided>
            driving (always variable): <elided>
        selector names: i1
        saw selector values of: 0
        never saw: 1

    Mux with id: 9
        always elif
            created at: <elided>
            driving (always variable): <elided>
        selector names: i2
        saw selector values of: 0
        never saw: 1

    Mux with id: 15
        always when
            created at: <elided>
            driving (always variable): <elided>
        selector names: i3
        saw selector values of: 0
        never saw: 1

    Mux with id: 21
        always unless
            created at: <elided>
            driving (always variable): <elided>
        selector names: i4
        saw selector values of: 0
        never saw: 1
    |}]
;;

let%expect_test "if - named" =
  let create_in_out i = input [%string {|i%{i#Int}|}] 1, create_out_var i ~named:true in
  test_coverage (fun () ->
    let i1, o1 = create_in_out 1 in
    let i2, o2 = create_in_out 2 in
    let i3, o3 = create_in_out 3 in
    let i4, o4 = create_in_out 4 in
    compile
      ([ if_ i1 [ o1 <--. 1 ] [ o1 <--. 2 ] ]
       @ elif i2 [ o2 <--. 3 ] [ o2 <--. 4 ]
       @ [ when_ i3 [ o3 <--. 5 ] ]
       @ [ unless i4 [ o4 <--. 6 ] ]);
    [ o1.value; o2.value; o3.value; o4.value ]);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 4
    muxes with full coverage: 0

    Mux with id: 3
        always if
            created at: <elided>
            driving (always variable): o1
        selector names: i1
        saw selector values of: 0
        never saw: 1

    Mux with id: 9
        always elif
            created at: <elided>
            driving (always variable): o2
        selector names: i2
        saw selector values of: 0
        never saw: 1

    Mux with id: 15
        always when
            created at: <elided>
            driving (always variable): o3
        selector names: i3
        saw selector values of: 0
        never saw: 1

    Mux with id: 21
        always unless
            created at: <elided>
            driving (always variable): o4
        selector names: i4
        saw selector values of: 0
        never saw: 1
    |}]
;;

let clock = Signal.input "clock" 1
let reg_spec = Reg_spec.create () ~clock

module State = struct
  type t =
    | A
    | B
    | C
  [@@deriving compare ~localize, enumerate, sexp_of, variants]
end

let create_sm encoding =
  State_machine.create (module State) reg_spec ~encoding ~enable:vdd
;;

let%expect_test "switch full" =
  let test encoding =
    let sm = create_sm encoding in
    let out = create_out_var 0 ~named:true in
    test_coverage (fun () ->
      compile
        [ sm.switch
            [ A, [ out <--. 1; sm.set_next B ]
            ; B, [ out <--. 2; sm.set_next C ]
            ; C, [ out <--. 3; sm.set_next A ]
            ]
        ];
      [ out.value ])
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 0x
      circuits tested: 1
      circuits with full coverage: 0

      ====== Circuit coverage for circuit ======

      total coverage: 0x
      cases: 0x
      always_states: 0x

      ==== Cases coverage ====

      total: 0x
      cases tested: 2
      cases with full coverage: 0

      Cases with id: 3
          always switch
              created at: <elided>
              driving (always variable): o0
          selector matched cases: A
          never matched: B C

      Cases with id: 6
          always switch
              created at: <elided>
              driving (state variable): <elided>
          selector matched cases: A
          never matched: B C

      ==== Always state coverage ====

      total: 0x
      always states tested: 1
      always states with full coverage: 0

      Always state with id: 4
          never saw:
              A -> B
              B -> C
              C -> A
      |}]);
  (* Metadata for the last case with one-hot encoding is poor...The mux is collapsed in to
     the proc mux and that shows up instead. The vast majority of code uses Binary
     encoding, but if apps start using Onehot more, improve this output. *)
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x
    always_states: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 6
    muxes with full coverage: 0

    Mux with id: 3
        always proc
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    Mux with id: 7
        always proc
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 8
        always switch B
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 10
        always switch A
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 16
        always switch B
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    Mux with id: 17
        always switch A
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        never saw:
            A -> A
            A -> B
            B -> B
            B -> C
            C -> A
            C -> C
    |}]
;;

let%expect_test "switch with default" =
  let test encoding =
    let sm = create_sm encoding in
    let out = create_out_var 0 ~named:true in
    test_coverage (fun () ->
      compile
        [ sm.switch
            ~default:[]
            [ A, [ out <--. 1; sm.set_next B ]; B, [ out <--. 2; sm.set_next C ] ]
        ];
      [ out.value ])
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 0x
      circuits tested: 1
      circuits with full coverage: 0

      ====== Circuit coverage for circuit ======

      total coverage: 0x
      cases: 0x
      always_states: 0x

      ==== Cases coverage ====

      total: 0x
      cases tested: 2
      cases with full coverage: 0

      Cases with id: 3
          always switch
              created at: <elided>
              driving (always variable): o0
          selector matched cases: A
          never matched: B C

      Cases with id: 6
          always switch
              created at: <elided>
              driving (state variable): <elided>
          selector matched cases: A
          never matched: B C

      ==== Always state coverage ====

      total: 0x
      always states tested: 1
      always states with full coverage: 0

      Always state with id: 4
          never saw:
              A -> B
              B -> C
              C -> C
      |}]);
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x
    always_states: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 4
    muxes with full coverage: 0

    Mux with id: 3
        always proc
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    Mux with id: 7
        always proc
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 8
        always switch A
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 13
        always switch A
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        never saw:
            A -> A
            A -> B
            B -> B
            B -> C
            C -> C
    |}]
;;

let%expect_test "switch with multiple defaults" =
  let test encoding =
    let sm = create_sm encoding in
    let out = create_out_var 0 ~named:true in
    test_coverage (fun () ->
      compile [ sm.switch ~default:[] [ A, [ out <--. 1; sm.set_next B ] ] ];
      [ out.value ])
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 0x
      circuits tested: 1
      circuits with full coverage: 0

      ====== Circuit coverage for circuit ======

      total coverage: 0x
      cases: 0x
      always_states: 0x

      ==== Cases coverage ====

      total: 0x
      cases tested: 2
      cases with full coverage: 0

      Cases with id: 3
          always switch
              created at: <elided>
              driving (always variable): o0
          selector matched cases: A
          never matched: (B | C)

      Cases with id: 6
          always switch
              created at: <elided>
              driving (state variable): <elided>
          selector matched cases: A
          never matched: (B | C)

      ==== Always state coverage ====

      total: 0x
      always states tested: 1
      always states with full coverage: 0

      Always state with id: 4
          never saw:
              A -> B
              B -> B
      |}]);
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x
    always_states: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 2
    muxes with full coverage: 0

    Mux with id: 3
        always proc
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    Mux with id: 7
        always proc
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        never saw:
            A -> A
            A -> B
            B -> B
    |}]
;;

let%expect_test "switch dead code elimination" =
  let test encoding =
    let sm = create_sm encoding in
    let out = create_out_var 0 ~named:true in
    test_coverage (fun () ->
      compile
        [ sm.switch
            [ A, [ out <--. 1; sm.set_next B ]
            ; B, [ out <--. 2; sm.set_next C ]
            ; C, [ sm.set_next A ] (* This case isn't relevant for out *)
            ]
        ];
      [ out.value ])
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 0x
      circuits tested: 1
      circuits with full coverage: 0

      ====== Circuit coverage for circuit ======

      total coverage: 0x
      cases: 0x
      always_states: 0x

      ==== Cases coverage ====

      total: 0x
      cases tested: 2
      cases with full coverage: 0

      Cases with id: 3
          always switch
              created at: <elided>
              driving (always variable): o0
          selector matched cases: A
          never matched: B C

      Cases with id: 6
          always switch
              created at: <elided>
              driving (state variable): <elided>
          selector matched cases: A
          never matched: B C

      ==== Always state coverage ====

      total: 0x
      always states tested: 1
      always states with full coverage: 0

      Always state with id: 4
          never saw:
              A -> B
              B -> C
              C -> A
      |}]);
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for circuit ======

    total coverage: 0x
    muxes: 0x
    always_states: 0x

    ==== Mux coverage ====

    total: 0x
    muxes tested: 5
    muxes with full coverage: 0

    Mux with id: 3
        always proc
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    Mux with id: 7
        always proc
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 9
        always switch B
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 10
        always switch A
            created at: <elided>
            driving (state variable): <elided>
        saw selector values of: 0
        never saw: 1

    Mux with id: 16
        always switch A
            created at: <elided>
            driving (always variable): o0
        saw selector values of: 0
        never saw: 1

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        never saw:
            A -> A
            A -> B
            B -> B
            B -> C
            C -> A
            C -> C
    |}]
;;
