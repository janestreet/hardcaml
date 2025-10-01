open Core
open Hardcaml
open Signal
open Coverage_prim

let set_bits bits value = Bits.( <--. ) bits value
let const i ~width = of_int_trunc ~width i

let%expect_test "join waiver base" =
  let test ts ~f =
    List.concat_map ts ~f:(fun a -> List.map ts ~f:(fun b -> f a b))
    |> List.dedup_and_sort ~compare:String.compare
    |> List.iter ~f:print_endline
  in
  let test_kind ts =
    test ts ~f:(fun a b ->
      let to_string a = Waiver.to_string a ~f:Int.to_string in
      let c = Waiver.join a b ~equal:Int.equal in
      [%string {|%{to_string a} + %{to_string b} = %{to_string c}|}])
  in
  let test_warn ts =
    test ts ~f:(fun a b ->
      let to_string a =
        if Waiver.warn_on_waived_but_observed a then "warn" else "dont-warn"
      in
      let c = Waiver.join a b ~equal:Int.equal in
      [%string {|%{to_string a} + %{to_string b} = %{to_string c}|}])
  in
  let ts : int Waiver.t list =
    List.concat_map Bool.all ~f:(fun warn_on_waived_but_observed ->
      [ Waiver.none ()
      ; Waiver.only_expect ~warn_on_waived_but_observed [ 1; 2; 3 ]
      ; Waiver.exclude ~warn_on_waived_but_observed [ 2; 3 ]
      ])
  in
  test_kind ts;
  [%expect
    {|
    exclude (2 3) + exclude (2 3) = exclude (2 3 2 3)
    exclude (2 3) + none = exclude (2 3)
    exclude (2 3) + only (1 2 3) = only (1)
    none + exclude (2 3) = exclude (2 3)
    none + none = none
    none + only (1 2 3) = only (1 2 3)
    only (1 2 3) + exclude (2 3) = only (1)
    only (1 2 3) + none = only (1 2 3)
    only (1 2 3) + only (1 2 3) = only (1 2 3 1 2 3)
    |}];
  test_warn ts;
  [%expect
    {|
    dont-warn + dont-warn = dont-warn
    dont-warn + warn = warn
    warn + dont-warn = warn
    warn + warn = warn
    |}]
;;

let test ?(debug = false) ~width ~build_output ~run () =
  Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
  let scope = Scope.create () in
  let clock = Signal.input "clock" 1 in
  let input = input "input" width in
  let output_signal = build_output scope clock input in
  let output = output "output" output_signal in
  let circuit = Circuit.create_exn ~name:"Test_circuit" [ output ] in
  if debug
  then
    Circuit.signal_graph circuit
    |> Signal_graph.iter ~f:(fun s -> print_s [%message (s : Signal.t)]);
  let sim = Cyclesim.create circuit in
  let input = Cyclesim.in_port sim "input" in
  run sim input;
  Cyclesim_coverage_expect_test.output_results ()
;;

let run_reg_test ?waiver ~width ~values () =
  test
    ~width
    ~build_output:(fun scope clock input ->
      let%hw test_reg = reg (Reg_spec.create ~clock ()) input in
      Option.value_map waiver ~default:test_reg ~f:(fun waiver ->
        Signal.add_register_waiver_exn test_reg waiver))
    ~run:(fun sim input ->
      Cyclesim.cycle sim;
      List.iter values ~f:(fun value ->
        (match value with
         | `Bit_on bit -> set_bits input (1 lsl bit)
         | `All_off -> set_bits input 0);
        Cyclesim.cycle ~n:2 sim))
    ()
;;

let%expect_test "reg" =
  let width = 5 in
  let do_toggle bits = List.concat_map bits ~f:(fun bit -> [ `Bit_on bit; `All_off ]) in
  let toggle bits =
    List.concat_map bits ~f:(fun bit ->
      List.map Bool.all ~f:(fun on -> { Toggle.on; bit }))
  in
  run_reg_test
    ~width
    ~values:(do_toggle [ 1; 3; 5 ])
    ~waiver:(Waiver.exclude (toggle [ 0; 2; 4 ]))
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}];
  run_reg_test
    ~width
    ~values:(do_toggle [ 1; 3; 5 ])
    ~waiver:(Waiver.only_expect (toggle [ 1; 3; 5 ]))
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "reg - unexpected" =
  let width = 5 in
  run_reg_test
    ~width
    ~values:[ `Bit_on 0; `All_off; `Bit_on 1 ]
    ~waiver:(Waiver.only_expect [ { Toggle.on = true; bit = 0 } ])
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1

    ====== Circuit coverage for Test_circuit ======

    total coverage: 1x
    regs: 1x

    ==== Reg coverage ====

    total: 1x
    regs tested: 1
    regs with full coverage: 1

    Reg with id: 2
        names: test_reg
        width: 5
        bits toggled ON: 0
        !! unexpectedly toggled ON: 1
        !! unexpectedly toggled OFF: 0
    |}]
;;

let%expect_test "reg - don't warn on waived by observed" =
  let width = 5 in
  run_reg_test
    ~width
    ~values:[ `Bit_on 0; `All_off; `Bit_on 1 ]
    ~waiver:
      (Waiver.only_expect
         ~warn_on_waived_but_observed:false
         [ { Toggle.on = true; bit = 0 } ])
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "mux - joined waivers" =
  let width = 3 in
  let const i = const ~width i in
  test
    ~width
    ~build_output:(fun scope _clock input ->
      let%hw mux = mux input [ const 0; const 1; const 2 ] in
      let mux = Signal.add_mux_waiver_exn mux (Waiver.only_expect [ 0; 1 ]) in
      Signal.add_mux_waiver_exn mux (Waiver.exclude [ 1 ]))
    ~run:(fun sim _input -> Cyclesim.cycle sim)
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "cases" =
  let width = 4 in
  let const i = const ~width i in
  test
    ~width
    ~build_output:(fun scope _clock input ->
      let%hw cases =
        Signal.cases
          input
          ~default:(const 4)
          [ const 2, const 0; const 1, const 1; const 0, const 1 ]
      in
      Signal.add_cases_waiver_exn cases (Waiver.exclude [ Case.Default; Positional 2 ]))
    ~run:(fun sim input ->
      List.iter [ 1; 2 ] ~f:(fun v ->
        set_bits input v;
        Cyclesim.cycle sim))
    ();
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

module State = struct
  type t =
    | A
    | B
    | C
  [@@deriving compare ~localize, enumerate, sexp_of, variants]
end

let create_sm reg_spec encoding =
  Always.State_machine.create (module State) reg_spec ~encoding ~enable:vdd
;;

let test_switch encoding ~add_waiver =
  let open Always in
  let width = 1 in
  test
    ~width
    ~build_output:(fun scope clock input ->
      let reg_spec = Reg_spec.create ~clock () in
      let out_var = Variable.wire ~default:gnd () in
      let%hw out = out_var.value in
      let sm = create_sm reg_spec encoding in
      add_waiver sm.current;
      compile
        [ sm.switch
            [ A, [ out_var <-- input; sm.set_next B ]
            ; B, [ out_var <--. 2; sm.set_next C ]
            ; C, [ out_var <--. 3; sm.set_next A ]
            ]
        ];
      out)
    ~run:(fun sim _input -> Cyclesim.cycle sim)
    ()
;;

let%expect_test "cases - switch - state " =
  test_switch Binary ~add_waiver:(fun r ->
    ignore (Signal.add_always_state_waiver_exn r (Waiver.exclude [ "B"; "C" ]) : Signal.t));
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "cases - switch - unknown state" =
  Expect_test_helpers_base.require_does_raise (fun () ->
    test_switch Binary ~add_waiver:(fun r ->
      ignore (Signal.add_always_state_waiver_exn r (Waiver.exclude [ "Z" ]) : Signal.t)));
  [%expect
    {|
    ("Waiver for unknown state case"
      (call_stack   <elided-in-tests>)
      (signal_names <elided-in-tests>)
      (state        Z)
      (states (A B C)))
    |}]
;;

let%expect_test "cases - switch - transition - unknown state" =
  Expect_test_helpers_base.require_does_raise (fun () ->
    test_switch Binary ~add_waiver:(fun r ->
      ignore
        (Signal.add_always_state_transition_waiver_exn
           r
           (Waiver.only_expect
              [ { Transition.from = "A"; to_ = "Z" }; { from = "Z"; to_ = "X" } ])
         : Signal.t)));
  [%expect
    {|
    ("Waiver for unknown state case"
      (call_stack   <elided-in-tests>)
      (signal_names <elided-in-tests>)
      (state        Z)
      (states (A B C)))
    |}]
;;

let%expect_test "mux - switch" =
  (* For now we don't support propagating state waivers through muxes generated by a
     Onehot encoded state switch. If Onehot starts getting more use and this becomes a
     issue we can add it. *)
  test_switch Onehot ~add_waiver:(fun r ->
    ignore (Signal.add_always_state_waiver_exn r (Waiver.exclude [ "B"; "C" ]) : Signal.t));
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Test_circuit ======

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
            driving (always variable): out
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
            driving (always variable): out
        saw selector values of: 0
        never saw: 1

    Mux with id: 17
        always switch A
            created at: <elided>
            driving (always variable): out
        saw selector values of: 0
        never saw: 1

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        never saw:
            A -> A
    |}]
;;

let waiver_warning_re = lazy (Re.Pcre.re "Ignoring waiver" |> Re.compile)

let%expect_test "ignored waivers" =
  let width = 3 in
  let const i = const ~width i in
  let test_single (signal, waiver) =
    test
      ~width
      ~build_output:(fun _scope clock input ->
        Caller_id.set_mode Disabled;
        let out = signal ~clock input in
        waiver out)
      ~run:(fun _sim _input -> ())
      ();
    Expect_test_helpers_base.expect_test_output ()
    |> String.split_lines
    |> List.filter ~f:(Re.execp (force waiver_warning_re))
    |> List.iter ~f:print_endline
  in
  let test_pass tests = List.iter tests ~f:test_single in
  let test_fail tests =
    List.iter tests ~f:(fun t ->
      Expect_test_helpers_base.require_does_raise (fun () -> test_single t))
  in
  let reg ~clock input = reg (Reg_spec.create ~clock ()) input in
  let state_reg ~clock input =
    let open Always in
    let sm = create_sm (Reg_spec.create ~clock ()) Binary in
    compile
      [ sm.switch
          [ A, [ if_ (Signal.all_bits_set input) [ sm.set_next B ] [] ]
          ; B, [ sm.set_next C ]
          ; C, [ sm.set_next A ]
          ]
      ];
    sm.current
  in
  let mux ~clock:_ input = mux input [ const 0; const 1; const 2 ] in
  let cases ~clock:_ input =
    Signal.cases
      input
      ~default:(const 4)
      [ const 2, const 0; const 1, const 1; const 0, const 1 ]
  in
  let mux_waiver s = Signal.add_mux_waiver_exn s (Waiver.only_expect [ 1; 2 ]) in
  let case_waiver s ~named =
    let waiver : Case.t Waiver.t =
      if named
      then Waiver.exclude [ Case.State { name = "Some_case"; value = 0 } ]
      else Waiver.exclude [ Case.Positional 1 ]
    in
    Signal.add_cases_waiver_exn s waiver
  in
  let toggle_waiver s =
    Signal.add_register_waiver_exn
      s
      (Waiver.only_expect [ { Toggle.bit = 1; on = true } ])
  in
  let state_waiver s =
    Signal.add_always_state_waiver_exn s (Waiver.only_expect [ "A" ])
  in
  let transition_waiver s =
    Signal.add_always_state_transition_waiver_exn
      s
      (Waiver.exclude [ { Transition.from = "A"; to_ = "B" } ])
  in
  let none_waiver s ~kind =
    match kind with
    | `Mux -> Signal.add_mux_waiver_exn s (Waiver.none ())
    | `Cases -> Signal.add_cases_waiver_exn s (Waiver.none ())
    | `Reg -> Signal.add_register_waiver_exn s (Waiver.none ())
    | `State -> Signal.add_always_state_waiver_exn s (Waiver.none ())
  in
  (* Reg *)
  test_pass [ reg, toggle_waiver; reg, none_waiver ~kind:`Reg ];
  [%expect {| |}];
  test_fail
    [ reg, mux_waiver
    ; reg, case_waiver ~named:false
    ; reg, state_waiver
    ; reg, transition_waiver
    ];
  [%expect
    {|
    ("Trying to add a mux waiver to a signal that isn't a mux"
     (t (
       register
       (width 3)
       ((clock      clock)
        (clock_edge Rising))
       (data_in input))))
    ("Trying to add a cases waiver to a signal that isn't a cases"
     (t (
       register
       (width 3)
       ((clock      clock)
        (clock_edge Rising))
       (data_in input))))
    "Trying to add an always state waiver to a plain register. Use [add_register_waiver_exn]"
    "Trying to add a always state waiver to plain register. Use [add_register_waiver_exn]"
    |}];
  (* State reg *)
  test_pass
    [ state_reg, state_waiver
    ; state_reg, transition_waiver
    ; state_reg, none_waiver ~kind:`State
    ];
  [%expect {| |}];
  test_fail
    [ state_reg, mux_waiver
    ; state_reg, case_waiver ~named:false
    ; state_reg, toggle_waiver
    ];
  [%expect
    {|
    ("Trying to add a mux waiver to a signal that isn't a mux"
     (t (
       register
       (width 2)
       ((clock      clock)
        (clock_edge Rising))
       (data_in wire))))
    ("Trying to add a cases waiver to a signal that isn't a cases"
     (t (
       register
       (width 2)
       ((clock      clock)
        (clock_edge Rising))
       (data_in wire))))
    "Trying to add a toggle waiver to an always state register. Use [add_always_state_waiver_exn]"
    |}];
  (* Mux *)
  test_pass [ mux, mux_waiver; mux, none_waiver ~kind:`Mux ];
  [%expect {| |}];
  test_fail
    [ mux, case_waiver ~named:false
    ; mux, toggle_waiver
    ; mux, state_waiver
    ; mux, transition_waiver
    ];
  [%expect
    {|
    ("Trying to add a cases waiver to a signal that isn't a cases"
     (t (
       mux
       (width  3)
       (select input)
       (data (0b000 0b001 0b010)))))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       mux
       (width  3)
       (select input)
       (data (0b000 0b001 0b010)))))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       mux
       (width  3)
       (select input)
       (data (0b000 0b001 0b010)))))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       mux
       (width  3)
       (select input)
       (data (0b000 0b001 0b010)))))
    |}];
  (* Cases *)
  test_pass [ cases, case_waiver ~named:false; cases, none_waiver ~kind:`Cases ];
  [%expect {| |}];
  test_fail
    [ cases, mux_waiver
    ; cases, toggle_waiver
    ; cases, case_waiver ~named:true
    ; cases, state_waiver
    ; cases, transition_waiver
    ];
  [%expect
    {|
    ("Trying to add a mux waiver to a signal that isn't a mux"
     (t (
       cases
       (width  3)
       (select input)
       (cases (
         (0b010 0b000)
         (0b001 0b001)
         (0b000 0b001)))
       (default 0b100))))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       cases
       (width  3)
       (select input)
       (cases (
         (0b010 0b000)
         (0b001 0b001)
         (0b000 0b001)))
       (default 0b100))))
    ("Waiver for unknown state case"
      (signal            <elided-in-tests>)
      (waived_state.name Some_case)
      (states ()))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       cases
       (width  3)
       (select input)
       (cases (
         (0b010 0b000)
         (0b001 0b001)
         (0b000 0b001)))
       (default 0b100))))
    ("Trying to add a register waiver to a signal that isn't a register"
     (t (
       cases
       (width  3)
       (select input)
       (cases (
         (0b010 0b000)
         (0b001 0b001)
         (0b000 0b001)))
       (default 0b100))))
    |}]
;;
