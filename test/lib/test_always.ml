open! Import
open! Signal
open! Always

let%expect_test "guarded assignment width mistmatch" =
  require_does_raise [%here] (fun () ->
    let w = Variable.wire ~default:vdd in
    compile [ w <-- zero 2 ]);
  [%expect
    {|
    ("attempt to assign expression to [Always.variable] of different width"
     (guared_variable_width 1)
     (expression_width      2)
     (expression (
       const
       (width 2)
       (value 0b00)))) |}]
;;

let reg_spec = Reg_spec.create () ~clock ~clear

module State = struct
  type t = int [@@deriving compare, sexp_of]

  let all = [ 1; 3; 5 ]
end

let%expect_test "single state State_machine compiles" =
  let module State = struct
    type t = int [@@deriving compare, sexp_of]

    let all = [ 1 ]
  end
  in
  let sm = State_machine.create (module State) reg_spec ~enable in
  require_does_not_raise [%here] (fun () ->
    compile [ sm.switch ~default:[] [ 1, [ sm.set_next 1 ] ] ])
;;

let%expect_test "[Reg.State_machine.create]" =
  let sm () = State_machine.create (module State) reg_spec ~enable in
  let bad_case (state : _ State_machine.t) = state.switch [ 1, []; 2, []; 6, [] ] in
  let bad_next (state : _ State_machine.t) = state.switch [ 1, [ state.set_next 4 ] ] in
  let incomplete ?default (state : _ State_machine.t) =
    state.switch ?default [ 1, []; 3, [] ]
  in
  let repeated (state : _ State_machine.t) = state.switch [ 1, []; 1, [] ] in
  require_does_raise [%here] (fun () -> compile [ bad_case (sm ()) ]);
  [%expect {|
    ("[Always.State_machine.switch] got unknown states" (2 6)) |}];
  require_does_raise [%here] (fun () -> compile [ bad_next (sm ()) ]);
  [%expect {|
    ("[Always.State_machine.set_next] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ incomplete (sm ()) ]);
  [%expect
    {|
    ("[Always.State_machine.switch] without [~default] had unhandled states" (5)) |}];
  require_does_not_raise [%here] (fun () -> compile [ incomplete ~default:[] (sm ()) ]);
  [%expect {| |}];
  require_does_raise [%here] (fun () -> compile [ repeated (sm ()) ]);
  [%expect {|
    ("[Always.State_machine.switch] got repeated state" 1) |}]
;;

let%expect_test "Statemachine.statmachine ~encoding" =
  let sm encoding = State_machine.create (module State) reg_spec ~encoding ~enable in
  let bad_case (state : _ State_machine.t) = state.switch [ 1, []; 2, []; 6, [] ] in
  let bad_next (state : _ State_machine.t) = state.switch [ 1, [ state.set_next 4 ] ] in
  let bad_test (state : _ State_machine.t) = when_ (state.is 4) [] in
  require_does_raise [%here] (fun () -> compile [ bad_case (sm Binary) ]);
  [%expect {|
    ("[Always.State_machine.switch] got unknown states" (2 6)) |}];
  require_does_raise [%here] (fun () -> compile [ bad_next (sm Binary) ]);
  [%expect {|
    ("[Always.State_machine.set_next] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ bad_test (sm Binary) ]);
  [%expect {|
    ("[Always.State_machine.is] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ bad_case (sm Onehot) ]);
  [%expect {|
    ("[Always.State_machine.switch] got unknown states" (2 6)) |}];
  require_does_raise [%here] (fun () -> compile [ bad_next (sm Onehot) ]);
  [%expect {|
    ("[Always.State_machine.set_next] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ bad_test (sm Onehot) ]);
  [%expect {|
    ("[Always.State_machine.is] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ bad_case (sm Gray) ]);
  [%expect {|
    ("[Always.State_machine.switch] got unknown states" (2 6)) |}];
  require_does_raise [%here] (fun () -> compile [ bad_next (sm Gray) ]);
  [%expect {|
    ("[Always.State_machine.set_next] got unknown state" 4) |}];
  require_does_raise [%here] (fun () -> compile [ bad_test (sm Gray) ]);
  [%expect {|
    ("[Always.State_machine.is] got unknown state" 4) |}]
;;

let%expect_test "test statemachine encodings" =
  let module State = struct
    type t =
      | Idle
      | S5
      | S10
      | S15
      | Valid
    [@@deriving compare, enumerate, sexp_of, variants]
  end
  in
  let test ~encoding ~nickel ~dime =
    let state : State.t State_machine.t =
      State_machine.create (module State) reg_spec ~encoding ~enable:vdd
    in
    let decoded =
      Array.init (List.length State.all) ~f:(fun _ -> Variable.wire ~default:gnd)
    in
    let enable_decoded state = decoded.(State.Variants.to_rank state) <--. 1 in
    compile
      [ state.switch
          [ ( Idle
            , [ enable_decoded Idle
              ; when_ nickel [ state.set_next S5 ]
              ; when_ dime [ state.set_next S10 ]
              ] )
          ; ( S5
            , [ enable_decoded S5
              ; when_ nickel [ state.set_next S10 ]
              ; when_ dime [ state.set_next S15 ]
              ] )
          ; ( S10
            , [ enable_decoded S10
              ; when_ nickel [ state.set_next S15 ]
              ; when_ dime [ state.set_next Valid ]
              ] )
          ; ( S15
            , [ enable_decoded S15
              ; when_ nickel [ state.set_next Valid ]
              ; when_ dime [ state.set_next Valid ]
              ] )
          ; Valid, [ enable_decoded Valid; state.set_next Idle ]
          ]
      ];
    let prefix = State_machine.Encoding.to_string encoding |> String.lowercase in
    let states =
      List.map State.all ~f:state.is |> Signal.concat_lsb |> output (prefix ^ "_states")
    in
    let decoded =
      Array.to_list decoded
      |> List.rev
      |> List.map ~f:(fun d -> d.value)
      |> Signal.concat_msb
      |> output (prefix ^ "_decoded")
    in
    let current = state.current |> output (prefix ^ "_current") in
    states, decoded, current
  in
  let nickel, dime = input "nickel" 1, input "dime" 1 in
  let binary_states, binary_decoded, binary_cur = test ~encoding:Binary ~nickel ~dime in
  let onehot_states, onehot_decoded, onehot_cur = test ~encoding:Onehot ~nickel ~dime in
  let gray_states, gray_decoded, gray_cur = test ~encoding:Gray ~nickel ~dime in
  (* Once reset, the states all sequence the same and generated the same decoded output *)
  let ok =
    (* dont care during reset *)
    clear
    (* Same state sequences for all encodings *)
    |: (binary_states
        ==: onehot_states
        &: (binary_states ==: gray_states)
        (* The decoded output should match the derived state *)
        &: (binary_states
            ==: binary_decoded
            &: (onehot_states ==: onehot_decoded)
            &: (gray_states ==: gray_decoded)))
    |> output "ok"
  in
  let run_sim ~verbose coins =
    let circuit =
      Circuit.create_exn
        ~name:"vending_machine"
        (if verbose
         then
           [ binary_states
           ; onehot_states
           ; gray_states
           ; binary_decoded
           ; onehot_decoded
           ; gray_decoded
           ; binary_cur
           ; onehot_cur
           ; gray_cur
           ; ok
           ]
         else [ ok ])
    in
    let sim = Cyclesim.create circuit in
    let waves, sim = Waves.Waveform.create sim in
    let port_nickel, port_dime =
      Cyclesim.in_port sim "nickel", Cyclesim.in_port sim "dime"
    in
    let clr = Cyclesim.in_port sim "clear" in
    let cycle ~nickel ~dime =
      port_nickel := if nickel then Bits.vdd else Bits.gnd;
      port_dime := if dime then Bits.vdd else Bits.gnd;
      Cyclesim.cycle sim;
      port_nickel := Bits.gnd;
      port_dime := Bits.gnd
    in
    clr := Bits.vdd;
    Cyclesim.cycle sim;
    clr := Bits.gnd;
    List.iter coins ~f:(fun (nickel, dime) -> cycle ~nickel ~dime);
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Waves.Waveform.print ~display_height:(if verbose then 39 else 12) ~wave_width:1 waves
  in
  let nickel, dime = (true, false), (false, true) in
  run_sim ~verbose:true [ nickel; nickel; nickel; nickel ];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││────┐                                              │
    │               ││    └───────────────────────                       │
    │dime           ││                                                   │
    │               ││────────────────────────────                       │
    │nickel         ││    ┌───────────────┐                              │
    │               ││────┘               └───────                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │binary_current ││ 0      │1  │2  │3  │4  │0                         │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │binary_decoded ││ 01     │02 │04 │08 │10 │01                        │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │binary_states  ││ 01     │02 │04 │08 │10 │01                        │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │gray_current   ││ 0      │1  │3  │2  │6  │0                         │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │gray_decoded   ││ 01     │02 │04 │08 │10 │01                        │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │               ││────────┬───┬───┬───┬───┬───                       │
    │gray_states    ││ 01     │02 │04 │08 │10 │01                        │
    │               ││────────┴───┴───┴───┴───┴───                       │
    │ok             ││────────────────────────────                       │
    │               ││                                                   │
    │               ││────┬───┬───┬───┬───┬───┬───                       │
    │onehot_current ││ 00 │01 │02 │04 │08 │10 │01                        │
    │               ││────┴───┴───┴───┴───┴───┴───                       │
    │               ││────┬───┬───┬───┬───┬───┬───                       │
    │onehot_decoded ││ 00 │01 │02 │04 │08 │10 │01                        │
    │               ││────┴───┴───┴───┴───┴───┴───                       │
    │               ││────┬───┬───┬───┬───┬───┬───                       │
    │onehot_states  ││ 00 │01 │02 │04 │08 │10 │01                        │
    │               ││────┴───┴───┴───┴───┴───┴───                       │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  run_sim ~verbose:false [ nickel; dime; nickel ];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││────┐                                              │
    │               ││    └───────────────────                           │
    │dime           ││        ┌───┐                                      │
    │               ││────────┘   └───────────                           │
    │nickel         ││    ┌───┐   ┌───┐                                  │
    │               ││────┘   └───┘   └───────                           │
    │ok             ││────────────────────────                           │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  run_sim ~verbose:false [ dime; dime ];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││────┐                                              │
    │               ││    └───────────────                               │
    │dime           ││    ┌───────┐                                      │
    │               ││────┘       └───────                               │
    │nickel         ││                                                   │
    │               ││────────────────────                               │
    │ok             ││────────────────────                               │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}];
  run_sim ~verbose:false [ nickel; nickel; dime ];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││────┐                                              │
    │               ││    └───────────────────                           │
    │dime           ││            ┌───┐                                  │
    │               ││────────────┘   └───────                           │
    │nickel         ││    ┌───────┐                                      │
    │               ││────┘       └───────────                           │
    │ok             ││────────────────────────                           │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
