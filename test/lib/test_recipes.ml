open Base
open Hardcaml
open Signal
open Recipe
open Let_syntax

module Test_mult = struct
  module I = struct
    type 'a t =
      { clock : 'a [@bits 1]
      ; start : 'a [@bits 1]
      ; a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fin : 'a [@bits 1]
      ; mult : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    module T = struct
      type 'a t =
        { a : 'a [@bits 8]
        ; b : 'a [@bits 8]
        ; acc : 'a [@bits 8]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include T
    module Same = Same (T)
  end

  (* shift 'a' up and 'b' down while 'b' does not equal zero.
   * Add 'b' to the accumulator when the lsb of 'b' is non-zero. *)
  let step s =
    let open State in
    { a = sll s.a 1; b = srl s.b 1; acc = mux2 (lsb s.b) (s.acc +: s.a) s.acc }
  ;;

  let mult a_in b_in =
    let%bind state = State.Same.new_var () in
    let%bind () =
      State.Same.apply ~f:(fun _ -> { a = a_in; b = b_in; acc = zero 8 }) state
    in
    (* serial multiplier *)
    let%bind () =
      State.Same.while_
        (fun state -> state.b <>:. 0)
        state
        ~do_:(State.Same.apply ~f:step state)
    in
    (* return output *)
    SVar.read state.acc
  ;;

  let create (i : _ I.t) =
    let fin, mult = follow ~clock:i.clock ~enable:vdd i.start @@ mult i.a i.b in
    O.{ fin; mult }
  ;;

  let%expect_test "Waveform from multiplying two numbers" =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create ~is_internal_port:(Base.Fn.const true) create in
    let waves, sim = Hardcaml_waveterm.Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    inputs.a := Bits.of_int ~width:8 11;
    inputs.b := Bits.of_int ~width:8 16;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    for _ = 0 to 10 do
      Cyclesim.cycle sim
    done;
    Hardcaml_waveterm.Waveform.print ~display_height:40 ~display_width:80 waves;
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
      │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
      │                  ││──────────────────────────────────────────────────────────│
      │a                 ││ 0B                                                       │
      │                  ││──────────────────────────────────────────────────────────│
      │                  ││──────────────────────────────────────────────────────────│
      │b                 ││ 10                                                       │
      │                  ││──────────────────────────────────────────────────────────│
      │start             ││────────┐                                                 │
      │                  ││        └─────────────────────────────────────────────────│
      │fin               ││                                                ┌───────┐ │
      │                  ││────────────────────────────────────────────────┘       └─│
      │                  ││────────────────────────────────────────────────┬─────────│
      │mult              ││ 00                                             │B0       │
      │                  ││────────────────────────────────────────────────┴─────────│
      │gnd               ││                                                          │
      │                  ││──────────────────────────────────────────────────────────│
      │iter_fin          ││                                                ┌───────┐ │
      │                  ││────────────────────────────────────────────────┘       └─│
      │iter_ready        ││        ┌───────────────────────────────────────────────┐ │
      │                  ││────────┘                                               └─│
      │iter_start        ││        ┌───────────────────────────────────────┐         │
      │                  ││────────┘                                       └─────────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬─────────│
      │new_var_a         ││ 00     │0B     │16     │2C     │58     │B0     │60       │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴─────────│
      │                  ││────────────────────────────────────────────────┬─────────│
      │new_var_acc       ││ 00                                             │B0       │
      │                  ││────────────────────────────────────────────────┴─────────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬─────────│
      │new_var_b         ││ 00     │10     │08     │04     │02     │01     │00       │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴─────────│
      │vdd               ││──────────────────────────────────────────────────────────│
      │                  ││                                                          │
      │                  ││                                                          │
      │                  ││                                                          │
      │                  ││                                                          │
      │                  ││                                                          │
      └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]
  ;;
end
