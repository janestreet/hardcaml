open Core
open Hardcaml
open Hardcaml_waveterm_kernel
open Hardcaml_waveterm_cyclesim
open Signal

module%test [@name "Wave formats applied to signals are correctly rendered"] _ = struct
  module I = struct
    type 'a t = { a : 'a [@bits 4] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { unsigned : 'a [@bits 4]
      ; signed : 'a [@bits 4]
      ; hex : 'a [@bits 4]
      ; binary : 'a [@bits 4]
      ; custom : 'a [@bits 4]
      ; index : 'a [@bits 4]
      ; map : 'a [@bits 4]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let%hw unsigned = wireof i.a --$ Unsigned_int in
    let%hw signed = wireof i.a --$ Int in
    let%hw hex = wireof i.a --$ Hex in
    let%hw binary = wireof i.a --$ Binary in
    let%hw custom = wireof i.a --$ Custom (fun bits -> "b'" ^ Bits.to_string bits) in
    let%hw index =
      wireof i.a --$ Index (List.init 16 ~f:(fun i -> "i'" ^ Int.to_string i))
    in
    let%hw map =
      wireof i.a
      --$ Map
            (List.init 16 ~f:(fun i ->
               Bits.of_unsigned_int ~width:4 i, "m'" ^ Int.to_string i))
    in
    { O.unsigned; signed; hex; binary; custom; index; map }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)

  let%expect_test _ =
    let sim = Sim.create ~config:Cyclesim.Config.trace_all (create (Scope.create ())) in
    let waves, sim = Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    let open Bits in
    for i = 5 to 11 do
      inputs.a <--. i;
      Cyclesim.cycle sim
    done;
    Waveform.print ~display_rules:[ Display_rule.port_name_matches (Posix ".*_0") ] waves;
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │binary_0       ││ 0101   │0110   │0111   │1000   │1001   │1010   │10│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │custom_0       ││ b'0101 │b'0110 │b'0111 │b'1000 │b'1001 │b'1010 │b'│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │hex_0          ││ 5      │6      │7      │8      │9      │A      │B │
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │index_0        ││ i'5    │i'6    │i'7    │i'8    │i'9    │i'10   │i'│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │map_0          ││ m'5    │m'6    │m'7    │m'8    │m'9    │m'10   │m'│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │signed_0       ││ 5      │6      │7      │-8     │-7     │-6     │-5│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
      │unsigned_0     ││ 5      │6      │7      │8      │9      │10     │11│
      │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end

module%test [@name "Propagation of formats through interfaces and %hw specifications"] _ =
struct
  module X = (val Types.value ~wave_format:Int ~name:"x" 4)

  module Y = struct
    type 'a t = { a : 'a [@bits 4] [@wave_format Int] } [@@deriving hardcaml]
  end

  module S = struct
    type t =
      | A
      | B
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; y : 'a Y.t
      }
    [@@deriving hardcaml]

    let%expect_test "" =
      print_s [%message (wave_formats : Wave_format.t t)];
      [%expect {| (wave_formats ((clock (Bit_or Hex)) (y ((a Int))))) |}]
    ;;
  end

  module O = struct
    type 'a t =
      { z : 'a Y.t
      ; n : 'a [@bits 2] [@wave_format Binary]
      }
    [@@deriving hardcaml]

    let%expect_test "" =
      print_s [%message (wave_formats : Wave_format.t t)];
      [%expect {| (wave_formats ((z ((a Int))) (n Binary))) |}]
    ;;
  end

  module Sim = Cyclesim.With_interface (I) (O)

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    let%hw.Always.State_machine sm = Always.State_machine.create (module S) spec in
    assert (width i.y.a = 4);
    let%hw.X.Of_signal foo = ~:(i.y.a) in
    Always.(compile [ sm.switch [ A, [ sm.set_next B ]; B, [ sm.set_next A ] ] ]);
    { O.z = { a = foo }; n = uresize (sm.is A) ~width:2 }
  ;;

  let hierarchy scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"h" ~scope create
  ;;

  let%expect_test _ =
    print_s
      [%message (I.wave_formats : Wave_format.t I.t) (O.wave_formats : Wave_format.t O.t)];
    [%expect
      {|
      ((I.wave_formats ((clock (Bit_or Hex)) (y ((a Int)))))
       (O.wave_formats ((z ((a Int))) (n Binary))))
      |}]
  ;;

  let%expect_test _ =
    let waves, sim =
      Waveform.create
        (Sim.create
           ~config:Cyclesim.Config.trace_all
           (hierarchy
              (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())))
    in
    let inputs = Cyclesim.inputs sim in
    inputs.y.a := Bits.of_string "0101";
    Cyclesim.cycle ~n:3 sim;
    Waveform.print waves;
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────────────────────                           │
      │y$a            ││ 5                                                 │
      │               ││────────────────────────                           │
      │               ││────────┬───────┬───────                           │
      │n              ││ 01     │00     │01                                │
      │               ││────────┴───────┴───────                           │
      │               ││────────────────────────                           │
      │z$a            ││ -6                                                │
      │               ││────────────────────────                           │
      │gnd            ││                                                   │
      │               ││────────────────────────                           │
      │               ││────────────────────────                           │
      │h$foo$x        ││ -6                                                │
      │               ││────────────────────────                           │
      │h$i$clock      ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
      │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
      │               ││────────────────────────                           │
      │h$i$y$a        ││ 5                                                 │
      │               ││────────────────────────                           │
      │               ││────────┬───────┬───────                           │
      │h$o$n          ││ 01     │00     │01                                │
      │               ││────────┴───────┴───────                           │
      │               ││────────────────────────                           │
      │h$o$z$a        ││ -6                                                │
      │               ││────────────────────────                           │
      │               ││────────┬───────┬───────                           │
      │h$sm           ││ A      │B      │A                                 │
      │               ││────────┴───────┴───────                           │
      └───────────────┘└───────────────────────────────────────────────────┘
      |}]
  ;;
end
