(* There is a bug where waveforms are printed differently when the simulator is in [Mutable]
   mode compared to [Immutable] mode.

   The data computed [Before] and [After] the clock is correct, however. *)
open! Import

module I = struct
  type 'a t = { a : 'a[@bits 8] }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { b : 'a[@bits 8]
    ; c : 'a[@bits 8]
    ; d : 'a[@bits 8] }
  [@@deriving sexp_of, hardcaml]
end

let create_fn (i : _ I.t) =
  let open Signal in
  let spec = Reg_spec.create () ~clock:(input "clock" 1)  in
  { O.
    b = i.a
  ; c = reg spec ~enable:vdd i.a
  ; d = (reg spec ~enable:vdd i.a) +:. 1}

module Simulator = Cyclesim.With_interface (I) (O)

let%expect_test "immutable waveforms" =
  let sim = Simulator.create ~kind:Immutable create_fn in
  let waves, sim = Waves.Waveform.create sim in
  let i = Cyclesim.inputs sim in
  let o_before = Cyclesim.outputs ~clock_edge:Before sim in
  let o_after = Cyclesim.outputs ~clock_edge:After sim in
  for j=0 to 9 do
    i.a := Bits.consti ~width:8 j;
    Cyclesim.cycle sim;
    print_s [%message (o_before : Bits.t ref O.t) (o_after : Bits.t ref O.t)]
  done;
  Waves.Waveform.print waves;
  [%expect {|
    ((o_before (
       (b 00000000)
       (c 00000000)
       (d 00000001)))
     (o_after (
       (b 00000000)
       (c 00000000)
       (d 00000001))))
    ((o_before (
       (b 00000001)
       (c 00000000)
       (d 00000001)))
     (o_after (
       (b 00000001)
       (c 00000001)
       (d 00000010))))
    ((o_before (
       (b 00000010)
       (c 00000001)
       (d 00000010)))
     (o_after (
       (b 00000010)
       (c 00000010)
       (d 00000011))))
    ((o_before (
       (b 00000011)
       (c 00000010)
       (d 00000011)))
     (o_after (
       (b 00000011)
       (c 00000011)
       (d 00000100))))
    ((o_before (
       (b 00000100)
       (c 00000011)
       (d 00000100)))
     (o_after (
       (b 00000100)
       (c 00000100)
       (d 00000101))))
    ((o_before (
       (b 00000101)
       (c 00000100)
       (d 00000101)))
     (o_after (
       (b 00000101)
       (c 00000101)
       (d 00000110))))
    ((o_before (
       (b 00000110)
       (c 00000101)
       (d 00000110)))
     (o_after (
       (b 00000110)
       (c 00000110)
       (d 00000111))))
    ((o_before (
       (b 00000111)
       (c 00000110)
       (d 00000111)))
     (o_after (
       (b 00000111)
       (c 00000111)
       (d 00001000))))
    ((o_before (
       (b 00001000)
       (c 00000111)
       (d 00001000)))
     (o_after (
       (b 00001000)
       (c 00001000)
       (d 00001001))))
    ((o_before (
       (b 00001001)
       (c 00001000)
       (d 00001001)))
     (o_after (
       (b 00001001)
       (c 00001001)
       (d 00001010))))
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
    │a              ││ 00     │01     │02     │03     │04     │05     │06│
    │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
    │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
    │b              ││ 00     │01     │02     │03     │04     │05     │06│
    │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
    │               ││────────────────┬───────┬───────┬───────┬───────┬──│
    │c              ││ 00             │01     │02     │03     │04     │05│
    │               ││────────────────┴───────┴───────┴───────┴───────┴──│
    │               ││────────────────┬───────┬───────┬───────┬───────┬──│
    │d              ││ 01             │02     │03     │04     │05     │06│
    │               ││────────────────┴───────┴───────┴───────┴───────┴──│
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "immutable waveforms" =
  let sim = Simulator.create ~kind:Mutable create_fn in
  let waves, sim = Waves.Waveform.create sim in
  let i = Cyclesim.inputs sim in
  let o_before = Cyclesim.outputs ~clock_edge:Before sim in
  let o_after = Cyclesim.outputs ~clock_edge:After sim in
  for j=0 to 9 do
    i.a := Bits.consti ~width:8 j;
    Cyclesim.cycle sim;
    print_s [%message (o_before : Bits.t ref O.t) (o_after : Bits.t ref O.t)]
  done;
  Waves.Waveform.print waves;
  [%expect {|
    ((o_before (
       (b 00000000)
       (c 00000000)
       (d 00000001)))
     (o_after (
       (b 00000000)
       (c 00000000)
       (d 00000001))))
    ((o_before (
       (b 00000001)
       (c 00000000)
       (d 00000001)))
     (o_after (
       (b 00000001)
       (c 00000001)
       (d 00000010))))
    ((o_before (
       (b 00000010)
       (c 00000001)
       (d 00000010)))
     (o_after (
       (b 00000010)
       (c 00000010)
       (d 00000011))))
    ((o_before (
       (b 00000011)
       (c 00000010)
       (d 00000011)))
     (o_after (
       (b 00000011)
       (c 00000011)
       (d 00000100))))
    ((o_before (
       (b 00000100)
       (c 00000011)
       (d 00000100)))
     (o_after (
       (b 00000100)
       (c 00000100)
       (d 00000101))))
    ((o_before (
       (b 00000101)
       (c 00000100)
       (d 00000101)))
     (o_after (
       (b 00000101)
       (c 00000101)
       (d 00000110))))
    ((o_before (
       (b 00000110)
       (c 00000101)
       (d 00000110)))
     (o_after (
       (b 00000110)
       (c 00000110)
       (d 00000111))))
    ((o_before (
       (b 00000111)
       (c 00000110)
       (d 00000111)))
     (o_after (
       (b 00000111)
       (c 00000111)
       (d 00001000))))
    ((o_before (
       (b 00001000)
       (c 00000111)
       (d 00001000)))
     (o_after (
       (b 00001000)
       (c 00001000)
       (d 00001001))))
    ((o_before (
       (b 00001001)
       (c 00001000)
       (d 00001001)))
     (o_after (
       (b 00001001)
       (c 00001001)
       (d 00001010))))
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
    │a              ││ 00     │01     │02     │03     │04     │05     │06│
    │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
    │               ││────────┬───────┬───────┬───────┬───────┬───────┬──│
    │b              ││ 00     │01     │02     │03     │04     │05     │06│
    │               ││────────┴───────┴───────┴───────┴───────┴───────┴──│
    │               ││────────────────┬───────┬───────┬───────┬───────┬──│
    │c              ││ 00             │01     │02     │03     │04     │05│
    │               ││────────────────┴───────┴───────┴───────┴───────┴──│
    │               ││────────────────┬───────┬───────┬───────┬───────┬──│
    │d              ││ 01             │02     │03     │04     │05     │06│
    │               ││────────────────┴───────┴───────┴───────┴───────┴──│
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
