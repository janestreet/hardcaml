open Core
open Hardcaml
open Hardcaml_waveterm_kernel
open Hardcaml_waveterm_cyclesim
open Signal

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

let%expect_test "wave formats" =
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (create (Scope.create ())) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let open Bits in
  for i = 5 to 11 do
    inputs.a <--. i;
    Cyclesim.cycle sim
  done;
  Waveform.print
    ~display_rules:
      [ Display_rule.port_name_matches (Re.Posix.compile (Re.Posix.re ".*_0")) ]
    waves;
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
