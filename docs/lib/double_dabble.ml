open Base
open Hardcaml
open Hardcaml_waveterm
open Signal
open! Stdio

(* $MDX part-begin=sw *)
let double_dabble value =
  let digits = String.length (Int.to_string value) in
  let num_bits = Int.ceil_log2 (value + 1) in
  let value = ref value in
  for _ = 0 to num_bits - 1 do
    for j = 0 to digits - 1 do
      let digit = (!value lsr (num_bits + (j * 4))) land 0xf in
      if digit >= 5 then value := !value + (3 lsl (num_bits + (j * 4)))
    done;
    value := !value lsl 1
  done;
  let bcd = !value lsr num_bits in
  List.init digits ~f:(fun i -> (bcd lsr (i * 4)) land 0xf)
  |> List.rev
  |> List.map ~f:(fun i -> Char.of_int_exn (i + Char.to_int '0'))
  |> String.of_char_list
;;

let%expect_test "Test algorithm" =
  for i = 0 to 1000 do
    assert (String.equal (Int.to_string i) (double_dabble i))
  done;
  [%expect {| |}]
;;

(* $MDX part-end *)

(* $MDX part-begin=hw *)
module Make (Digits : sig
    val num_digits : int
  end) =
struct
  open Digits

  (* Bits required to represent [999...999]. *)
  let binary_bits = num_bits_to_represent (Int.pow 10 num_digits - 1)

  module State = struct
    type t =
      | Start
      | Double
      | Dabble
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ~clock ~start ~binary_in =
    let spec = Reg_spec.create ~clock () in
    (* Registers to latch the input binary value and count through it's bits. *)
    let binary = Always.Variable.reg spec ~width:binary_bits in
    let bit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 binary_bits) in
    (* Register to count through digits while dabbling. *)
    let digit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 num_digits) in
    (* One digit count to use as a register write enable for the BCD digits. *)
    let digit_count_one_hot = binary_to_onehot digit_count.value in
    (* Registers for the BCD digit. *)
    let bcd = Array.init num_digits ~f:(fun _ -> Always.Variable.reg spec ~width:4) in
    (* Dabbling logic - look up the current BCD digit and perform dabble operation if
       greater than 4. *)
    let bcd_dabbled =
      let digit =
        mux digit_count.value (List.map (Array.to_list bcd) ~f:(fun bcd -> bcd.value))
      in
      mux2 (digit >:. 4) (digit +:. 3) digit
    in
    (* Statemachine *)
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ (* Wait for start. *)
                  bit_count <--. 0
                ; binary <-- binary_in
                ; proc (List.init num_digits ~f:(fun digit -> bcd.(digit) <--. 0))
                ; when_ start [ sm.set_next Double ]
                ] )
            ; ( Double
              , [ (* Shift in the next binary bit through all the BCD registers. *)
                  binary <-- sll binary.value ~by:1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       bcd.(digit)
                       <-- lsbs bcd.(digit).value
                           @:
                           if digit = 0
                           then msb binary.value
                           else msb bcd.(digit - 1).value))
                ; digit_count <--. 0
                ; bit_count <-- bit_count.value +:. 1
                ; (* Count through all the input binary bits. *)
                  if_
                    (bit_count.value ==:. binary_bits - 1)
                    [ sm.set_next Start ]
                    [ sm.set_next Dabble ]
                ] )
            ; ( Dabble
              , [ (* Iterate through each digit and perform the dabble operation. *)
                  digit_count <-- digit_count.value +:. 1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       when_ digit_count_one_hot.:(digit) [ bcd.(digit) <-- bcd_dabbled ]))
                ; when_ (digit_count.value ==:. num_digits - 1) [ sm.set_next Double ]
                ] )
            ]
        ]);
    sm.is Start, Array.map bcd ~f:(fun bcd -> bcd.value)
  ;;
end
(* $MDX part-end *)

(* $MDX part-begin=boilerplate *)
module Make_sim (Digits : sig
    val num_digits : int
  end) =
struct
  open Digits
  include Make (Digits)

  let circuit () =
    let done_, bcd =
      create
        ~clock:(input "clock" 1)
        ~start:(input "start" 1)
        ~binary_in:(input "binary" binary_bits)
    in
    Circuit.create_exn
      ~name:"double_dabble"
      (output "done" done_
       :: Array.to_list (Array.mapi bcd ~f:(fun i -> output [%string "bcd%{i#Int}"])))
  ;;

  type ports =
    { start : Bits.t ref
    ; binary_in : Bits.t ref
    ; done_ : Bits.t ref
    ; bcd : Bits.t ref array
    }

  let sim () =
    let sim = Cyclesim.create (circuit ()) in
    ( sim
    , { start = Cyclesim.in_port sim "start"
      ; binary_in = Cyclesim.in_port sim "binary"
      ; done_ = Cyclesim.out_port sim "done"
      ; bcd =
          Array.init num_digits ~f:(fun i ->
            Cyclesim.out_port sim [%string "bcd%{i#Int}"])
      } )
  ;;
end
(* $MDX part-end *)

let%expect_test "Debug waveform" =
  let module Bcd =
    Make_sim (struct
      let num_digits = 2
    end)
  in
  let open Bits in
  let sim, ports = Bcd.sim () in
  let waves, sim = Waveform.create sim in
  ports.start := Bits.vdd;
  ports.binary_in <--. 99;
  Cyclesim.cycle sim;
  ports.start := Bits.gnd;
  while not (Bits.to_bool !(ports.done_)) do
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim;
  Waveform.expect_exact waves ~wave_width:1 ~display_width:110;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
│clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│                  ││────────────────────────────────────────────────────────────────────────────────────    │
│binary            ││ 63                                                                                     │
│                  ││────────────────────────────────────────────────────────────────────────────────────    │
│start             ││────┐                                                                                   │
│                  ││    └───────────────────────────────────────────────────────────────────────────────    │
│                  ││────────┬───────────┬───────────┬───┬───────┬───────────┬───────────┬───┬───────┬───    │
│bcd0              ││ 0      │1          │3          │6  │9      │2          │4          │9  │C      │9      │
│                  ││────────┴───────────┴───────────┴───┴───────┴───────────┴───────────┴───┴───────┴───    │
│                  ││────────────────────────────────────────────┬───────────┬───────────┬───────────┬───    │
│bcd1              ││ 0                                          │1          │2          │4          │9      │
│                  ││────────────────────────────────────────────┴───────────┴───────────┴───────────┴───    │
│done              ││────┐                                                                           ┌───    │
│                  ││    └───────────────────────────────────────────────────────────────────────────┘       │
└──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘
1aad590737b42e17e27724cd63f61511
|}]
;;

(* $MDX part-begin=testing *)
let create_test num_digits =
  let module Bcd =
    Make_sim (struct
      let num_digits = num_digits
    end)
  in
  let open Bits in
  let sim, ports = Bcd.sim () in
  Cyclesim.cycle sim;
  let test value =
    (* Start the statemachine. *)
    ports.start := Bits.vdd;
    ports.binary_in <--. value;
    Cyclesim.cycle sim;
    ports.start := Bits.gnd;
    (* Wait for it to complete. *)
    while not (Bits.to_bool !(ports.done_)) do
      Cyclesim.cycle sim
    done;
    (* Convert the BCD output to a string. *)
    Array.map ports.bcd ~f:(fun bcd ->
      Char.of_int_exn (Bits.to_unsigned_int !bcd + Char.to_int '0'))
    |> Array.to_list
    |> List.rev
    |> String.of_list
  in
  test
;;

let%expect_test "2 digits" =
  let num_digits = 2 in
  let test = create_test num_digits in
  for i = 0 to Int.pow 10 num_digits - 1 do
    assert (Int.of_string (test i) = i)
  done;
  [%expect {| |}]
;;

let%expect_test "3 digits" =
  let num_digits = 3 in
  let test = create_test num_digits in
  for i = 0 to Int.pow 10 num_digits - 1 do
    assert (Int.of_string (test i) = i)
  done;
  [%expect {| |}]
;;

let%expect_test "4 digits" =
  let num_digits = 4 in
  let test = create_test num_digits in
  for i = 0 to Int.pow 10 num_digits - 1 do
    assert (Int.of_string (test i) = i)
  done;
  [%expect {| |}]
;;
(* $MDX part-end *)
