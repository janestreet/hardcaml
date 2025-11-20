open Base
open Hardcaml
open Signal

(* Take in binary coded decimal numbers and output a binary representation. *)

(* $MDX part-begin=bcd *)
let create ~clock ~start ~bcd =
  let spec = Reg_spec.create ~clock () in
  (* Find the number of digits to convert and create a counter. *)
  let digits = split_msb ~part_width:4 bcd in
  let num_digits = List.length digits in
  (* We only support multiple digits - for a single digit the output equals the input
     anyway. *)
  assert (num_digits > 1);
  let count = Always.Variable.reg spec ~width:(Int.ceil_log2 num_digits) in
  (* Create an accumulator large enough to hold the final result. *)
  let max_result = Int.pow 10 num_digits - 1 in
  let acc = Always.Variable.reg spec ~width:(num_bits_to_represent max_result) in
  (* Select the current digit being processed. Note that when we split [bcd] into [digits]
     we did so from the top most digit. *)
  let digit = mux count.value digits |> uresize ~width:(width acc.value) in
  Always.(
    compile
      [ if_
          (count.value ==:. 0)
          [ (* Wait for start. When applied set the accumulator with the top most digit.
            *)
            when_ start [ count <-- count.value +:. 1; acc <-- digit ]
          ]
          [ (* Add each succesive digit to the accumulator times 10. *)
            count <-- count.value +:. 1
          ; acc <-- drop_top (acc.value *: of_unsigned_int ~width:4 10) ~width:4 +: digit
          ; (* Finished processing digits. *)
            when_ (count.value ==:. num_digits - 1) [ count <--. 0 ]
          ]
      ]);
  acc.value, count.value ==:. 0
;;

(* $MDX part-end *)

(* $MDX part-begin=testbench *)
let bcd_of_string v =
  String.to_list v
  |> List.map ~f:(fun c ->
    Char.to_int c - Char.to_int '0' |> Bits.of_unsigned_int ~width:4)
  |> Bits.concat_msb
;;

let test num_digits =
  (* Create the simulator. *)
  let sim =
    let result, done_ =
      create
        ~clock:(input "clock" 1)
        ~start:(input "start" 1)
        ~bcd:(input "bcd" (num_digits * 4))
    in
    Circuit.create_exn ~name:"bcd" [ output "result" result; output "done" done_ ]
    |> Cyclesim.create
  in
  (* Query the input and output ports. *)
  let start = Cyclesim.in_port sim "start" in
  let bcd = Cyclesim.in_port sim "bcd" in
  let done_ = Cyclesim.out_port sim "done" in
  let result = Cyclesim.out_port sim "result" in
  (* Start the circuit running and wait for it to be done. Return the computed result. *)
  let run v =
    start := Bits.vdd;
    bcd := bcd_of_string v;
    Cyclesim.cycle sim;
    start := Bits.gnd;
    Cyclesim.cycle sim;
    while not (Bits.to_bool !done_) do
      Cyclesim.cycle sim
    done;
    Bits.to_unsigned_int !result
  in
  run
;;

(* $MDX part-end *)

(* $MDX part-begin=tests *)
let%expect_test "" =
  let run = test 5 in
  Stdio.print_s [%message (run "00000" : int)];
  Stdio.print_s [%message (run "12345" : int)];
  Stdio.print_s [%message (run "99999" : int)];
  [%expect
    {|
    ("run \"00000\"" 0)
    ("run \"12345\"" 12345)
    ("run \"99999\"" 99999)
    |}];
  let run = test 2 in
  Stdio.print_s [%message (run "00" : int)];
  Stdio.print_s [%message (run "21" : int)];
  Stdio.print_s [%message (run "55" : int)];
  Stdio.print_s [%message (run "99" : int)];
  [%expect
    {|
    ("run \"00\"" 0)
    ("run \"21\"" 21)
    ("run \"55\"" 55)
    ("run \"99\"" 99)
    |}]
;;
(* $MDX part-end *)
