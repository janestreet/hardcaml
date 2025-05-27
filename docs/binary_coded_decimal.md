# 4.2.1 BCD Conversion

In the Binary Coded Decimal (BCD) system we represent decimal numbers in a binary format.
The format uses 4 bits per decimal digit to encode the numbers 0 through 9. For example,
the number 971 would be represented in BCD as:

```
  9    7    1
|1001|0111|0001|
```

# Converting to Binary

To convert a BCD number to a binary number we need to do some multiplications by powers
of 10. First we split the input vector into parts of width 4 then compute `9*100 + 7*10 +
1`

We can do something a little more efficient by starting with the top most digit and
computing `(((9*10) + 7) * 10) + 1`

# Implementing in Hardware

<!-- $MDX file=./lib/binary_coded_decimal.ml,part=bcd -->
```ocaml
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
```

We start by figuring out how many digits we need to process and a counter from
`0..num_digits-1` is created. An accumulator is also created wide enough to hold the
largest result value.

> 📝 An assertion is raised if num_digits is equal to 1. This is because the `count`
> register has width `Int.ceil_log2 num_digits = 0` which you aren't allowed to do. It is
> not unusual for otherwise generic hardcaml circuit generators to fall down on the
> smallest example in this way, though it often tends to be a trivial case (here, the
> circuit literally does nothing if there is only 1 digit) and can often be worked around.

While `count` is 0 we are waiting for `start` to be raised. Then, for `num_digits` cycles
we will add each digit (starting from the top most) to `acc * 10`. When `count ==:.
num_digits-1` we have computed the result and set `count` back to 0.

# Testbench

<!-- $MDX file=./lib/binary_coded_decimal.ml,part=testbench -->
```ocaml
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
```

The following steps are performed

- Create the BCD design.
- Create a `Circuit`.
- Create a `Cyclesim` simulator.
- Get the simulator ports.
- Return a function which performs a single test - start the circuit with a BCD value,
  wait for it to complete and return the result.

The `test` function doesn't actually compute a BCD, but it does do all the setup stuff
(which can be time consuming for larger designs). It returns a function that allows us to
run multiple conversions one after the other.

# Tests

<!-- $MDX file=./lib/binary_coded_decimal.ml,part=tests -->
```ocaml
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
```
