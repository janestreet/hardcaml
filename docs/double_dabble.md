# 4.3.2 Double Dabble

We previously saw how to convert a BCD value to binary using a simple multiply by 10
scheme. We'd now like to convert the other way - from binary to BCD.

Conceptually this is a bit harder to achieve - we could do division by 10 with remainder
to calculate the result, but we tend to shy away from dividers where possible in hardware
(to be fair an iterative divider would probably work). It turns out there is a clever way
of doing this called the Double Dabble algorithm (no, really!).

# Double, then Dabble

The idea is we form a large shift register with the BCD values at the MSB and the binary value at the LSB.

```
[BCDN]...[BCD1][BCD0][...BINARY..]
```

We shift the whole thing left by one bit (this is the double part), then run through each
of the 4 bit BCD values and if it is greater then 4, add 3. Repeat for all the bits in the
input binary value.

One way to think of this is we are bypassing the carry logic of a normal binary number and
adding a special value which, upon doubling, will correctly overflow into the next BCD
digit.

The algorithm is shown below - while the idea is not complicated there is a bunch of
annoying bit fiddling going on in order to pack and then extract the appropriate bits from
an int value.

<!-- $MDX file=./lib/double_dabble.ml,part=sw -->
```ocaml
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
```

# Implementing the Hardware

A state machine will implement the algorithm.  There are 3 states

- `Start` - wait for a `start` signal to be valid along with the binary value to convert.
- `Double` - shift the binary value and all the BCD digits up by 1 bits. Go to `Start` if
  all the input bits are processed, otherwise go to `Dabble`.
- `Dabble` - iterate through the BCD digits - if the value is greater than 4 then add 3.
  Go back to `Double`.

<!-- $MDX file=./lib/double_dabble.ml,part=hw -->
```ocaml
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
```

In `Double` we need to shift up the BCD values. This is done by selecting the msb of the
previous bcd register and shifting it in - expect for index 0 where we select the msb of
the input binary value.

In `Dabble` we iterate through each BCD digit - if we have 3 digits, it will take 3
 cycles. On each cycle we use `digit_count_one_hot` to enable the write to the appropriate
 BCD digit register. This is created using the `binary_to_onehot` function which converts
 values as follows:
 
 - `00` -> `0001`
 - `01` -> `0010`
 - `10` -> `0100`
 - `11` -> `1000`

We could have implemented the write enable check as `digit_count.value ==:. index` but the
onehot version is a bit more efficient.

Finally, note that we write the value `bcd_dabbled` to every BCD digit register (guarded
by the write enable). This value is created by multiplexing the BCD registers based on
`digit_count` then performing the dabble check. This is important - there is only one
instantiation of the check, not one per digit.

# Testing

We need to create a `Circuit`, a `Cyclesim` simulator and provide access to the input and
output ports. The following code does this. We will soon learn about `Interfaces` which
makes a lot of this boilerplate code go away. 

<!-- $MDX file=./lib/double_dabble.ml,part=boilerplate -->
```ocaml
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
```
There is nothing interesting going on here, just definition of string names for circuit ports,  
then constructing a simulator and looking up the ports.

Below we write exhaustive testbenches for 2, 3, and 4 digit conversions.

<!-- $MDX file=./lib/double_dabble.ml,part=testing -->
```ocaml
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
```
