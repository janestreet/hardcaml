# 4.3.1 Sequence Detector

In this example we will explore the implementation of a sequence detector. The hardware
will take in one bit per cycle and raise a `detect` signal when it sees a specific
sequence of bits.

Below we have implemented an example in Verilog, VHDL and Hardcaml for comparison. The
sequence detected is `101`.

### Verilog

<!-- $MDX file=./hdl/sequence_detector.v -->
```verilog
module sequence_detector (
  input clock, clear, d,
  output reg detect
);

  localparam 
    S1 = 0, 
    S10 = 1, 
    S101 = 2;

  reg [1:0] state;

  always @(posedge clock) 
    if (clear) begin 
      state <= S1;
      detect <= 0;
    end else begin
      case (state)
        S1: 
          if (d) state <= S10;
          else state <= S1;
        S10: 
          if (d) state <= S1;
          else state <= S101;
        S101: 
          state <= S1;
      endcase

      detect <= (state == S101) & d;
    end

endmodule
```

### VHDL

<!-- $MDX file=./hdl/sequence_detector.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity sequence_detector_vhdl is
  port (
    clock, clear, d : in std_logic;
    detect : out std_logic
  );
end entity;

architecture rtl of sequence_detector_vhdl is

  type state_t is (S1, S10, S101);
  signal state : state_t;

begin

  process (clock) begin
    if rising_edge(clock) then 
      if clear = '1' then 
        state <= S1;
        detect <= '0';
      else
        case (state) is
          when S1 => 
            if d = '1' then state <= S10;
            else state <= S1;
            end if;
          when S10 => 
            if d = '1' then state <= S1;
            else state <= S101;
            end if;
          when S101 => 
            state <= S1;
        end case;

        if state = S101 and d = '1' then
          detect <= '1';
        else
          detect <= '0';
        end if;
      end if;
    end if;
  end process;

end architecture;
```

### Hardcaml

<!-- $MDX file=./lib/sequence_detector.ml,part=simple_101 -->
```ocaml
  module State = struct
    type t =
      | S1
      | S10
      | S101
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ~clock ~clear ~d =
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            [ S1, [ if_ d [ sm.set_next S10 ] [ sm.set_next S1 ] ]
            ; S10, [ if_ d [ sm.set_next S1 ] [ sm.set_next S101 ] ]
            ; S101, [ sm.set_next S1 ]
            ]
        ]);
    reg spec (sm.is S101 &: d)
  ;;
```

In each state we transition to the next matching state if the input data is expected or
return to `S1` if not. When we are in the final state `S101` we check the input is correct
and set the `detect` output.

# Testing

<!--
```ocaml
# open Base
# open Hardcaml
# open Signal
# open Hardcaml_waveterm
# open Hardcaml_docs.Sequence_detector.Explicit
# let create_sequence_detector_sim () =
    Cyclesim.create
      (Circuit.create_exn
        ~name:"sequence_detector"
        [ output "detect" 
            (create 
                ~clock:(input "clock" 1) 
                ~clear:(input "clear" 1)
                ~d:(input "d" 1)) 
       ])
val create_sequence_detector_sim : unit -> Cyclesim.t_port_list = <fun>
```
-->

```ocaml
# let test bits = 
    let sim = create_sequence_detector_sim () in
    let waves, sim = Waveform.create sim in
    let d = Cyclesim.in_port sim "d" in
    List.iter (Bits.bits_lsb bits) ~f:(fun bit ->
      d := bit;
      Cyclesim.cycle sim); 
    Cyclesim.cycle sim;
    Waveform.print waves ~wave_width:1 ~display_width:40
val test : Bits.t -> unit = <fun>
# test (Bits.of_string "101")
┌Signals─┐┌Waves───────────────────────┐
│clock   ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│        ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│clear   ││                            │
│        ││────────────────            │
│d       ││────┐   ┌───────            │
│        ││    └───┘                   │
│detect  ││            ┌───            │
│        ││────────────┘               │
└────────┘└────────────────────────────┘
- : unit = ()
# test (Bits.of_string "1010")
┌Signals─┐┌Waves───────────────────────┐
│clock   ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│        ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│clear   ││                            │
│        ││────────────────────        │
│d       ││    ┌───┐   ┌───────        │
│        ││────┘   └───┘               │
│detect  ││                ┌───        │
│        ││────────────────┘           │
└────────┘└────────────────────────────┘
- : unit = ()
# test (Bits.of_string "1011")
┌Signals─┐┌Waves───────────────────────┐
│clock   ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│        ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│clear   ││                            │
│        ││────────────────────        │
│d       ││────────┐   ┌───────        │
│        ││        └───┘               │
│detect  ││                            │
│        ││────────────────────        │
└────────┘└────────────────────────────┘
- : unit = ()
```

As we see the first two test sequences are correctly detected. However, the third one is
not.  Can you see why?

# Parameterizing over the Sequence

In the following code we make the circuit parameterized over the sequence we wish to
detect. The code is pretty similar to before - the interesting bit is that the number
states required is determined by the sequence length we need to match against.

<!-- $MDX file=./lib/sequence_detector.ml,part=generic -->
```ocaml
  let next_states sequence cur_state =
    let sequence_length = Bits.width sequence in
    (cur_state + 1) % sequence_length, 0
  ;;

  let create ~sequence ~clock ~clear ~d =
    let sequence_length = Bits.width sequence in
    let module State = struct
      type t = int [@@deriving compare ~localize, sexp_of]

      (* Valid states are integers in the range [0..sequence_length-1]. *)
      let all = List.init sequence_length ~f:Fn.id
    end
    in
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            (List.init sequence_length ~f:(fun cur_state ->
               ( cur_state
               , let t, f = next_states sequence cur_state in
                 let t, f =
                   (* Swap the states if the match bit is [0]. *)
                   if Bits.to_bool sequence.Bits.:(cur_state) then t, f else f, t
                 in
                 [ if_ d [ sm.set_next t ] [ sm.set_next f ] ] )))
        ]);
    (* Decode the [detect] result - we are in the final state and the last bit matches. *)
    reg
      spec
      (sm.is (sequence_length - 1)
       &: if Bits.to_bool sequence.Bits.:(sequence_length - 1) then d else ~:d)
  ;;
```

The `State` module specifies a number of states equal to the length of `sequence`. This is
done in the definition of the `all` value.

The implementation of each state is built dynamically. They are all of the form

```
[ if_ d [ sm.set_next t] [sm.set_next f] ]
```

The function `next_states` returns the states to transition to if the input is valid or
invalid. Note we swap them depending on if we expect to match a `0` or a `1`.

# Fixing the Matching Problem

We noted a problem that we don't match all sequences properly. The problem is caused by
the state machine always returning to the initial state if the current input bit doesn't
match. It is possible, however, that we should return to some earlier state which
partially matches the input sequence.

In the previous example we input the sequence `1->1->0->1` and it didn't match.  Consider what happens:

1. We get the first `1` and transition to `S10`
2. We get a `1` and transition to `S1`
3. We get a `0` and stay in `S1`
4. We get a `1` and transition to `S10`

We didn't match.  What we should do is

1. We get the first `1` and transition to `S10`
2. We get a `1` and stay in `S10`
3. We get a `0` and transition to `S101`
4. We get a `1` and set `detect`

To this fix we need to find the state which matches the longest prefix of our sequence
when the input bit does not match. We can do this by modifying our `next_states` function.
 
There is actually a further problem - when we get a sequence match that may actually form
a prefix for the next match i.e. consider matching `111` against `1111` there are two
matches adjacent to each other.

So, there are two cases in which we must find the longest partial match and move to the
state that represents it.

1. If the current input bit doesn't match the sequence
2. or if we are in the last state and the bit does match
 
<!-- $MDX file=./lib/sequence_detector.ml,part=fix_next_state -->
```ocaml
  let find_prev_match match_bit sequence cur_state =
    let open Bits in
    if cur_state = 0
    then 0
    else (
      let sequence = sequence.:[cur_state - 1, 0] in
      let rec find cur_state sequence match_ =
        if cur_state = 0
        then 0
        else if Bits.equal sequence match_
        then cur_state
        else if cur_state = 1
        then 0
        else find (cur_state - 1) (lsbs sequence) (msbs match_)
      in
      find cur_state sequence (msbs (match_bit @: sequence)))
  ;;

  let next_states sequence cur_state =
    let open Bits in
    let sequence_length = width sequence in
    let on_match =
      if cur_state = sequence_length - 1
      then find_prev_match sequence.:(cur_state) sequence cur_state
      else (cur_state + 1) % sequence_length
    in
    let no_match = find_prev_match ~:sequence.:(cur_state) sequence cur_state in
    on_match, no_match
  ;;
```

It is important to note that we are not actually generating any hardware here - we are
performing calculations to define what hardware we should generate.

# Building a Test Harness

We now want to show that the fixes we made have actually worked. To do this we will create
a self checking testbench.

The idea is we will run a sequence of bits through the detector module and record every
place it finds a match. We will compare this against a software reference model and
compare the two results. We can then run a bunch of random tests and make sure everything
is correct.

Lets start off by creating the software reference model.

<!-- $MDX file=./lib/sequence_detector.ml,part=golden_model -->
```ocaml
let software_reference sequence bits =
  let open Bits in
  List.init
    (width bits - width sequence + 1)
    ~f:(fun pos ->
      if to_bool (bits.:[width sequence + pos - 1, pos] ==: sequence)
      then Some pos
      else None)
  |> List.filter_opt
  (* We find the first bit of the match, but the hardware will find the last bit so offset
     it by the length of the sequence. *)
  |> List.map ~f:(fun d -> d + width sequence - 1)
;;
```
We can now write a testbench that runs the sequence detector and collects all the matches 
it sees.  It compares against the software model and raises if they do not agree.

<!-- $MDX file=./lib/sequence_detector.ml,part=testbench -->
```ocaml
let test ~sequence ~data_in =
  let sim = create_sequence_detector_sim ~sequence in
  let d = Cyclesim.in_port sim "d" in
  let detect = Cyclesim.out_port sim "detect" in
  let open Bits in
  let cycles = width data_in in
  let results = ref [] in
  for i = 0 to cycles - 1 do
    d := data_in.:(i);
    Cyclesim.cycle sim;
    if Bits.to_bool !detect then results := i :: !results
  done;
  let results = List.rev !results in
  let expected = software_reference sequence data_in in
  if not (List.equal Int.equal results expected)
  then
    raise_s
      [%message
        (sequence : Bits.t) (data_in : Bits.t) (results : int list) (expected : int list)]
;;
```

Now we can run some random tests across different sequences.

<!-- $MDX file=./lib/sequence_detector.ml,part=expect_test -->
```ocaml
let%expect_test "check random sequences" =
  for _ = 0 to 10 do
    let sequence = Bits.random ~width:(1 + Random.int 5) in
    for _ = 0 to 100 do
      let data_in = Bits.random ~width:(Bits.width sequence + Random.int 100) in
      test ~sequence ~data_in
    done
  done
;;
```

