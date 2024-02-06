# Serial multipler example

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

We now show a complete example which implements a bit-serial unsigned
multiplier. We will both implement the design and show how to test it.

# Algorithm

Given two input numbers of bit-widths `N` and `M`, a bit serial
multiplier will produce a result of size `M+N` every M clock cycles.

Given two input values `a` and `b`, we consider each bit of `b` in turn
and form a partial product.

```
if b[i] then (a << i) else 0
```

This then gets added to a running sum. Let's start with a simple OCaml
implementation of this algorithm.

```ocaml
# open Base
# let rec umul a b =
  if b=0 then 0
  else
    let partial_product = if (b land 1) = 1 then a else 0 in
    partial_product + umul (a lsl 1) (b lsr 1)
val umul : int -> int -> int = <fun>
# umul 3 5
- : int = 15
# umul 100 99
- : int = 9900
```

Note that we do not explicitly track the iteration number. Rather we
detect when to stop by shifting `b` down on each iteration and
checking if it is `0`. Similarly, the partial product term `a << i` is
computed by shifting up by one each iteration. A similar set of tricks
will be used to define an efficient hardware implementation.

Before we turn fully to the hardware implementation, let's try to port
the above code to the Hardcaml `Bits` type.

```ocaml
# open Hardcaml
# open Hardcaml.Bits
# let rec umul a b =
  if to_int b = 0 then zero (width a)
  else
    let partial_product = mux2 b.:[0,0] a (zero (width a)) in
    partial_product +: umul (sll a 1) (srl b 1)
val umul : t -> t -> t = <fun>
# to_int (umul (of_int ~width:2 3) (of_int ~width:3 5))
- : int = 3
```

The reason this hasn't worked is we are computing the running sum and
partial product terms using the bit-width of `a`. We need to
also include the width of `b`.

```ocaml
# let umul a b = umul (uresize a (width a + width b)) b
val umul : t -> t -> t = <fun>
# to_int (umul (of_int ~width:2 3) (of_int ~width:3 5))
- : int = 15
# to_int (umul (of_int ~width:7 100) (of_int ~width:7 99))
- : int = 9900
```

# Hardware implementation

We start by implementing functions for the partial product and running sum.

```ocaml
# open Hardcaml.Signal
# let partial_product a b0 =
    mux2 b0 a (zero (width a))
val partial_product : Type.t -> Type.t -> Type.t = <fun>
# let running_sum first sum a b0 =
    let sum = mux2 first (zero (width sum)) sum in
    ue sum +: (ue (partial_product a b0))
val running_sum : Type.t -> Type.t -> Type.t -> Type.t -> Type.t = <fun>
```

The `running_sum` function takes a new argument called `first`. This
is used to indicate when we are processing bit `0` of `b` and clears
the initial sum to `0`.

The inputs to the addition in `running_sum` are of `width a` and are
zero-extended by the `ue` function to produce a result of `width a +
1`. Didn't we need more precision than this in the `Bits`
implementation? We will avoid this by outputting a fully computed bit
at each iteration.

```ocaml
# let running_sum_reg spec first a b0 =
    let sum_w = wire (width a) -- "running_sum" in
    let running_sum = running_sum first sum_w a b0  -- "running_sum_next" in
    (* Split the sum into it's least significant bit and the rest. *)
    let running_sum_bit_out = lsb running_sum in
    let running_sum = msbs running_sum in
    (* Register the sum *)
    let sum = reg spec ~enable:vdd running_sum in
    sum_w <== sum;
    sum, running_sum_bit_out
val running_sum_reg :
  Type.register -> Type.t -> Type.t -> Type.t -> Type.t * Type.t = <fun>
```

We also need to store the computed bits in a register.

```ocaml
# let computed_bits spec width bit =
   reg_fb spec ~width ~f:(fun d -> bit @: msbs d)
val computed_bits : Type.register -> int -> Type.t -> Type.t = <fun>
```

The final implementation just needs to put these functions together.

```ocaml
# let umul_sequential clock first a b_width b0 =
    let spec = Reg_spec.create ~clock () in
    let running_sum, computed_bit = running_sum_reg spec first a b0 in
    let computed_bits = computed_bits spec b_width computed_bit in
    running_sum @: computed_bits
val umul_sequential : Type.t -> Type.t -> Type.t -> int -> Type.t -> Type.t =
  <fun>
```

# Testbench

We will now test our multiplier. First, let's create the required
`Circuit.t`.

```ocaml
# let create_circuit a_width b_width =
    let clock = input "clock" 1 in
    let first = input "first" 1 in
    let a = input "a" a_width in
    let b0 = input "b0" 1 in
    let result = umul_sequential clock first a b_width b0 in
    Circuit.create_exn ~name:"umul" [ output "result" result ]
val create_circuit : int -> int -> Circuit.t = <fun>
```

We can now create a simulation and waveform and get a handle on the
input and output ports.

```ocaml
# module Waveform = Hardcaml_waveterm.Waveform
module Waveform = Hardcaml_waveterm.Waveform
# let create_sim circuit =
    let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
    let waves, sim = Waveform.create sim in
    let first = Cyclesim.in_port sim "first" in
    let a = Cyclesim.in_port sim "a" in
    let b0 = Cyclesim.in_port sim "b0" in
    let result = Cyclesim.out_port sim "result" in
    waves, sim, first, a, b0, result
val create_sim :
  Circuit.t ->
  Waveform.t * (Cyclesim.Port_list.t, Cyclesim.Port_list.t) Cyclesim.t *
  Bits.t ref * Bits.t ref * Bits.t ref * Bits.t ref = <fun>
```

The following testbench will take `a` and `b` and create a circuit
adapted to their bit widths. It will then perform `width b` iterations
and return the final result.

```ocaml
# let test a_in b_in =
    let open Bits in
    let waves, sim, first, a, b0, result =
      create_circuit (width a_in) (width b_in)
      |> create_sim
    in
    let step iteration =
      first := if iteration=0 then vdd else gnd;
      b0 := b_in.:[iteration,iteration];
      Cyclesim.cycle sim;
    in
    a := a_in;
    for i=0 to width b_in - 1 do
      step i
    done;
    (* grab the result and perform 1 more cycle so we can see the result in the waveform *)
    let result = !result in
    Cyclesim.cycle sim;
    waves, result
val test : Bits.t -> Bits.t -> Waveform.t * Bits.t = <fun>
```

Let's test our running examples of multiplying `3*5` and `100*99`.

```ocaml
# let waves, result = test (Bits.of_int ~width:2 3) (Bits.of_int ~width:3 5)
val waves : Waveform.t = <abstr>
val result : Bits.t = 01111
# Stdio.printf "%i" (Bits.to_int result)
15
- : unit = ()
# Waveform.print ~display_height:25 waves
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
│               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
│               ││────────────────────────────────                   │
│a              ││ 3                                                 │
│               ││────────────────────────────────                   │
│b0             ││────────┐       ┌───────────────                   │
│               ││        └───────┘                                  │
│first          ││────────┐                                          │
│               ││        └───────────────────────                   │
│               ││────────┬───────┬───────┬───────                   │
│result         ││ 00     │0C     │06     │0F                        │
│               ││────────┴───────┴───────┴───────                   │
│gnd            ││                                                   │
│               ││────────────────────────────────                   │
│               ││────────┬───────┬───────┬───────                   │
│running_sum    ││ 0      │1      │0      │1                         │
│               ││────────┴───────┴───────┴───────                   │
│               ││────────┬───────┬───────┬───────                   │
│running_sum_nex││ 3      │1      │3      │4                         │
│               ││────────┴───────┴───────┴───────                   │
│vdd            ││────────────────────────────────                   │
│               ││                                                   │
│               ││                                                   │
└───────────────┘└───────────────────────────────────────────────────┘
- : unit = ()
# let _, result = test (Bits.of_int ~width:7 100) (Bits.of_int ~width:7 99)
val result : Bits.t = 10011010101100
# Stdio.printf "%i" (Bits.to_int result)
9900
- : unit = ()
```
