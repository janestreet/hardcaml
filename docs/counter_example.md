# Counter example design

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

The following is a simple 8-bit counter. It resets back to 0 when the
clear signal is high and counts up when the incr signal is high.
Otherwise, it holds its previous value.

## Design

```ocaml
open Hardcaml
open Hardcaml.Signal
open Hardcaml_waveterm

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; incr : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { dout : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end
```

```ocaml
# let create (i : _ I.t) =
    { O.dout =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~enable:i.incr
          ~width:8
          ~f:(fun d -> d +:. 1)
    }
val create : t I.t -> t O.t = <fun>
```

This design, although simple, shows the usual pattern for defining a
circuit in Hardcaml. The inputs and outputs of the circuit are
specified using interfaces, and the circuit is built using a
function which takes an input interface and returns an output
interface.

The implementation uses the `reg_fb` function. This constructs a
register with feedback. Let's look at each argument in turn.

- `Reg_spec.t` packages up the `clock` and synchronous clear `signal`.
  There are various other arguments which can control an asynchronous
  reset, rising or falling clock edge and so on.
- `enable`: when high the register will load a new value. Otherwise, it
  holds its previous value.
- `width` is the bit width of the register.
- `f`: this function receives the current value of the
  register, and computes the next value. In this case, it increments it
  by one.

## Testbench

The following is a simple testbench for the counter which shows its
behaviour for different values of `clear` and `incr`.

The purpose of a testbench is to set values for the inputs of a
circuit and check what values this causes the outputs to take over
time.

```ocaml
module Simulator = Cyclesim.With_interface(I)(O)
```
```ocaml
# let testbench (create_design_fn : t I.t -> t O.t) =
    (* Construct the simulation and get its input and output ports. *)
    let sim = Simulator.create create_design_fn in
    let inputs : _ I.t = Cyclesim.inputs sim in
    let outputs : _ O.t = Cyclesim.outputs sim in
    (* Perform a clock cycle.  Apply the given values to [incr] and [clear].
       Printf the current values of [dout]. *)
    let step ~clear ~incr =
      inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
      inputs.incr := if incr=1 then Bits.vdd else Bits.gnd;
      Stdio.printf "dout='%s'\n" (Bits.to_string !(outputs.dout));
      Cyclesim.cycle sim
    in
    (* run the counter for 6 clock cycles *)
    step ~clear:0 ~incr:0;
    step ~clear:0 ~incr:1;
    step ~clear:0 ~incr:1;
    step ~clear:1 ~incr:0;
    step ~clear:0 ~incr:0;
    step ~clear:0 ~incr:0
  ;;
val testbench : (t I.t -> t O.t) -> unit = <fun>

# testbench create
dout='00000000'
dout='00000000'
dout='00000001'
dout='00000010'
dout='00000000'
dout='00000000'
- : unit = ()
```

## Two other implementations

### With a wire

The following implementation shows what is actually happening within the
`reg_fb` function. First, a wire is created that can be read when we
construct a register. It is assigned after we have the register
output.

Wires allow us to describe cyclic logic structures in Hardcaml. Note
that all such cycles in a hardware design *must* pass through a
sequential element such as a register or memory. Cyclic paths that do
not are called combinational loops, and Hardcaml will detect and raise
an error if one is found.

```ocaml
# let counter_with_wire (i : _ I.t) =
    let w = wire 8 in
    let dout =
      reg
        (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
        ~enable:i.incr
        (w +:. 1)
    in
    w <== dout;
    { O.dout }
val counter_with_wire : t I.t -> t O.t = <fun>
# testbench counter_with_wire
dout='00000000'
dout='00000000'
dout='00000001'
dout='00000010'
dout='00000000'
dout='00000000'
- : unit = ()
```

### With the Always DSL

We can also describe the counter with the [Always DSL](./always.md).

Note that we could have encoded the clear and increment logic when we
constructed the `Always.Variable.reg`. The Always fragment would then
have only consisted of an assignment to `dout`.

```ocaml
# let counter_with_always (i : _ I.t) =
    let dout =
      Always.Variable.reg
        (Reg_spec.create ~clock:i.clock ())
        ~enable:vdd
        ~width:8
    in
    Always.(compile [
        if_ i.clear
          [ dout <--. 0;]
          [ when_ i.incr
            [ dout <-- dout.value +:. 1 ]
          ]
    ]);
    { O.dout = dout.value}
val counter_with_always : t I.t -> t O.t = <fun>
# testbench counter_with_always
dout='00000000'
dout='00000000'
dout='00000001'
dout='00000010'
dout='00000000'
dout='00000000'
- : unit = ()
```
