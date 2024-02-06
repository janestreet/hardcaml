# Sequential Logic

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Sequential logic is built from primitives such as registers and
memories.

A key concept in sequential logic is the `clock`. Sequential logic
updates on the rising (or, less commonly, falling) edge of the clock and
holds its value throughout the clock cycle.

# Reg_spec

The clock and related reset and clear signals are grouped together in
a type called a [`Reg_spec.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Reg_spec/index.html).

```ocaml
# open Hardcaml
# let clock = Signal.input "clock" 1
val clock : Signal.t = (wire (names (clock)) (width 1) (data_in empty))
# let clear = Signal.input "clear" 1
val clear : Signal.t = (wire (names (clear)) (width 1) (data_in empty))
# let spec = Reg_spec.create ~clock ~clear ()
val spec : Reg_spec.t =
  {Hardcaml.Reg_spec.reg_clock =
    (wire (names (clock)) (width 1) (data_in empty));
   reg_clock_edge = Hardcaml__.Edge.Rising; reg_reset = empty;
   reg_reset_edge = Hardcaml__.Edge.Rising; reg_reset_value = empty;
   reg_clear = (wire (names (clear)) (width 1) (data_in empty));
   reg_clear_level = Hardcaml__.Level.High; reg_clear_value = empty;
   reg_enable = empty}
```

Multiple sequential elements are then able to refer to the same
`Reg_spec.t`.

`Reg_spec`s can be overridden and can change the value of the register
when it is reset or cleared.

```ocaml
# let new_spec = Reg_spec.override spec ~clear_to:Signal.vdd
val new_spec : Reg_spec.t =
  {Hardcaml.Reg_spec.reg_clock =
    (wire (names (clock)) (width 1) (data_in empty));
   reg_clock_edge = Hardcaml__.Edge.Rising; reg_reset = empty;
   reg_reset_edge = Hardcaml__.Edge.Rising; reg_reset_value = empty;
   reg_clear = (wire (names (clear)) (width 1) (data_in empty));
   reg_clear_level = Hardcaml__.Level.High;
   reg_clear_value = (const (names (vdd)) (width 1) (value 0b1));
   reg_enable = empty}
```

By default, registers start with the value `gnd` (or 0). Registers
built with `new_spec` will start with the value `vdd` (or 1).

# Registers, pipelines

A simple register takes a signal as input and basically delays it for one cycle.

```ocaml
# let d_in = Signal.input "d_in" 8
val d_in : Reg_spec.signal = (wire (names (d_in)) (width 8) (data_in empty))
# let q_out = Signal.reg spec ~enable:Signal.vdd d_in
val q_out : Reg_spec.signal =
  (register (width 8)
 ((clock clock) (clock_edge Rising) (clear clear) (clear_level High)
  (clear_to 0b00000000) (enable 0b1))
 (data_in d_in))
```

The pipeline function will delay its input for multiple cycles.

```ocaml
# let q_out_after_3_clocks = Signal.pipeline spec ~enable:Signal.vdd ~n:3 d_in
val q_out_after_3_clocks : Reg_spec.signal =
  (register (width 8)
 ((clock clock) (clock_edge Rising) (clear clear) (clear_level High)
  (clear_to 0b00000000) (enable 0b1))
 (data_in register))
```

# Registers with feedback

We noted [previously](combinational_logic.md) that combinational
logic could not contain cycles.  We can lift this restriction with
sequential logic so long as the cycle passes through a register (or memory).

The function `reg_fb` encodes a simple example of this pattern. For
example, to build a counter, we need to access the current value to produce
the next one.

```ocaml
# let counter = Signal.reg_fb spec ~enable:Signal.vdd  ~width:8 ~f:(fun d -> Signal.(d +:. 1))
val counter : Reg_spec.signal =
  (register (width 8)
 ((clock clock) (clock_edge Rising) (clear clear) (clear_level High)
  (clear_to 0b00000000) (enable 0b1))
 (data_in wire))
```

# Wires

In Hardcaml a `wire` is a signal which can be declared before
providing its input driver. Logically, it does nothing - it just
passes its input through to its output.

```ocaml
# let w = Signal.wire 1;;
val w : Reg_spec.signal = (wire (width 1) (data_in empty))
```

Wires can later be assigned an input driver.

```ocaml
# Signal.(w <== vdd);;
- : unit = ()
# w;;
- : Reg_spec.signal = (wire (width 1) (data_in 0b1))
```

Apart from the fact they logically do nothing, they are really useful!
It is how the `reg_fb` function is implemented.

```ocaml
# let reg_fb spec ~enable ~w f =
    let d = Signal.wire w in
    let q = Signal.reg spec ~enable (f d) in
    Signal.(d <== q);
    q
val reg_fb :
  Reg_spec.t ->
  enable:Reg_spec.signal ->
  w:int -> (Reg_spec.signal -> Reg_spec.signal) -> Reg_spec.signal = <fun>
```

Without wires we cannot express the above function since the input
to `reg` refers to its own output.

Two words of caution when using wires.

1. Combinational cycles! We can't stop you from creating them, but we can
   throw an exception when you do. It's worth repeating - cycles must
   pass through a sequential primitive.
2. The hairy unassigned wire exception. Hardcaml will detect when you
   forget to assign a value to a wire, but the error will not be
   especially useful in finding out where. You can
   enable an extra level of debugging information by setting the
   following [value](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Caller_id/index.html),
   which will help track down where the wire was originally defined.

<!-- Dont execute this -->
```
Caller_id.set_mode Top_of_stack
```

# State Machines

State machines are just a complex combination of registers and
multiplexers, so it is possible to build them directly with the
primitives described here - but not very conveniently. Rather, we
recommend using the [Always DSL](always.md).

# Memories

## Core asynchronous memory primitive

Hardcaml provides the `multiport_memory` primitive for describing
memory structures.

They are synchronously written using `write_ports`. Each write port
can have a different clock.

```ocaml
let clock = Signal.input "clock" 1;;
let address = Signal.input "address" 8;;
let write_enable = Signal.input "write_enable" 1;;
let data = Signal.input "data" 32;;
```

```ocaml
# let write_port =
    { Write_port.write_clock = clock
    ; write_address = address
    ; write_enable = write_enable
    ; write_data = data }
val write_port : Reg_spec.signal Write_port.t =
  {Hardcaml.Write_port.write_clock =
    (wire (names (clock)) (width 1) (data_in empty));
   write_address = (wire (names (address)) (width 8) (data_in empty));
   write_enable = (wire (names (write_enable)) (width 1) (data_in empty));
   write_data = (wire (names (data)) (width 32) (data_in empty))}
```

The memory is read asynchronously using `read_addresses`. The read data
is returned as an array, one for each read port.

```ocaml
# let read_address = address
val read_address : Reg_spec.signal =
  (wire (names (address)) (width 8) (data_in empty))
# let q =
    Signal.multiport_memory
      256
      ~write_ports:[|write_port|]
      ~read_addresses:[|read_address|]
val q : Reg_spec.signal array =
  [|(memory_read_port (width 32)
 ((memory multiport_memory) (read_addresses address)))|]
```

## RAMs

On their own, `multiport_memory`s are not that useful. Instead, we
provide the [`Ram`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Ram/index.html)
module, which can target the physical RAM blocks in
FPGAs (i.e. BlockRAM or UltraRAM in Xilinx devices).

This is done by instantiating a multiport_memory and a register placed
on either the read address or output data. Note that this means the
read ports are now synchronous and read data is returned one cycle
later.

```ocaml
# let read_port =
    { Read_port.read_clock = clock
    ; read_address
    ; read_enable = Signal.input "read_enable" 1 }
val read_port : Reg_spec.signal Read_port.t =
  {Hardcaml.Read_port.read_clock =
    (wire (names (clock)) (width 1) (data_in empty));
   read_address = (wire (names (address)) (width 8) (data_in empty));
   read_enable = (wire (names (read_enable)) (width 1) (data_in empty))}
```

Each read port can have a different clock.

```ocaml
# let q =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:256
      ~write_ports:[|write_port|]
      ~read_ports:[|read_port|]
      ()
val q : Reg_spec.signal array =
  [|(register (width 32) ((clock clock) (clock_edge Rising) (enable read_enable))
 (data_in memory_read_port))|]
```

For this to work we use a feature of FPGA synthesizers called RTL RAM
inference. This process is notoriously finicky, so read the tool
reports to ensure it is doing what you expect (I'm looking at you,
Vivado!).

## Practical considerations

The RAM structures provided with Hardcaml are very flexible regarding
the number of read and write ports they can provide. Physically,
however, FPGAs provide RAMs with 1 or 2 ports. If you specify more
than this, you will probably not get the results you intended.

The [`hardcaml_xilinx`](https://github.com/janestreet/hardcaml_xilinx)
library offers a more targeted solution for Xilinx FPGAs and may be
more suitable for applications where precise control of the Vendor RAM
primitive is required.
