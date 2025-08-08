# 4.6 How Hardcaml Converts Signals

Hardcaml defines an OCaml variant type called `Signal.Type.t` which expresses the RTL
structures it can describe. How this is converted to RTL explains a lot about the various
choices we make in the Hardcaml API.

Below we will show how every variant in `Signal.Type.t` is converted to Verilog.
Conversion to VHDL is very similar. Note that this really is all of the primitives in
Hardcaml! There are not very many, and all other features of Hardcaml are built atop them.

The correctness of the generated code will depend on rules that Hardcaml enforces and we
will highlight where that is the case. The following rules are applied in order to try to
make the resulting Verilog constructs trivial to understand.

- Widths of arguments are restricted so we do not have to understand any weird Verilog
  auto-conversion rules.
- The width of the LHS and RHS of assignments are nearly always the same (comparison and
  multiplication being exceptions). 
- Assignments are _complete_ which generally means where we generate a Verilog `case`
  construct, we will include a `default` branch.

# Simple Operations

We will quickly go through a number of simple operations which convert trivially to RTL.

## Constants

Constants are always written in binary form.

```verilog
wire [4:0] _123;
assign _123 = 5'b10001;
```

## Concatenation

```verilog
wire [23:0] _123;
assign _123 = { _33, _87; _43 };
```

The width of the result is the sum of the width of all the elements in the concatenation.

## Wires

Wires just make a copy. The only interesting thing in the generated RTL is that wires are
declared before they are assigned, which is a requirement to allow them to perform cyclic
references.

```verilog
wire [3:0] a_wire;

assign a_wire = _123;
```

## Select

```verilog
wire [2:0] _123;

assign _123 = _55[3:1];
```

The rules of the Hardcaml `select` function ensure that the Verilog selection cannot
access out of range bits.

# Arithmetic Operators

## Addition and subtraction

The Hardcaml rules are that the arguments and result must all be of the same width. Thus
we write:

```verilog
wire [7:0] _123;

assign _123 = a + b;
```

Given `a` and `b` are the same size as the result it doesn't matter if we consider them to
be signed or unsigned - the resulting bit pattern is the same.

The same is true of subtraction.

## Multiplication

We must differentiate between signed and unsigned multiplication. For unsigned we write:

```verilog
wire [8:0] _123;

assign _123 = a * b;
```

By default Verilog performs unsigned multiplication. The result width is the sum of the
width of the arguments.

To perform signed multiplication we write:


```verilog
wire [8:0] _123;

assign _123 = $signed(a) * $signed(b);
```

# Logical Operators

The `and`, `or`, and `xor` logical operations are defined similarly to addition and
subtraction in that the result and arguments must have the same width.


```verilog
wire [7:0] _123;

assign _123 = a ^ b;
```

`not` is also similar except that it only has one argument.

```verilog
wire [7:0] _123;

assign _123 = ~a;
```

# Comparison Operators

Hardcaml provides 2 comparison operators - equals and unsigned less than. The result of
both is a single bit. The arguments are the same width.

```verilog
wire _123;

assign _123 = a == b;
```

And less than:

```verilog
wire _123;

assign _123 = a < b;
```

Hardcaml synthesizes the full set of comparison operators for both signed and unsigned
arguments using these two primitives and the `not` operator.

# Mux

A Hardcaml mux takes a select signal and some number of data inputs. The final data input
is repeated enough times to create a complete mux structure. For example, our select could
be 3 bits and we provide 5 data inputs. The last value will be repeated and used for
indices 5, 6, and 7.

These rules are encoded by using a case statement with a default branch.

```verilog
case (select)
0: _123 <= _1;
1: _123 <= _2;
2: _123 <= _3;
3: _123 <= _4;
default: _123 <= _5;
endcase
```

Even if the case is complete, a default branch is written.

```verilog
case (select)
0: _123 <= _1;
1: _123 <= _2;
2: _123 <= _3;
3: _123 <= _4;
4: _123 <= _5;
5: _123 <= _6;
6: _123 <= _7;
default: _123 <= _8;
endcase
```

# Cases

The Hardcaml `Cases` variant also generates a Verilog case statement, but the branch match
values do not have to be successively incrementing constants starting at 0 as with a mux.

```verilog
case (select)
3: _123 <= _1;
7: _123 <= _2;
9: _123 <= _4
default: _123 <= _5;
endcase
```

We do, however, need to provide a default value. As with muxes we ensure that no matter
what select value is provided, the result value _must_ be driven.

# Registers

Register are written using the following general template:

```verilog
always @(posedge clock, posedge reset)
  if (reset == 1'b1) q <= 1'b0;
  else if (clear == 1'b1) q <= 1'b0;
  else if (enable == 1'b1) q <= d;
```

The asynchronous reset, synchronous clear and enable are all optional. If they are not
present Hardcaml will not write out that part of the template.

reset, clear and enable must all be 1 bit wide. The register input value, reset to value
and clear to value are all the same width as the result value.

Hardcaml further supports negative edge clocks.

# Memories

Hardcaml memories are defined in two parts - the memory array itself along with its write
logic and one or more read ports.

These memory structures describe asynchronous read memories that may be converted to
synchronous by registering the address(es) or output port(s).

Hardcaml memories may have multiple read and/or write ports, though only specific
instances will be inferred into actual hardware memories (otherwise we describe
potentially inefficient register banks).

```verilog
reg [7:0] mem [0:3];

always @(posedge clock)
  if (write_enable)
    mem[write_address] <= write_data;
```

The above may be written once for each provided write port.

Read ports are written as:

```verilog
assign q = mem[read_address];
```

Again, we will get similar code for each read port.

# Instantiations

Instantiations are written into the RTL following the specification of inputs and outputs
given in Hardcaml.

```verilog
foo the_foo (.i(i), .o1(_123[3:0]), .o2(_123[5:3]));
```

Note that there is a single output vector defined for all the output ports from which each
individual output will then be selected. This makes the traversal of the Hardcaml signal
graph simpler, at the cost of slightly more verbose RTL.
