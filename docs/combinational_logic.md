# Combinational logic and working with vectors

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Combinational logic is formed from boolean functions whose outputs are fully
determined by their current inputs. This is in contrast to sequential logic,
where the outputs are a function of their current and previous inputs.

At the lowest level, combinational circuits are built from simple primitives such
as NAND gates (in ASIC designs) or LUTs (in FPGA designs). Building circuits
with such low-level primitives is tedious, so instead, we provide a set of higher-level
primitives with which to design circuits. The `Comb.S` module type
provides these primitives.

Combinational circuits can be thought of as graphs with `wires` (often
also referred to as `signals`) connecting together logic operations,
which are the nodes of the graph. These graphs are directed and must
not contain any cycles.

# Vectors and widths

The fundamental type in Hardcaml is a vector with a specified width. The width
can be one or more bits. There exists a special signal called `empty` which has
zero width but is rarely used and exists more for internal use.

The simplest way to create a `Bits.t` value is the `of_string` function.

```ocaml
# open Base;;
# open Hardcaml.Bits;;
# let x = of_string "11001";;
val x : t = 11001
```

We can now interrogate the width of this value.

```ocaml
# width x
- : int = 5
```

It is always possible to get the width of a Hardcaml vector.

# Binary constants and converting to OCaml ints

`of_string` takes a string which consists of `0`s and `1`s (`_`s are
also allowed, and are ignored). The first character in the string
becomes the most significant bit (msb) of the vector. The vector width
is the sum of the number of `0` and `1` characters in the string.

```ocaml
# let x = of_string "100"
val x : t = 100
```

A feature of the `Bits` module (but not `Signal`s) is the ability to convert
back to an OCaml value. We can do so with the `to_int` function.

```ocaml
# to_int x
- : int = 4
```

`to_int` has interpreted `x` as an unsigned, three-bit integer value.
`to_sint` will treat it as a signed, twos-complement integer value.

```ocaml
# to_sint x
- : int = -4
```

Care should be taken with vector widths that are greater than (or equal to) the
actual width of an OCaml integer, as the conversion will not raise if the value
does not fit.

# Richer constants

Probably the most useful constant-generating function is `of_int`.

```ocaml
# of_int ~width:10 514
- : t = 1000000010
```

If the given value is negative, it will be sign extended to the appropriate width.

```ocaml
# of_int ~width:10 (-1)
- : t = 1111111111
```

The function `of_string` is actually more general than just converting
binary values. It can also take a specification string which roughly
follows the form of Verilog constants: `<width>'<format><value>`.
Binary, decimal, octal, and hexadecimal notations are supported.

```ocaml
# of_string "5'b1101"
- : t = 01101
# of_string "5'hd"
- : t = 01101
# of_string "5'd13"
- : t = 01101
```

If the format specifier is capitalised, the leading bit in the given
value will be used for sign extension (only relevant for the
binary, octal, and hex specifiers).

```ocaml
# of_string "5'B1101"
- : t = 11101
# of_string "5'Hd"
- : t = 11101
```

# Operators, widths and their names

Hardcaml provides a set of operators providing the usual functions
such as logical, arithmetic and comparison operations. A key API
design point is Hardcaml does not encode signedness into the type of
vectors. Where needed, the operator suffix indicates how to interpret
the operands. For example, there are two "less than" operators.

- `(<:)` for unsigned less than
- `(<+)` for signed less than

An operation that ends in a `+` treats is operands a signed, while
one that end in a `:` treats its operands as unsigned.

Many operators are agnostic to signedness. The addition operator
`(+:)` is an example of this. The reason for this is that the addition
operator requires that both its arguments be the same width (or it
will raise a runtime exception). Interpreting the arguments as signed
or unsigned will lead to the same result.

# API tour

The following describes several key functions. See the
documentation for
[`Comb.S`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Comb/module-type-S/index.html)
for other useful functions.

## Arithmetic

- `(+:)` addition
- `(-:)` subtraction

Both arguments must be the same width. The result will be the width of
the arguments.

- `(*:)` unsigned multiplication
- `(*+)` signed multiplication

The arguments can have arbitrary widths. The result will be the sum
of the widths of the arguments.

## Logical

- `(&:)` logical and
- `(|:)` logical or
- `(^:)` logical xor

Both arguments must be the same width. The result will be the width of
the arguments.

- `(~:)` logical not

The result will be the width of the argument.

## Comparison

- `(==:)` equals
- `(<>:)` not equals
- `(<:)`, `(<=:)`, `(>:)`, `>=:` unsigned comparisons
- `(<+)`, `(<=+)`, `(>+)`, `>=+` signed comparisons

Both arguments must be the same width. The result will be 1 bit.

## Multiplexers

`mux2 sel t f` selects `t` when `sel` is high and `f` when `sel` is
low. `sel` should be 1 bit wide. `t` and `f` must be the same width.

`mux sel lst` selects the element in `lst` at position `sel`. The range
of `sel` must not exceed the length of `lst`. `lst` can be shorter,
however, and the last element in the list is logically repeated as
much as needed.

```ocaml
# List.init 4 ~f:(fun sel -> mux (of_int ~width:2 sel) [ gnd; vdd ]);;
- : t list = [0; 1; 1; 1]
```

## Select

A range of bits can be extracted from the vector using `select`.

```ocaml
# select (of_string "001100") 3 2
- : t = 11
```

The upper and lower indexes are inclusive. Selecting outside the range
of the input value raises.

The operator `(.:[,])` provides a nice syntax for this

```ocaml
# (of_string "001100").:[3,2]
- : t = 11
```

The operator `(.:+[, Some x])` can also be used similiar to Verilog

```ocaml
# (of_string "001100").:+[3, Some 2]
- : t = 01
```

Other selection functions include `sel_top`, `sel_bottom`, `drop_top`,
`drop_bottom` and a richer set of operators for special circumstances.

## Concatentation

Vectors can be concatenated using the following functions.

```ocaml
# vdd @: gnd
- : t = 10
# concat_msb [ vdd; gnd ]
- : t = 10
# concat_lsb [ gnd; vdd ]
- : t = 10
```

The \_msb and \_lsb suffix in the functions indicates whether the head of the list
holds the most significant bit or least significant bit respectively.

# Integer arguments

Various operators can take an integer as their right-hand argument.

```ocaml
# of_string "011" +:. 1
- : t = 100
```

The right-hand argument will be converted to a vector by inferring the
required width from the left-hand argument.

Such operators are suffixed with a `.`.
