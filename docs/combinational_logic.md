# 2.1 Combinational Logic

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
# open Base
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

Combinational circuits can be thought of as graphs with nodes representing logic
operations and edges representing `wires` (often also referred to as `signals`). These
graphs are directed and must not contain any cycles.

# Vectors and Widths

The fundamental type in Hardcaml is a vector with a specified width. The width
can be one or more bits. There exists a special signal called `empty` which has
zero width but is rarely used and exists for internal use.

In Hardcaml the types `Signal.t` and `Bits.t` are used to represent vectors. For now we
will consider `Bits.t`s.

The simplest way to create a `Bits.t` value is the `of_string` function:

```ocaml
# open Hardcaml.Bits;;
# let x = of_string "11001";;
val x : t = 11001
```

Note that in both the specification of the bit vector with `of_string` and the printing of
the value `x` we treat the left most 1 or 0 as the most significant bit.

We can now interrogate the width of this value:

```ocaml
# width x
- : int = 5
```

It is always possible to get the width of a Hardcaml vector.

# Binary Constants and Converting to OCaml Ints

`of_string` takes a string which consists of `0`s and `1`s (`_`s are
also allowed, and are ignored). The first character in the string
becomes the most significant bit (msb) of the vector. The vector width
is the sum of the number of `0` and `1` characters in the string.

```ocaml
# let x = of_string "100"
val x : t = 100
```

A feature of the `Bits` module (but not `Signal`s) is the ability to convert
back to an OCaml value. We can do so with the `to_unsigned_int` function.

```ocaml
# to_unsigned_int x
- : int = 4
```

`to_unsigned_int` has interpreted `x` as an unsigned, three-bit integer value.
`to_signed_int` will treat it as a signed, twos-complement integer value.

```ocaml
# to_signed_int x
- : int = -4
```

If the resulting value cannot fit in an integer then the functions will raise.
`to_int_trunc` may be useful in such cases if used carefully.

# Richer Constants

Probably the most useful constant-generating functions are `of_unsigned_int` and `of_signed_int`:

```ocaml
# of_unsigned_int ~width:10 514
- : t = 1000000010
```

If the given value is negative, it will be sign extended to the appropriate width:

```ocaml
# of_signed_int ~width:10 (-1)
- : t = 1111111111
```

Values too large or too small to be represented in `width` bits will raise an exception.
`of_int_trunc` will silently truncate the input value. Variants for `Int32.t` and
`Int64.t` are also provided.


The function `of_string` is actually more general than just converting
binary values. It can also parse a specification string which roughly
follows the Verilog constant format: `<width>'<format><value>`.
Binary (`b`), decimal (`d`), octal (`o`), and hexadecimal (`h`) notations are
supported.

```ocaml
# of_string "5'b1101"
- : t = 01101
# of_string "5'hd"
- : t = 01101
# of_string "5'd13"
- : t = 01101
```

If the format specifier is capitalized, the leading bit in the given
value will be used for sign extension (only relevant for the
binary, octal, and hex specifiers).

```ocaml
# of_string "5'B1101"
- : t = 11101
# of_string "5'Hd"
- : t = 11101
```

# Operators, Widths, and Their Names

Hardcaml provides a set of operators providing the usual functions
such as logical, arithmetic and comparison operations. A key API
design point is Hardcaml does not encode signedness into the type of
vectors. Instead, the operator suffix indicates how to interpret
the operands. For example, there are two "less than" operators.

- Operators ending with `+` treat operands as signed
- Operators ending with `:` treat operands as unsigned

For example, there are two distinct "less than" operators:
- `(<+)` for signed less than
- `(<:)` for unsigned less than

Many operators are agnostic to signedness. The addition operator `(+:)` is an example of
this. Addition requires both arguments to have the same bit width (otherwise it raises a
runtime exception), and the bit-by-bit addition process produces identical results whether
the values are interpreted as signed or unsigned. The carry behavior is the same; only the
semantic interpretation of the result differs.

# API Tour

The following describes several key functions. See the
documentation for
[`Comb.S`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/comb_intf.ml)
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
- `(<:)`, `(<=:)`, `(>:)`, `(>=:)` unsigned comparisons
- `(<+)`, `(<=+)`, `(>+)`, `(>=+)` signed comparisons

Both arguments must be the same width. The result will be 1 bit.

## Multiplexers

`mux2 sel t f` selects `t` when `sel` is high and `f` when `sel` is
low. `sel` should be 1 bit wide. `t` and `f` must be the same width.

`mux sel lst` selects the element in `lst` at position `sel`. The range
of `sel` must not exceed the length of `lst`. `lst` can be shorter,
however, and the last element in the list is logically repeated as
much as needed.

```ocaml
# List.init 4 ~f:(fun sel -> mux (of_unsigned_int ~width:2 sel) [ gnd; vdd ]);;
- : t list = [0; 1; 1; 1]
```

## Cases

The `cases` function is similar to a multiplexer in that it selects one of its inputs to
output. It is given a `select` signal and each case has a corresponding `match` value.
The first case where `select = match` is output.

```ocaml
# let cases select =
    cases
        ~default:(of_unsigned_int ~width:8 10)
        (of_unsigned_int ~width:4 select)
        [ of_unsigned_int ~width:4 2, of_unsigned_int ~width:8 20
        ; of_unsigned_int ~width:4 3, of_unsigned_int ~width:8 30
        ; of_unsigned_int ~width:4 6, of_unsigned_int ~width:8 60
        ]
    |> to_unsigned_int
val cases : int -> int = <fun>
# cases 3
- : int = 30
# cases 6
- : int = 60
```

If no case matches, the `default` value is output.

```ocaml
# cases 4
- : int = 10
```

## Select

A range of bits can be extracted from the vector using `select`.

```ocaml
# select (of_string "0011000") ~high:4 ~low:3
- : t = 11
```

The upper and lower indexes are inclusive. Selecting outside the range
of the input value raises.

The operator `(.:[,])` provides a nice syntax for this:

```ocaml
# (of_string "001100").:[3,2]
- : t = 11
```

The operator `(.:+[, Some x])` can also be used similar to Verilog:

```ocaml
# (of_string "001100").:+[3, Some 2]
- : t = 01
```

Other selection functions include `sel_top`, `sel_bottom`, `drop_top`,
`drop_bottom` and a richer set of operators for special circumstances.

## Concatenation

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

# Integer Arguments

Various operators can take an integer as their right-hand argument.

```ocaml
# of_string "011" +:. 1
- : t = 100
```

The right-hand argument will be converted to a vector by inferring the
required width from the left-hand argument.

Such operators are suffixed with a `.`.

# Richer Operations on Vectors

The functions on vectors described so far form a small subset of the full API provided by
Hardcaml. However, all the other functions are expressed in terms of this subset.

## Splitting

`split_lsb` and `split_msb` split a vector into multiple parts. `part_width` defines the
width of each part. By default the original vector must be exactly divisible by
`part_width`.

```ocaml
# split_lsb ~part_width:4 (of_string "16'H4321")
- : t list = [0001; 0010; 0011; 0100]
```

```ocaml
# split_msb ~part_width:4 ~exact:false (of_string "15'H4321")
- : t list = [1000; 0110; 0100; 001]
```

## Shifting

`sll`, `srl`, `sra` shift a vector by a constant amount.

`rotr`, `rotl` rotate by a constant amount.

`log_shift` builds a variable width shift circuit.

```ocaml
# sra (of_string "10") ~by:1
- : t = 11
# log_shift (of_string "0100") ~f:rotl ~by:(of_string "10")
- : t = 0001
```

## Resizing

`uresize` and `sresize` resize a vector to the given width which may be larger or smaller.
When growing larger `uresize` adds 0's at the top while `sresize` repeats the msb.

## Reductions

`reduce` takes a list of vectors and an operation to perform between them all.

Or reduction:

```ocaml
# reduce ~f:(|:) (bits_lsb (of_string "00100"))
- : t = 1
```

Summation

```ocaml
# reduce ~f:(+:) (List.map [1;3;5;6] ~f:(of_unsigned_int ~width:6))
- : t = 001111
```
 
 `tree` does much the same thing as `reduce`, except it forms the computation in a tree
 structure. Additional, the operation it performs can take more than 2 arguments as
 specified by an `arity` value.
 
```ocaml
# tree ~arity:4 (of_string "111111" |> bits_lsb)
    ~f:(function [a] -> a
               | [a;b] -> a &: b
               | [a;b;c] -> a &: b &: c
               | [a;b;c;d] -> a &: b &: c &: d
               | _ -> failwith "impossible")
- : t = 1
```

We commonly use `tree` and `reduce` together.

```ocaml
# tree ~arity:4 (of_string "111111" |> bits_lsb) ~f:(reduce ~f:(&:))
- : t = 1
```

## With_valid

`With_valid` is a record type with a 1 bit `valid` signal and an arbitrary `value` signal.
It is meant to work a little like an Option type where `valid=0` means `None `and
`valid=1` means `Some value`

## Priority based selectors

`priority_select` takes a list of `With_valids` and returns the first one whose `valid` is
high. It returns a single `With_valid` whose valid is low if no case was selected.

`priority_select_with_default` adds a default value which is output if no case is
selected.

```ocaml
# priority_select_with_default 
    ~default:(of_string "1111") 
    [ { valid = gnd; value = of_string "0001" }
    ; { valid = gnd; value = of_string "0100" }
    ]
- : t = 1111
# priority_select_with_default 
    ~default:(of_string "1111") 
    [ { valid = vdd; value = of_string "0001" }
    ; { valid = vdd; value = of_string "0100" }
    ]
- : t = 0001
```

## Counting bits

`popcount`, `leading_zeros`, `leading_ones`, `trailing_zeros`, `trailing_ones` all count
some number of bits within a vector. Their implementations are tree based and have `log
width` logic depth.

```ocaml
# popcount (of_string "1100011")
- : t = 100
# trailing_zeros (of_string "1110010100")
- : t = 0010
```

## Onehot and Gray

`binary_to_onehot` and `onehot_to_binary` convert between onehot and binary
representations.

Similarly for `gray_to_binary` and `binary_to_gray`.

```ocaml
# binary_to_onehot (of_string "110")
- : t = 01000000
# onehot_to_binary (of_string "01000")
- : t = 011
# binary_to_gray (of_string "110")
- : t = 101
# gray_to_binary (of_string "01000")
- : t = 01111
```

## Random

`random` creates a random constant vector of the given width. Mostly useful for
testbenches.

## Bits set

`any_bits_set` is equivalent to `x <>:. 0`.

`all_bits_set`is equivalent to `x ==:. (-1)`.

`no_bits_set` is equivalent to `x ==:. 0`.

## `With_zero_width`

We disallow zero width vectors in Hardcaml. `With_zero_width` allows us to represent them.
The type is `Comb.t option` where `None` indicates zero width.

The functions provided try to do sane things with zero width vectors.

```ocaml
# With_zero_width.(concat_msb [ None; ones 2; zero 0; zero 1; Some (of_string "1101") ])
- : With_zero_width.t = Some 1101101
```

## TypedMath

`Unsigned` and `Signed` are modules with signature `TypedMath`. They provide arithmetic
and comparison operators. Since they know the signedness of the type, they can, for
example, add or compare vectors of different widths.

Additionally, addition and subtraction grow their result width by one bit to ensure
overflow is impossible. This extra bit captures the carry that might result from the
operation.
