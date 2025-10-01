# 5.5.1 Typed ALU

We saw a simple implementation of an 8 bit [ALU](alu.md) before. With interfaces and
enums we can now express it as follows:

<!-- $MDX file=./lib/combinational_examples.ml,part=typed_alu -->
```ocaml
  module Op = struct
    module Enum = struct
      type t =
        | Add
        | Sub
        | Mul
        | Sll
        | Srl
        | And
        | Or
        | Xor
        | Not
        | Less
        | Equal
      [@@deriving sexp_of, compare ~localize, enumerate]
    end

    include Hardcaml.Enum.Make_enums (Enum)
  end

  module I = struct
    type 'a t =
      { op : 'a Op.Binary.t
      ; a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let typed_alu ({ op; a; b } : _ I.t) =
    { O.q =
        Op.Binary.match_
          (module Signal)
          ~default:(zero 8)
          op
          [ Add, a +: b
          ; Sub, a -: b
          ; Mul, (a *: b).:[7, 0]
          ; Srl, srl a ~by:1
          ; Sll, sll a ~by:1
          ; And, a &: b
          ; Or, a |: b
          ; Xor, a ^: b
          ; Not, ~:a
          ; Less, uresize (a <: b) ~width:8
          ; Equal, uresize (a ==: b) ~width:8
          ]
    }
  ;;
```
