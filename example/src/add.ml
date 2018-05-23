open! Import

include Add_intf

module Make_gen (B : Gen) = struct

  module B = B

  open B

  module Add_result = struct
    type t =
      { carry : B.bit
      ; sum   : B.bit }
    [@@deriving sexp_of]
  end

  type add_result = Add_result.t =
    { carry : B.bit
    ; sum   : B.bit }

  let full_adder x y z : Add_result.t =
    { sum = x ^: y ^: z
    ; carry = (x &: y) |: (x &: z) |: (y &: z) }

  let half_adder x y : Add_result.t =
    { sum = x ^: y
    ; carry = x &: y }

  module Subtract_result = struct
    type t =
      { borrow : B.bit
      ; sub : B.bit }
    [@@deriving sexp_of]
  end

  type subtract_result = Subtract_result.t =
    { borrow : B.bit
    ; sub : B.bit }

  let subtractor x y z : Subtract_result.t =
    { sub = x ^: y ^: z
    ; borrow = ( (~: x) &: (y |: z) ) |: (x &: y &: z) }

  let ripple_carry_adder x y ~carry_in_bit =
    let c, s = List.fold2_exn
                 (List.rev (bits x)) (List.rev (bits y))
                 ~init:(carry_in_bit, [])
                 ~f:(fun (carry_in, sum_bits) x y ->
                   let { Add_result. carry; sum } = full_adder x y carry_in in
                   carry, sum :: sum_bits)
    in
    concat (c::s)
end

module Make (B : Comb.S) =
  Make_gen (struct
    include B
    type bit = t [@@deriving sexp_of]
  end)
