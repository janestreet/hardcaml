open! Base

module T = struct
  type 'a t =
    { lhs : 'a
    ; rhs : 'a
    }
  [@@deriving hardcaml]
end

module M (Data : Interface.S) = struct
  module type S = sig
    type nonrec 'a t = 'a Data.t T.t [@@deriving sexp_of]

    include Interface.S with type 'a t := 'a t
  end
end

module type Pair = sig
  module M = M

  type 'a t = 'a T.t =
    { lhs : 'a
    ; rhs : 'a
    }

  module Wrap (Data : Interface.S) : M(Data).S
end
