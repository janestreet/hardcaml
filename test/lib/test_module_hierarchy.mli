(** Export the defined modules for use in other hierarchy-related tests *)
open! Import

module Inner : sig
  module I : sig
    type 'a t =
      { a : 'a
      ; b : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { c : 'a
      ; d : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Middle : sig
  module I = Inner.I

  module O : sig
    type 'a t =
      { o : 'a Inner.O.t array
      ; x : 'a
      }
    [@@deriving hardcaml]
  end

  val create : ?with_hierarchical_here:bool -> Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Outer : sig
  module I = Middle.I
  module O = Middle.O

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end
