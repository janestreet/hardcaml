open Core

module type S = sig
  type t [@@deriving compare ~localize, hash, sexp_of]

  include Comparator.S with type t := t

  include%template Equal.S [@mode local] with type t := t
  include%template Comparable.S [@mode local] with type t := t

  val zero : t
  val one : t
  val to_int : t -> int
  val to_string : t -> string
  val generator : unit -> [ `New of unit -> t ] * [ `Reset of unit -> unit ]

  module For_testing : sig
    val of_int : int -> t
  end
end

module type Uid_builder = sig
  module type S = S

  module Make () : S
end
