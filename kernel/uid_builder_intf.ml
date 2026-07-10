open! Core0

module type S = sig
  type t [@@deriving bin_io, compare ~localize, hash, sexp_of]

  include Comparator.S with type t := t

  include%template Equal.S [@mode local] with type t := t
  include%template Comparable.S [@mode local] with type t := t
  include%template Hashable.S_binable [@mode local] with type t := t

  val zero : t
  val one : t
  val to_int : t -> int
  val to_string : t -> string
  val generator : unit -> [ `New of unit -> t ] * [ `Reset of unit -> unit ]

  module Expert : sig
    (** Only use this if you know what you are doing. Not using the generator properly can
        lead to very strange problems. *)
    val of_int : int -> t
  end
end

module type Uid_builder = sig
  module type S = S

  module Make () : S
end
