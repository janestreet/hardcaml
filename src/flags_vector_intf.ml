(** Device a hardcaml interfaces for bit flags from an enumeration.

    From software, the interface can be controlled using normal flags.
*)

open Base

module type Cases = sig
  type t [@@deriving sexp_of, compare, enumerate]

  val port_name : string
end

module type S = sig
  type cases

  include Scalar.S

  module Flags : sig
    type t = private Int63.t [@@deriving sexp_of]

    include Flags.S with type t := t

    val of_cases : cases -> t
    val to_string : t -> string
  end

  val number_of : int
  val init : (module Comb.S with type t = 'a) -> (cases -> 'a) -> 'a t
  val of_bits : (module Comb.S with type t = 'a) -> 'a -> 'a t
  val to_bits : 'a t -> 'a
  val of_flags : (module Comb.S with type t = 'a) -> Flags.t -> 'a t
  val to_flags : Bits.t t -> Flags.t
  val of_cases_list : (module Comb.S with type t = 'a) -> cases list -> 'a t
  val to_cases_list : Bits.t t -> cases list
  val invert : (module Comb.S with type t = 'a) -> 'a t -> 'a t
  val add : (module Comb.S with type t = 'a) -> 'a t -> 'a t -> 'a t
  val remove : (module Comb.S with type t = 'a) -> 'a t -> 'a t -> 'a t
  val is_set : (module Comb.S with type t = 'a) -> 'a t -> cases -> 'a
  val mux2 : (module Comb.S with type t = 'a) -> 'a -> 'a t -> 'a t -> 'a t
  val deref : 'a ref t -> 'a t

  (** [iter_flags] calls [f] for each flag where [is_set] is true. **)
  val iter_flags : Flags.t -> f:(cases -> unit) -> unit
end

module type Flags_vector = sig
  module type Cases = Cases
  module type S = S

  module Make (Cases : Cases) : S with type cases = Cases.t
end
