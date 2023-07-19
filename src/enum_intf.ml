open Base

(** An enumerated type (generally a variant type with no arguments) which should derive
    [compare, enumerate, sexp_of, variants]. *)
module type Cases = sig
  type t [@@deriving compare, enumerate, sexp_of]
end

(** Functions to project an [Cases] type into and out of hardcaml bit vectors representated
    as an interface. *)
module type S_enum = sig
  module Cases : Cases
  include Interface.S

  val ast : Interface.Ast.t
  val of_enum : (module Comb.S with type t = 'a) -> Cases.t -> 'a t
  val to_enum : Bits.t t -> Cases.t Or_error.t
  val to_enum_exn : Bits.t t -> Cases.t
  val ( ==: ) : (module Comb.S with type t = 'a) -> 'a t -> 'a t -> 'a

  val match_
    :  (module Comb.S with type t = 'a)
    -> ?default:'a
    -> 'a t
    -> (Cases.t * 'a) list
    -> 'a

  val to_raw : 'a t -> 'a

  type 'a outer := 'a t

  module Of_signal : sig
    include module type of Of_signal (** @inline *)

    (** Tests for equality between two enums. For writing conditional statements
        based on the value of the enum, consider using [match_] below, or
        [Of_always.match_] instead
    *)
    val ( ==: ) : t -> t -> Signal.t

    (** Create an Cases value from a statically known value. *)
    val of_enum : Cases.t -> Signal.t outer

    (** Creates a Cases value from a raw value. Note that this only performs a
        check widths, and does not generate circuitry to validate that the input
        is valid. See documentation on Casess for more information.
    *)
    val of_raw : Signal.t -> Signal.t outer

    (** Multiplex on an enum value. If there are unhandled cases, a [default]
        needs to be specified.
    *)
    val match_
      :  ?default:Signal.t
      -> Signal.t outer
      -> (Cases.t * Signal.t) list
      -> Signal.t

    (** Convenient wrapper around [eq x (of_enum Foo)] *)
    val is : t -> Cases.t -> Signal.t
  end

  module Of_bits : sig
    include module type of Of_bits (** @inline *)

    val is : t -> Cases.t -> Bits.t
    val ( ==: ) : t -> t -> Bits.t
    val of_enum : Cases.t -> Bits.t outer
    val of_raw : Bits.t -> Bits.t outer
    val match_ : ?default:Bits.t -> Bits.t outer -> (Cases.t * Bits.t) list -> Bits.t
  end

  module Of_always : sig
    include module type of Of_always (** @inline *)

    (** Performs a "pattern match" on a [Signal.t t], and "executes" the branch that
        matches the signal value. Semantics similar to [switch] in verilog.
    *)
    val match_
      :  ?default:Always.t list
      -> Signal.t t
      -> (Cases.t * Always.t list) list
      -> Always.t
  end

  module Make_comb (X : Comb.S) : sig
    include module type of Make_comb (X) (** @inline *)

    val is : t -> Cases.t -> comb
    val ( ==: ) : t -> t -> comb
    val of_enum : Cases.t -> comb outer
    val of_raw : comb -> comb outer
    val match_ : ?default:comb -> comb outer -> (Cases.t * comb) list -> comb
  end

  (** Set an input port in simulation to a concrete Cases value. *)
  val sim_set : Bits.t ref t -> Cases.t -> unit

  (** Similar to [sim_set], but operates on raw [Bits.t] instead. *)
  val sim_set_raw : Bits.t ref t -> Bits.t -> unit

  (** Read an output port from simulation to a concreate Cases value.
      Returns [Ok enum] when the [Bits.t] value can be parsed, and
      [Error _] when the value is unhandled.
  *)
  val sim_get : Bits.t ref t -> Cases.t Or_error.t

  (** Equivalent to [ok_exn (sim_get x)] *)
  val sim_get_exn : Bits.t ref t -> Cases.t

  (** Similar to [sim_get], but operates on raw [Bits.t] instead. This
      doesn't return [_ Or_error.t]. Undefined values will be returned as
      it is.
  *)
  val sim_get_raw : Bits.t ref t -> Bits.t

  val unwrap : 'a t -> 'a

  module Unsafe : sig
    val wrap : 'a -> 'a t
  end
end

(** Binary and onehot selectors for [Casess]. *)
module type S_enums = sig
  module Cases : Cases
  module Binary : S_enum with module Cases := Cases
  module One_hot : S_enum with module Cases := Cases
end

module type Enum = sig
  module type S_enum = S_enum
  module type S_enums = S_enums

  (** Constructs a hardcaml interface which represents hardware for the given [Enum] as an
      abstract [Interface], using a Binary internal representation. *)
  module Make_binary (Cases : Cases) : S_enum with module Cases := Cases

  (** Similar to [Make_binary], but using a one hot representation internally. *)
  module Make_one_hot (Cases : Cases) : S_enum with module Cases := Cases

  (** [Make_enums] is semantically similar to:

      {[
        module Binary = Make_binary(Cases)
        module One_hot = Make_one_host(Cases)
      ]}
  *)
  module Make_enums (Cases : Cases) : S_enums with module Cases := Cases
end
