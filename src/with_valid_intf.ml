open! Core0

(** Uses a [valid] bit to indicate the validity of a [value]. Conceptually similar to an
    [Option.t]. *)
module type With_valid = sig
  type ('a, 'b) t2 = ('a, 'b) Comb.with_valid2 =
    { valid : 'a
    ; value : 'b
    }
  [@@deriving sexp, bin_io, equal ~localize, compare ~localize]

  type 'a t = ('a, 'a) t2 [@@deriving sexp, equal ~localize, compare ~localize, bin_io]

  val valid : ('a, 'b) t2 -> 'a
  val value : ('a, 'b) t2 -> 'b
  val value_with_default : (module Comb.S with type t = 'a) -> 'a t -> default:'a -> 'a
  val map : 'a t -> f:('a -> 'b) @ local -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
  val iter : 'a t -> f:('a -> unit) @ local -> unit
  val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
  val to_list : 'a t -> 'a list
  val map_valid : ('a, 'b) t2 -> f:('a -> 'c) -> ('c, 'b) t2

  (** Applies the provided function to the value, keeping [valid] the same. Semantically
      similar to OCaml's [Option.map] *)
  val map_value : ('a, 'b) t2 -> f:('b -> 'c) -> ('a, 'c) t2

  (** Similar to [map_value], but for a function that takes two arguments. The result is
      [valid] if both arguments were valid. *)
  val map_value2
    :  (module Comb.S with type t = 'a)
    -> ('a, 'b) t2
    -> ('a, 'c) t2
    -> f:('b -> 'c -> 'd) @ local
    -> ('a, 'd) t2

  (** Applies the provided With_valid-returning function to the value. The result is
      [valid] if the initial argument was valid, and the value returned by the function is
      valid. Semantically similar to [Option.bind] *)
  val and_then
    :  (module Comb.S with type t = 'a)
    -> ('a, 'b) t2
    -> f:('b -> ('a, 'c) t2) @ local
    -> ('a, 'c) t2

  (** Similar to [and_then], but for a function that takes two arguments. The result is
      [valid] if both arguments are valid and the value returned by the function is valid. *)
  val and_then2
    :  (module Comb.S with type t = 'a)
    -> ('a, 'b) t2
    -> ('a, 'c) t2
    -> f:('b -> 'c -> ('a, 'd) t2) @ local
    -> ('a, 'd) t2

  val to_option : (Bits.t, 'a) t2 -> 'a option
  val wave_formats : Wave_format.t t
  val port_names_and_widths_dynamic : nbits:int -> (string * int) t

  (** Create a new hardcaml interface with type ['a With_valid.t X.t] *)
  module Fields : sig
    module type S = sig
      type 'a value
      type nonrec 'a t = 'a t value

      include Interface.S with type 'a t := 'a t

      val value_with_default
        :  (module Comb.S with type t = 'a)
        -> 'a t
        -> default:'a value
        -> 'a value

      val value : 'a t -> 'a value
    end

    module Make (X : Interface.Pre) : S with type 'a value = 'a X.t

    module M (X : T1) : sig
      module type S = S with type 'a value = 'a X.t
    end

    (** Convenient interface to create [module With_valid = ...] using the
        [include functor] extension. *)
    module Include : sig
      module type S = sig
        type 'a value

        module With_valid : S with type 'a value = 'a value
      end

      module type F = functor (X : Interface.Pre) -> S with type 'a value := 'a X.t

      module Make (X : Interface.Pre) : S with type 'a value := 'a X.t
    end
  end

  (** Create a new hardcaml interface with type [('a, 'a X.t) With_valid.t2]. *)
  module Wrap : sig
    module type S = sig
      type 'a value
      type nonrec 'a t = ('a, 'a value) t2

      include Interface.S with type 'a t := 'a t

      val value_with_default
        :  (module Comb.S with type t = 'a)
        -> 'a t
        -> default:'a value
        -> 'a value
    end

    module Make (X : Interface.Pre) : S with type 'a value = 'a X.t

    module M (X : T1) : sig
      type nonrec 'a t = ('a, 'a X.t) t2

      module type S = S with type 'a value = 'a X.t
    end

    (** Convenient interface to create [module With_valid = ...] using the
        [include functor] extension. *)
    module Include : sig
      module type S = sig
        type 'a value

        module With_valid : S with type 'a value = 'a value
      end

      module type F = functor (X : Interface.Pre) -> S with type 'a value := 'a X.t

      module Make (X : Interface.Pre) : S with type 'a value := 'a X.t
    end
  end

  module Vector (X : sig
      val width : int
    end) : Interface.S with type 'a t = 'a t

  (** Helper functions, similar to those of Of_signal in [Interface.S], to ease usage of
      [Signal.t t]. Function types differ from Of_signal where the width of [value] is
      needed. *)
  module Of_signal : sig
    (** Creates a With_valid of wires with a specified [value] width, e.g.
        [Of_signal.wires 5] creates a With_valid where [valid] is a 1-bit wire and [value]
        is a 5-bit wire. If [named] is true then wires are given their respective
        With_valid field names. If [from] is provided the wires are attached to the fields
        in [from], and an error is raised if the provided width does not match the [value]
        width in [from]. *)
    val wires
      :  ?named:bool (** Default is [false]. *)
      -> ?from:Signal.t t (** No default. *)
      -> int
      -> Signal.t t

    val __ppx_auto_name : Signal.t t -> string -> Signal.t t
  end

  (** Helper functions, similar to those of Of_always in [Interface.S], to ease usage of
      [Always.Variable.t t]. Function types differ from Of_always where the width of
      [value] is needed. *)
  module Of_always : sig
    (** Creates a With_valid with wire variables, where [width] specifies the bitwidth of
        [value]. E.g., [Of_always.wire (Signal.zero) ~width:5] creates a With_valid where
        [valid] is a 1-bit wire variable and [value] is a 5-bit wire variable, both
        defaulting to zeros. *)
    val wire : (int -> Signal.t) -> width:int -> Always.Variable.t t

    (** Creates a With_valid with register variables, where [width] specicies the bitwidth
        of [value]. *)
    val reg : (width:int -> Always.Variable.t t) Signal.with_register_spec

    val __ppx_auto_name : Always.Variable.t t -> string -> Always.Variable.t t
  end
end
