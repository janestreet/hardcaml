(** Uses a [valid] bit to indicate the validity of a [value].  Conceptually similar to an
    [Option.t]. *)

open Base

type ('a, 'b) t2 = ('a, 'b) Comb.with_valid2 =
  { valid : 'a
  ; value : 'b
  }
[@@deriving sexp, bin_io]

type 'a t = ('a, 'a) t2 [@@deriving sexp, bin_io]

val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val iter : 'a t -> f:('a -> unit) -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val to_list : 'a t -> 'a list
val map_valid : ('a, 'b) t2 -> f:('a -> 'c) -> ('c, 'b) t2
val map_value : ('a, 'b) t2 -> f:('b -> 'c) -> ('a, 'c) t2

(** Create a new hardcaml interface with type ['a With_valid.t X.t] *)
module Fields : sig
  module Make (X : Interface.Pre) : Interface.S with type 'a t = 'a t X.t

  module M (X : T1) : sig
    type nonrec 'a t = 'a t X.t

    module type S = Interface.S with type 'a t = 'a t
  end
end

(** Create a new hardcaml interface with type [('a, 'a X.t) With_valid.t2]. *)
module Wrap : sig
  module Make (X : Interface.Pre) : Interface.S with type 'a t = ('a, 'a X.t) t2

  module M (X : T1) : sig
    type nonrec 'a t = ('a, 'a X.t) t2

    module type S = Interface.S with type 'a t = 'a t
  end

  module type S = sig
    type 'a value

    include Interface.S with type 'a t = ('a, 'a value) t2
  end
end

module Vector (X : sig
    val width : int
  end) : Interface.S with type 'a t = 'a t
