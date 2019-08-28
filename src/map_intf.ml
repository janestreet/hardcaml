(** An extension of [Base.Map] with a [Make] functor that builds a map module with [type
    t] and [val empty].  This reduces verbosity, allowing one to write [Foo_map.t] and
    [Foo_map.empty] rather than [Map.M(Foo).t] and [Map.empty (module Foo)]. *)

open! Import

module type Key = sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end

module type S = sig
  module Key : Key

  type 'a t = 'a Base.Map.M(Key).t [@@deriving sexp_of]

  val empty : 'a t
end

module type Map = sig
  include module type of struct
    include Base.Map
  end

  module type Key = Key
  module type S = S

  module Make (Key : Key) : S with module Key := Key
end

