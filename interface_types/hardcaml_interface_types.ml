open! Core

module type Pre_partial = sig
  type 'a t [@@deriving equal ~localize, compare ~localize, sexp_of]

  val iter : 'a t -> f:('a -> unit) @ local -> unit
  val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
  val map : 'a t -> f:('a -> 'b) @ local -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
  val to_list : 'a t -> 'a list
end

module type Pre = sig
  include Pre_partial

  val port_names_and_widths : (string * int) t
end
