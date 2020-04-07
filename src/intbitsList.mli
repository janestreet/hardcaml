(** Bits described as lists of ints ie [0;1;1;1;0] - width implicit as length of list*)

open! Import
include Comb.S with type t = int list
