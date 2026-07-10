(** An immutable finite sequence of bits with a specified width.

    It can be interpreted as an integer (signed or unsigned, one or twos complement) as
    required. *)

include Bits_intf.Bits (** @inline *)
