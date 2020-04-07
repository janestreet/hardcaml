(** Combinational logic API.

    This includes standard arithmetic, logical, equality and comparision operators, as
    well as more hardware specific functions such as bit selection, concatention,
    multiplexing etc.

    In operators, a trailing colon [:] indicates that the operator treats the bits as
    unsigned or that sign doesn't matter, while a trailing plus [+] indicates that
    the operator treats the bits as signed twos-complement. *)

include Comb_intf.Comb (** @inline *)
