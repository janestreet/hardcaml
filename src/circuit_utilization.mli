(** Utilization information for a circuit which can be printed to a sexp.

    It tries to balance summarizing different node types to provide an overview of both
    usage and the potential implication on the critical path without being overly
    detailed.

    [and], [or], [xor] and [not] gates just print the total number of bits.

    [adder], [subtractors], [multipliers] and [comparators] show the total number of bits
    and the largest single node.

    [multiplexers] are grouped by depth and show the total number of bits and largest
    data width in each group.

    [memories] are grouped by their aspect ratio - data width and depth.

    [instantiations] are recursively processed if present in an optional
    [Circuit_database.t]. *)

open! Import

type t [@@deriving sexp_of]

(** Calculate the utilization of gates, rams, registers. If [database] is provided
    instantiations are recursively calculated as well. *)
val create : ?database:Circuit_database.t -> Circuit.t -> t
