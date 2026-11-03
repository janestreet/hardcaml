open! Core0
include module type of Hardcaml_kernel.Wave_format

(** Convert [Bits.t] to a string representation depending on the required format.

    Staging is used to precompute arrays/maps to make the conversion operation faster. *)
val to_string : t -> (Bits.t -> string) Staged.t

(** Convert a [Four_state_logic.t] to a string representation depending on the required
    format. Pure values display exactly as {!to_string}. Impure [Binary] and [Bit] values
    use [0], [1], [X], and [Z] per bit; [Hex] values use [X] or [Z] per digit; other
    formats use one [X] or [Z] for the whole value (see {!Four_state_logic.Impurity}). *)
val to_string_four_state : t -> (Four_state_logic.t -> string) Staged.t

val default : t

(** Parse a short, case-insensitive format name. Accepted aliases:
    - [Binary]: "b", "bin", "binary"
    - [Hex]: "h", "hex"
    - [Unsigned_int]: "u", "uint", "unsigned", "unsigned-int"
    - [Int]: "s", "int", "sint", "signed", "signed-int"

    Only these basic formats are supported; compound formats like [Bit_or] are not
    supported. Raises on an unknown name. *)
val of_string_exn : string -> t
