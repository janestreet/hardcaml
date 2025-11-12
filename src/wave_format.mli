(** How to display the value of a signal. Generally used in waveforms. *)

open! Core0

type t =
  | Binary (** Binary. *)
  | Bit (** Like [Binary], but shows a single bit as a line. *)
  | Bit_or of t (** If [width=1] then [Bits] otherwise [t]. *)
  | Hex (** Hexadecimal. *)
  | Unsigned_int (** Unsigned integer. *)
  | Int (** Signed integer. *)
  | Index of string list (** Use data as index into given list of strings. *)
  | Custom of (Bits.t -> string) (** User defined formatting. *)
  | Map of (Bits.t, string) List.Assoc.t (** Map from Bits.t to string. *)
[@@deriving bin_io, sexp_of]

(** [Custom] constructors are compared for physical equality. *)
include%template Equal.S [@mode local] with type t := t

(** Convert [Bits.t] to a string representation depending on the required format.

    Staging is used to precompute arrays/maps to make the conversion operation faster. *)
val to_string : t -> (Bits.t -> string) Staged.t

val default : t
