(** Hardware architecture specification. *)

(** This type is for use in hardware designs where we may have a number of different
    implementations, and the choice of which is best depends on the use case. *)
type t =
  | Small (** Smallest size *)
  | Balanced (** Best balance between size and speed *)
  | Fast (** Fastest speed *)
[@@deriving sexp]

val of_string : string -> t

(** Default is set to balanced. *)
val default : t
