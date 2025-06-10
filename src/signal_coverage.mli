(** Coverage information about a single signal. *)

module Mux : sig
  type t [@@deriving sexp_of]

  include Coverage.S with type t := t

  val select_names : t -> Name_and_loc.t list
  val mark_choice : t -> int -> unit
  val case_count : t -> int
  val case_covered : t -> int -> bool
end

module Cases : sig
  module Case : sig
    type t =
      | Specified of int
      | Default
    [@@deriving sexp_of, to_string]
  end

  type t [@@deriving sexp_of]

  include Coverage.S with type t := t

  val select_names : t -> Name_and_loc.t list
  val mark_choice : t -> Case.t -> unit
  val cases : t -> Case.t list
  val case_covered : t -> Case.t -> bool
end

module Reg : sig
  type t [@@deriving sexp_of]

  include Coverage.S with type t := t

  val bits : t -> int
  val mark_toggled_bit : t -> bit:int -> on:bool -> unit
  val bit_toggled : t -> bit:int -> on:bool -> bool
end

type t' =
  | Mux of Mux.t
  | Reg of Reg.t
  | Cases of Cases.t
[@@deriving sexp_of]

module Debug_info : sig
  type t =
    { names : Name_and_loc.t list
    ; call_stack : Stack_slot.t list
    ; comment : string option
    }
  [@@deriving hash, compare, equal, sexp_of]

  val create : Signal.t -> t
end

type 'a with_metadata =
  { data : 'a
  ; id : Signal_graph.Normalized_signal_uid.t
  ; debug_info : Debug_info.t
  }
[@@deriving sexp_of]

type t = t' with_metadata [@@deriving sexp_of]

include Coverage.S with type t := t

val maybe_create : Signal_graph.Normalized_signal_uid.t -> Signal.t -> t option

module Grouped : sig
  type flat = t list

  type t =
    { muxes : Mux.t with_metadata list
    ; cases : Cases.t with_metadata list
    ; regs : Reg.t with_metadata list
    }
  [@@deriving sexp_of]

  val of_flat : flat -> t
end
