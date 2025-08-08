(** Coverage information about a single signal. *)

open Coverage_prim

type 'a coverage =
  { observed : 'a list
  ; unexpectedly_observed : 'a list
  ; not_covered : 'a list
  }

val filter_coverage : 'a coverage -> f:('a -> bool) -> 'a coverage

module type S = sig
  include Coverage.S

  type choice
  type coverage_unit

  val mark_choice : t -> choice -> unit
  val coverage : t -> coverage_unit coverage
  val id : t -> Signal_graph.Normalized_signal_uid.t
  val signal_names : t -> Name_and_loc.t list
  val call_stack : t -> Call_stack.t
end

module Mux : sig
  module Debug_info : sig
    module Kind : sig
      type t =
        | Basic
        | If of Always_metadata.If.t
        | Switch of Always_metadata.Switch_mux.t
      [@@deriving sexp_of]
    end

    type t =
      { select_names : Name_and_loc.t list
      ; kind : Kind.t
      }
    [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  include S with type t := t and type choice = int and type coverage_unit = int

  val case_count : t -> int
  val debug_info : t -> Debug_info.t
end

module Cases : sig
  module Debug_info : sig
    module Kind : sig
      type t =
        | Basic
        | Switch of Always_metadata.Switch_cases.t
      [@@deriving sexp_of]
    end

    type t =
      { select_names : Name_and_loc.t list
      ; kind : Kind.t
      }
    [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  include
    S
    with type t := t
     and type choice = Case.Positional.t
     and type coverage_unit = Case.Positional_with_state.t

  val case_count : t -> int
  val debug_info : t -> Debug_info.t
end

module Reg : sig
  type t [@@deriving sexp_of]

  include S with type t := t and type choice = Toggle.t and type coverage_unit = Toggle.t

  val bits : t -> int
end

module Always_state : sig
  type t [@@deriving sexp_of]

  include
    S
    with type t := t
     and type choice = Transition.Value.t
     and type coverage_unit = Transition.State.t
end

type t =
  | Mux of Mux.t
  | Cases of Cases.t
  | Reg of Reg.t
  | Always_state of Always_state.t
[@@deriving sexp_of]

include Coverage.S with type t := t

val maybe_create : Signal_graph.Normalized_signal_uid.t -> Signal.t -> t option
val call_stack : t -> Call_stack.t
val call_stack_of_signal : Signal.t -> Call_stack.t

module Grouped : sig
  type flat = t list

  type t =
    { muxes : Mux.t list
    ; cases : Cases.t list
    ; regs : Reg.t list
    ; always_states : Always_state.t list
    }
  [@@deriving sexp_of]

  val of_flat : flat -> t
end
