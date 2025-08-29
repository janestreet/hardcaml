(** Coverage metadata to be stored on a Signal *)

open Coverage_prim

module Kind : sig
  type t =
    | Variable of Always_metadata.Variable.t
    | If of Always_metadata.If.t
    | Switch_mux of Always_metadata.Switch_mux.t
    | Switch_cases of Always_metadata.Switch_cases.t
  [@@deriving bin_io, sexp_of]
end

module Waiver : sig
  type t =
    | Mux of int Waiver.t
    | Cases of Case.t Waiver.t
    | Reg of Toggle.t Waiver.t
    | Always_state of
        { state : string Waiver.t
        ; transition : string Transition.t Waiver.t
        }
  [@@deriving bin_io, sexp_of, to_string]

  val is_none : t -> bool
end

type t =
  { waiver : Waiver.t option
  ; kind : Kind.t option
  }
[@@deriving bin_io, sexp_of]

(** Kinds *)

val set_kind : t option -> Kind.t -> t

(** Waivers *)

val add_mux_waiver_exn : t option -> int Coverage_prim.Waiver.t -> t
val add_cases_waiver_exn : t option -> Case.t Coverage_prim.Waiver.t -> t
val add_register_waiver_exn : t option -> Toggle.t Coverage_prim.Waiver.t -> t
val add_always_state_waiver_exn : t option -> string Coverage_prim.Waiver.t -> t

val add_always_state_transition_waiver_exn
  :  t option
  -> string Transition.t Coverage_prim.Waiver.t
  -> t
