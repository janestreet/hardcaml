(** High level coverage stats *)

module type S = sig
  type t

  val fully_covered : t -> bool

  (** Number of observed cases that were not included in the 'full coverage set'. This can
      occur when coverage waivers are attached to signals or Always state machine state
      registers are assign unexpected values. *)
  val unexpectedly_observed_cases : t -> int
end

module type Coverage = sig
  module type S = S

  module Make (M : sig
      type t

      val total_cases : t -> int
      val covered_cases : t -> int

      (** Number of observed cases that were not included in the total cases set. For
          times when this can occur see [unexpectedly_observed_cases] above. *)
      val unexpectedly_observed_cases : t -> int
    end) : S with type t = M.t
end
