open! Core0

val write
  :  Out_channel.t
  -> Circuit_coverage.t list
  -> hide_unstable:bool
       (** Hide unstable output such as code source line. This is useful when running as
           an expect test. *)
  -> verbose:bool
       (** Display coverage information for all signals, not just signals without
           coverage. *)
  -> compact:bool
       (** Remove headers and flatten results into a single list of uncovered signals.
           This is useful for documentation. *)
  -> unit
