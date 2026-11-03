open! Import

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; enable : 'a
    ; d : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { q : 'a } [@@deriving hardcaml]
end

val f : Signal.t list -> Signal.t I.t -> Signal.t O.t
