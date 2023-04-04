open! Import

module I : sig
  type 'a t =
    { clk : 'a
    ; clr : 'a
    ; enable : 'a
    ; d : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
end

val f : Signal.t list -> Signal.t I.t -> Signal.t O.t
