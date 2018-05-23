(** Coordinate rotation digital computer.

    CORDIC is an iterative shift-add algorithm for computing trig and hyperbolic functions
    like sin, cosh, atan etc.

    Generally it requires 3 adders, 2 barrel shifters and a fairly small lookup table.

    {b Gain}

    The [x] and [y] outputs from the CORDIC are (usually) scaled by the CORDIC gain.  The
    exact value of the gain depends on the number of iterations performed, but tends
    towards about 1.647.

    For some functions the gain can be adjusted for by altering input constants.  Where it
    cannot, and it's a problem, it should be removed with a constant multiplier.

    {b Hyperbolic mode}

    In hyperbolic mode the standard iteration scheme needs to be adjusted.  Generally the
    iterations run from [0,1,...,(iters-1)].  In hyperbolic mode the iterations are
    [1,2,..,4,4,...13,13,...].  That is they start at 1 and iterations [3k+1] (starting
    with k=4, then 13, 40, 121 etc) are repeated.

    The hardware designs require a fixed number of iterations regardless of the mode.
    Therefore the number of iterations specified is exactly the number run regardless of
    mode (indices are modified internally in hyperbolic mode).  Some care might need to be
    taken to not stop processing in hyperbolic mode on one of the double iterations to
    ensure convergence. *)

open! Import

module Architecture = struct
  type t =
    | Combinational
    | Pipelined
    | Iterative
  [@@deriving sexp_of]
end

module Config = struct
  type t =
    { architecture : Architecture.t
    ; iterations   : int }
  [@@deriving sexp_of]
end

module type S = sig

  module I : sig
    type 'a t =
      { clk    : 'a
      ; clr    : 'a
      ; enable : 'a
      ; ld     : 'a
      ; system : 'a
      ; mode   : 'a
      ; c      : 'a
      ; x      : 'a
      ; y      : 'a
      ; z      : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { xo : 'a
      ; yo : 'a
      ; zo : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  val create
    :  Config.t
    -> Signal.t I.t
    -> Signal.t O.t
end

module type Cordic = sig

  module type S = S

  module Architecture = Architecture

  module Config = Config

  module System : sig
    include module type of struct include Cordic_reference.System end

    val to_signal : t -> Signal.t
  end

  module Mode : sig
    include module type of struct include Cordic_reference.Mode end

    val to_signal : t -> Signal.t
  end

  module Make (Fixnum_spec : Fixnum.Spec) : S
end
