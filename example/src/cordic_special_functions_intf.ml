open! Import

module Architecture = Cordic.Architecture
module Config       = Cordic.Config
module Mode         = Cordic.Mode
module System       = Cordic.System

module type Function = sig

  module Cordic : Cordic.S

  module Args : Interface.S

  module Results : sig
    type 'a t [@@deriving compare]
    include Interface.S with type 'a t := 'a t
  end

  module I : sig
    type 'a t =
      { clk    : 'a
      ; clr    : 'a
      ; enable : 'a
      ; ld     : 'a
      ; args   : 'a Args.t }

    [@@deriving sexp_of, hardcaml]
  end

  module O : Interface.S

  val create : Config.t -> Signal.t I.t -> Signal.t O.t

  val configure_input : Config.t -> Signal.t I.t -> Signal.t Cordic.I.t

  val configure_output : Config.t -> Signal.t Cordic.O.t -> Signal.t O.t

  module Sim : module type of Cyclesim.With_interface(I)(O)

  module Test : sig

    val combinational
      :  sim : Sim.t
      -> float Args.t
      -> float Results.t

    val pipelined
      :  iterations : int
      -> sim        : Sim.t
      -> float Args.t
      -> float Results.t

    val iterative
      :  iterations : int
      -> sim        : Sim.t
      -> float Args.t
      -> float Results.t
  end
end

module type Cordic_special_functions = sig

  module type Function = Function

  module Make (Fixnum_spec : Fixnum.Spec) : sig
    module Atan : sig
      module Args : sig
        type 'a t =
          { arg : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { angle : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Atan2 : sig
      module Args : sig
        type 'a t =
          { x : 'a
          ; y : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { angle : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Atanh : sig
      module Args : sig
        type 'a t =
          { arg : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { angle : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Cos_sin : sig
      module Args : sig
        type 'a t =
          { angle : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { cos : 'a
          ; sin : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Cosh_sinh : sig
      module Args : sig
        type 'a t =
          { angle : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { cosh : 'a
          ; sinh : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Div : sig
      module Args : sig
        type 'a t =
          { a : 'a
          ; b : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { quotient : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Mul : sig
      module Args : sig
        type 'a t =
          { a : 'a
          ; b : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { product : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Polar_to_rect : sig
      module Args : sig
        type 'a t =
          { magnitude : 'a
          ; phase     : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { x : 'a
          ; y : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Rect_to_polar : sig
      module Args : sig
        type 'a t =
          { x : 'a
          ; y : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { magnitude : 'a
          ; phase     : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end

    module Rotate_vector : sig
      module Args : sig
        type 'a t =
          { x     : 'a
          ; y     : 'a
          ; angle : 'a }
        [@@deriving sexp_of, hardcaml]
      end

      module Results : sig
        type 'a t =
          { xo : 'a
          ; yo : 'a }
        [@@deriving compare, sexp_of, hardcaml]
      end

      include Function
        with module Args := Args
        with module Results := Results
    end
  end
end
