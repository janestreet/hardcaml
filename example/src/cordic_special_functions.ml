open! Import

include Cordic_special_functions_intf

let gain  (config : Config.t) = Cordic_reference.gain ~iterations:config.iterations
let gainh (config : Config.t)  = Cordic_reference.gainh ~iterations:config.iterations

module Make (Fixnum_spec : Fixnum.Spec) = struct

  module Cordic = Cordic.Make (Fixnum_spec)
  module Fixnum = Fixnum.Make (Fixnum_spec)

  module Generic_args = struct
    type 'a t =
      { x : 'a
      ; y : 'a
      ; z : 'a }
    [@@deriving sexp_of]
  end

  module Make_function
      (M : sig
         val system : System.t
         val mode : Mode.t
         module Args : sig
           include Interface.S
           val to_generic : Signal.t t -> Config.t -> Signal.t Generic_args.t
         end
         module Results : sig
           include Interface.S
           val create
             :  Config.t
             -> xo:Signal.t
             -> yo:Signal.t
             -> zo:Signal.t
             -> Signal.t t
         end
       end) = struct

    open M

    module Cordic = Cordic

    module I = struct
      type 'a t =
        { clk    : 'a
        ; clr    : 'a
        ; enable : 'a
        ; ld     : 'a
        ; args   : 'a Args.t }
      [@@deriving sexp_of, hardcaml]
    end

    let configure_input config { I. clk; clr; enable; ld; args } =
      let { Generic_args. x; y; z } = Args.to_generic args config in
      let system = System.to_signal system in
      let mode = Mode.to_signal mode in
      let c = Fixnum.signal_constf 0. in
      { Cordic.I. clk; clr; enable; ld; system; mode; c; x; y; z }
    ;;

    let configure_output config { Cordic.O. xo; yo; zo } =
      Results.create config ~xo ~yo ~zo
    ;;

    let create config i =
      configure_input config i
      |> Cordic.create config
      |> configure_output config
    ;;

    module O = Results

    module Sim = Cyclesim.With_interface(I)(O)

    module Test = struct

      let to_float bits = bits |> Fixnum.of_bits |> Fixnum.to_float

      let set_input ~sim args =
        let i : _ I.t = Cyclesim.inputs sim in
        Args.iter2 i.args args ~f:(fun r a ->
          r := Fixnum.bits_constf a)

      let get_output ~sim =
        let o : _ O.t = Cyclesim.outputs sim in
        O.map o ~f:(fun r -> to_float !r)

      let combinational ~sim args =
        set_input ~sim args;
        Cyclesim.reset sim;
        Cyclesim.cycle sim;
        get_output ~sim

      let pipelined ~iterations ~sim args =
        let i : _ I.t = Cyclesim.inputs sim in
        Cyclesim.reset sim;
        i.enable := Bits.vdd;
        set_input ~sim args;
        for _ = 1 to iterations do
          Cyclesim.cycle sim;
        done;
        get_output ~sim

      let iterative ~iterations ~sim args =
        let i : _ I.t = Cyclesim.inputs sim in
        Cyclesim.reset sim;
        i.enable := Bits.vdd;
        i.ld := Bits.vdd;
        set_input ~sim args;
        Cyclesim.cycle sim;
        i.ld := Bits.gnd;
        for _ = 1 to iterations do
          Cyclesim.cycle sim;
        done;
        get_output ~sim
    end
  end

  module Atan = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Vectoring

      module Args = struct
        type 'a t =
          { arg : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { arg } _ : _ Generic_args.t =
          { x = Fixnum.signal_constf 1.
          ; y = arg
          ; z = Fixnum.signal_constf 0. }
      end

      module Results = struct
        type 'a t =
          { angle : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo:_ ~yo:_ ~zo = { angle = zo }
      end
    end

    include T
    include Make_function (T)
  end

  module Atan2 = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Vectoring

      module Args = struct
        type 'a t =
          { x : 'a [@bits Fixnum.width]
          ; y : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { x; y } _ : _ Generic_args.t =
          { x = y
          ; y = x
          ; z = Fixnum.signal_constf 0. }
      end

      module Results = struct
        type 'a t =
          { angle : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo:_ ~yo:_ ~zo = { angle = zo }
      end
    end

    include T
    include Make_function (T)
  end

  module Atanh = struct
    module T = struct
      let system = System.Hyperbolic
      let mode   = Mode.Vectoring

      module Args = struct
        type 'a t =
          { arg : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { arg } _ : _ Generic_args.t =
          { x = Fixnum.signal_constf 1.
          ; y = arg
          ; z = Fixnum.signal_constf 0. }
      end

      module Results = struct
        type 'a t =
          { angle : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo:_ ~yo:_ ~zo = { angle = zo }
      end
    end

    include T
    include Make_function (T)
  end

  module Cos_sin = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Rotation

      module Args = struct
        type 'a t =
          { angle : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { angle } config : _ Generic_args.t =
          { x = Fixnum.signal_constf (1. /. gain config)
          ; y = Fixnum.signal_constf 0.
          ; z = angle }
      end

      module Results = struct
        type 'a t =
          { cos : 'a [@bits Fixnum.width]
          ; sin : 'a [@bits Fixnum.width]}
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo ~yo ~zo:_ = { cos = xo; sin = yo }
      end
    end

    include T
    include Make_function (T)
  end

  module Cosh_sinh = struct
    module T = struct
      let system = System.Hyperbolic
      let mode   = Mode.Rotation

      module Args = struct
        type 'a t =
          { angle : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { angle } config : _ Generic_args.t =
          { x = Fixnum.signal_constf (1. /. gainh config)
          ; y = Fixnum.signal_constf 0.
          ; z = angle }
      end

      module Results = struct
        type 'a t =
          { cosh : 'a [@bits Fixnum.width]
          ; sinh : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo ~yo ~zo:_ = { cosh = xo; sinh = yo }
      end
    end

    include T
    include Make_function (T)
  end

  module Div = struct
    module T = struct
      let system = System.Linear
      let mode   = Mode.Vectoring

      module Args = struct
        type 'a t =
          { a : 'a [@bits Fixnum.width]
          ; b : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { a; b } _ : _ Generic_args.t =
          { x = b
          ; y = a
          ; z = Fixnum.signal_constf 0. }
      end

      module Results = struct
        type 'a t =
          { quotient : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo:_ ~yo:_ ~zo = { quotient = zo }
      end
    end

    include T
    include Make_function (T)
  end

  module Mul = struct
    module T = struct
      let system = System.Linear
      let mode   = Mode.Rotation

      module Args = struct
        type 'a t =
          { a : 'a [@bits Fixnum.width]
          ; b : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { a; b } _ : _ Generic_args.t =
          { x = a
          ; y = Fixnum.signal_constf 0.
          ; z = b }
      end

      module Results = struct
        type 'a t =
          { product : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo:_ ~yo ~zo:_ = { product = yo }
      end
    end

    include T
    include Make_function (T)
  end

  module Polar_to_rect = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Rotation

      module Args = struct
        type 'a t =
          { magnitude : 'a [@bits Fixnum.width]
          ; phase     : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { magnitude; phase } config : _ Generic_args.t =
          { x = Fixnum.signal_mul magnitude (Fixnum.signal_constf (1. /. gain config))
          ; y = Fixnum.signal_constf 0.
          ; z = phase }
      end

      module Results = struct
        type 'a t =
          { x : 'a [@bits Fixnum.width]
          ; y : 'a [@bits Fixnum.width]}
        [@@deriving compare, sexp_of, hardcaml]

        let create _ ~xo ~yo ~zo:_ = { x = xo; y = yo }
      end
    end

    include T
    include Make_function (T)
  end

  module Rect_to_polar = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Vectoring

      module Args = struct
        type 'a t =
          { x : 'a [@bits Fixnum.width]
          ; y : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { x; y } _ : _ Generic_args.t =
          { x
          ; y
          ; z = Fixnum.signal_constf 0. }
      end

      module Results = struct
        type 'a t =
          { magnitude : 'a [@bits Fixnum.width]
          ; phase     : 'a [@bits Fixnum.width] }
        [@@deriving compare, sexp_of, hardcaml]

        let create config ~xo ~yo:_ ~zo =
          { magnitude = Fixnum.signal_mul xo (Fixnum.signal_constf (1. /. gain config))
          ; phase = zo }
      end
    end

    include T
    include Make_function (T)
  end

  module Rotate_vector = struct
    module T = struct
      let system = System.Circular
      let mode   = Mode.Rotation

      module Args = struct
        type 'a t =
          { x     : 'a [@bits Fixnum.width]
          ; y     : 'a [@bits Fixnum.width]
          ; angle : 'a [@bits Fixnum.width] }
        [@@deriving sexp_of, hardcaml]

        let to_generic { x; y; angle } _ : _ Generic_args.t =
          { x
          ; y
          ; z = angle }
      end

      module Results = struct
        type 'a t =
          { xo : 'a [@bits Fixnum.width]
          ; yo : 'a [@bits Fixnum.width]}
        [@@deriving compare, sexp_of, hardcaml]

        let create config ~xo ~yo ~zo:_ =
          let igain = 1. /. gain config in
          { xo = Fixnum.signal_mul xo (Fixnum.signal_constf igain)
          ; yo = Fixnum.signal_mul yo (Fixnum.signal_constf igain) }
      end
    end

    include T
    include Make_function (T)
  end
end
