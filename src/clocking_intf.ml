module type Functions = sig
  module Signal : Signal.S
  module Always : Always.S with module Signal := Signal

  type 'a t

  val add_clear : Signal.t t -> Signal.t -> Signal.t t
  val to_spec : Signal.t t -> Signal.Reg_spec.t
  val to_spec_no_clear : Signal.t t -> Signal.Reg_spec.t

  val reg
    :  Signal.t t
    -> ?enable:Signal.t
    -> ?clear:Signal.t
    -> ?clear_to:Signal.t
    -> Signal.t
    -> Signal.t

  val cut_through_reg
    :  Signal.t t
    -> ?clear:Signal.t
    -> ?clear_to:Signal.t
    -> enable:Signal.t
    -> Signal.t
    -> Signal.t

  val reg_no_clear : Signal.t t -> ?enable:Signal.t -> Signal.t -> Signal.t

  val pipeline
    :  ?attributes:Rtl_attribute.t list
    -> ?enable:Signal.t
    -> ?clear:Signal.t
    -> ?clear_to:Signal.t
    -> Signal.t t
    -> n:int
    -> Signal.t
    -> Signal.t

  val pipeline_no_clear
    :  ?attributes:Rtl_attribute.t list
    -> ?enable:Signal.t
    -> Signal.t t
    -> n:int
    -> Signal.t
    -> Signal.t

  val reg_fb
    :  ?enable:Signal.t
    -> ?clear:Signal.t
    -> ?clear_to:Signal.t
    -> Signal.t t
    -> width:int
    -> f:(Signal.t -> Signal.t)
    -> Signal.t

  val reg_fb_no_clear
    :  ?enable:Signal.t
    -> ?clear:Signal.t
    -> ?clear_to:Signal.t
    -> Signal.t t
    -> width:int
    -> f:(Signal.t -> Signal.t)
    -> Signal.t

  module Var : sig
    val reg
      :  ?enable:Signal.t
      -> ?clear:Signal.t
      -> ?clear_to:Signal.t
      -> Signal.t t
      -> width:int
      -> Always.Variable.t

    val reg_no_clear : ?enable:Signal.t -> Signal.t t -> width:int -> Always.Variable.t

    val cut_through_reg
      :  ?enable:Signal.t
      -> ?clear:Signal.t
      -> ?clear_to:Signal.t
      -> Signal.t t
      -> width:int
      -> Always.Variable.t

    val reg_with_int_default
      :  ?enable:Signal.t
      -> ?clear:Signal.t
      -> Signal.t t
      -> width:int
      -> clear_to:int
      -> Always.Variable.t
  end

  (** Utility functions for clock domain crossings. *)
  module Cdc : sig
    (** Take a single cycle input pulse, and stretch it for [n] cycles. The output goes
        high on the same cycle as the input. *)
    val stretch : Signal.t t -> n:int -> Signal.t -> Signal.t

    val stretch_no_clear : Signal.t t -> n:int -> Signal.t -> Signal.t

    (** Take a [Signal.t With_valid.t] that is in a slower clock and convert the [valid]
        pulse to a pulse in the faster clock. *)
    val with_valid_pulse_detect_rising_edge
      :  Signal.t t
      -> Signal.t With_valid.t
      -> Signal.t With_valid.t

    (** Create [1 + num_additional_pipeline_stages] registers in a pipeline with
        [async_reg] attribute set. Note this does not add any false path or timing
        constraints, the user needs to do that separately. *)
    val reg_no_clear_with_async_reg_annotation
      :  num_additional_pipeline_stages:int
      -> Signal.t t
      -> Signal.t
      -> Signal.t
  end
end

module Untyped = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml]
end

module type S = sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml]

  val to_untyped : 'a t -> 'a Untyped.t
  val of_untyped : 'a Untyped.t -> 'a t

  include
    Functions
    with module Signal := Signal
     and module Always := Always
     and type 'a t := 'a t

  module Clocked : sig
    type nonrec 'a t = 'a t =
      { clock : 'a
      ; clear : 'a
      }

    (** Interface is included here with the type reexported intentionally. This is so that
        users can write [module Clocking = Clocking.Clocked] at the top of modules that
        just use the Clocked_signal.t versions of the functions above. *)
    include Interface.S with type 'a t := 'a t

    include
      Functions
      with module Signal := Clocked_signal
       and module Always := Always.Clocked
      with type 'a t := 'a t
  end
end

module type Clocking = sig
  module type S = S

  module Make () : S
  include S with type 'a t = 'a Untyped.t
end
