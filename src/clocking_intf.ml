module type S = sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml]

  val add_clear : Signal.t t -> Signal.t -> Signal.t t
  val to_spec : Signal.t t -> Reg_spec.t
  val to_spec_no_clear : Signal.t t -> Reg_spec.t
  val reg : Signal.t t -> ?enable:Signal.t -> Signal.t -> Signal.t
  val reg_no_clear : Signal.t t -> ?enable:Signal.t -> Signal.t -> Signal.t

  val pipeline
    :  ?attributes:Rtl_attribute.t list
    -> ?enable:Signal.t
    -> Signal.t t
    -> n:int
    -> Signal.t
    -> Signal.t

  val reg_fb
    :  ?enable:Signal.t
    -> Signal.t t
    -> width:int
    -> f:(Signal.t -> Signal.t)
    -> Signal.t

  module Var : sig
    val reg : ?enable:Signal.t -> Signal.t t -> width:int -> Always.Variable.t

    val reg_with_default
      :  ?enable:Signal.t
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
        [async_reg] attribute set *)
    val reg_no_clear_with_async_reg_annotation
      :  num_additional_pipeline_stages:int
      -> Signal.t t
      -> Signal.t
      -> Signal.t
  end
end

module type Clocking = sig
  module type S = S

  module Make () : S
  include S
end
