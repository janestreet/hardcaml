open! Import

module type MakePureCombTransform_arg = sig
  include Comb.Primitives

  val wire : int -> t
  val ( <== ) : t -> t -> unit
end

module type Transform = sig
  type 'a transform_fn' = (Signal.Uid.t -> 'a) -> Signal.t -> 'a
  type transform_fn = Signal.t transform_fn'

  module type TransformFn' = sig
    type t

    val transform : t transform_fn'
    val rewrite : t transform_fn' -> Signal.t Signal.Uid_map.t -> Signal.t list -> t list
    val rewrite_signals : t transform_fn' -> Signal.t list -> t list
  end

  module type TransformFn = sig
    (** function which will map signals to a new representation *)
    val transform : transform_fn
  end

  module type MakePureCombTransform_arg = MakePureCombTransform_arg

  (** functor to build the function to map a signal to a new combinatorial signal
      representation *)
  module MakePureCombTransform (B : MakePureCombTransform_arg) :
    TransformFn' with type t = B.t

  (** functor to build the function to map a signal to a new combinatorial signal
      representation *)
  module MakeCombTransform (B : Comb.Primitives with type t = Signal.t) : TransformFn

  (** simple copying transform *)
  module CopyTransform : TransformFn

  (** rewrites the list of signals based on the given function *)
  val rewrite_signals : transform_fn -> Signal.t list -> Signal.t list
end
