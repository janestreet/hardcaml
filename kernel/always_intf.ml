(** [Always] is a DSL that lets one describe a circuit in the same style as a Verliog
    [always] block.

    [if] and [switch] control constructs are provided. (<--) is used for assignment.

    Code is written as lists of assignments, if and control statements.

    variables;

    {[
      let r_sync = Reg_spec.create ~clock ~clear in
      let var = wire (zero 8) in
      let var = reg r_sync enable 8 in
    ]}

    assignment;

    {[
      var <-- exp
    ]}

    if statements;

    {[
      if_ condition [ ... ] [ ... ]
    ]}

    switch statements;

    {[
      switch condition [
        of_int ~width:3 0, [ ... ];
        of_int ~width:3 1, [ ... ];
        of_int ~width:3 2, [ ... ];
        of_int ~width:3 3, [ ... ];
      ]
    ]}

    signals;

    {[
      let (s:signal) = q (v:guarded) in
    ]}

    compilation;

    {[
      compile [ ... ]
    ]}

    example;

    {v
      let state = reg r_sync enable 2 in
      let a = wire 8 in
      compile [
        if_ (a.value ==:. 4) [
          a <-- of_int ~width:8 2
        ] [
          switch state.value [
          (of_int ~width:2 0) [
            a <--. 3;
            state <-- of_int ~width:2 1;
          ];
          (of_int ~width:2 1) [
            a <--. 2;
            state <-- of_int ~width:2 2;
          ];
          (of_int ~width:2 2) [
            a <--. 1;
            state <-- of_int ~width:2 3;
          ];
          (of_int ~width:2 3) [
            a <--. 0;
            state <-- of_int ~width:2 4;
          ]
        ]
      ];
      let state = state.value in
      let a = a.value in
      ....
    v} *)

open! Core0

module type S = sig
  module Signal : Signal.S

  (** The type of variables in guarded assignments. Variables may be asychronous [wire]s,
      or synchronous [reg]s. The current value of the variable may be accessed through the
      [value] field below. *)
  module Variable : sig
    type internal

    type t = private
      { value : Signal.t
      ; internal : internal
      }
    [@@deriving sexp_of]

    (** gets the current value of the variable *)
    val value : t -> Signal.t

    (** create a wire *)
    val wire : here:[%call_pos] -> default:Signal.t -> unit -> t

    (** create a register *)
    val reg : here:[%call_pos] -> (width:int -> t) Signal.with_register_spec

    (** creates a register which forwards its input value to the output during cycles in
        which it is enabled. *)
    val cut_through_reg : (width:int -> t) Signal.with_register_spec

    (** create a pipeline of registers *)
    val pipeline
      :  here:[%call_pos]
      -> (width:int -> depth:int -> t) Signal.with_register_spec

    val __ppx_auto_name : t -> string -> t

    module Expert : sig
      (** Transform the value within the Variable *)
      val map_value : t -> f:(Signal.t -> Signal.t) -> t
    end
  end

  type assign_internal
  type if_internal
  type match_internal
  type switch_internal

  type match_ = private
    { value : Signal.t * t list
    ; internal : match_internal
    }
  [@@deriving sexp_of]

  and t = private
    | Assign of
        { value : Variable.t * Signal.t
        ; internal : assign_internal
        }
    | If of
        { value : Signal.t * t list * t list
        ; internal : if_internal
        }
    | Switch of
        { value : Signal.t * match_ list
        ; internal : switch_internal
        }
  [@@deriving sexp_of]

  type always := t
  type 'a case = 'a * t list
  type 'a cases = 'a case list

  (** if statement *)
  val if_ : here:[%call_pos] -> Signal.t -> t list -> t list -> t

  (** else if branch *)
  val elif : here:[%call_pos] -> Signal.t -> t list -> t list -> t list

  (** else branch (for readability) *)
  val else_ : t list -> t list

  (** if sel then [...] else [] *)
  val when_ : here:[%call_pos] -> Signal.t -> t list -> t

  (** if sel then [] else [...] *)
  val unless : here:[%call_pos] -> Signal.t -> t list -> t

  (** switch statement *)
  val switch : here:[%call_pos] -> Signal.t -> Signal.t cases -> t

  (** Allows sequences of expressions to be inserted into the code; a syntactic nicety. *)
  val proc : here:[%call_pos] -> t list -> t

  (** assignment *)
  val ( <-- ) : Variable.t -> Signal.t -> t

  (** assign an integer constant - width is inferred and value is truncated. *)
  val ( <--. ) : Variable.t -> int -> t

  (** assign an unsigned integer constant *)
  val ( <-:. ) : Variable.t -> int -> t

  (** assign a signed integer constant *)
  val ( <-+. ) : Variable.t -> int -> t

  (** increment (defaults to 1) *)
  val incr : ?by:int -> Variable.t -> t

  (** decrement (defaults to 1) *)
  val decr : ?by:int -> Variable.t -> t

  module State_machine : sig
    type 'a t =
      { current : Signal.t
      ; is : 'a -> Signal.t
      ; set_next : 'a -> always
      (** [switch cases] does a switch on all possible states. The cases must be
          exhaustive and irredundant. If the cases are non-exhaustive, one can supply
          [~default] to make them exhaustive. *)
      ; switch : here:[%call_pos] -> ?default:always list -> 'a cases -> always
      }
    [@@deriving sexp_of]

    module Encoding : sig
      type t =
        | Binary
        | Gray
        | Onehot
      [@@deriving sexp_of, enumerate]

      val to_string : t -> string
    end

    module type State = sig
      type t [@@deriving compare ~localize, enumerate, sexp_of]
    end

    (** [create reg_spec ~e] creates a new state machine where the state is stored in a
        register created from [reg_spec] and [e].

        [encoding] chooses the state encoding from binary, gray or onehot. Generally
        binary is correctly identified by synthesizers and transformed to onehot.

        [auto_wave_format] will automatically make state names show in waveforms. *)
    val create
      :  here:[%call_pos]
      -> ?encoding:Encoding.t (** default is [Binary] *)
      -> ?auto_wave_format:bool (** default is [true] *)
      -> ?attributes:Rtl_attribute.t list
           (** attributes to apply to the state register. Default is one_hot encoding up
               to 32 states, sequential otherwise. *)
      -> ?enable:Signal.t
      -> ?unreachable:'a list
           (** List of states in [State.all] that cannot be defined nor set. *)
      -> (module State with type t = 'a)
      -> Signal.Reg_spec.t
      -> 'a t

    val __ppx_auto_name : 'a t -> string -> 'a t
  end

  (** compile to structural code *)
  val compile : t list -> unit
end

module type Always = sig
  module type S = S

  include S with module Signal := Signal

  module Clocked : sig
    include S with module Signal := Clocked_signal

    val set_variable_dom : Variable.t -> dom:Clock_domain.Runtime.t -> Variable.t

    module Variable_overrides : sig
      val wire : default:Clocked_signal.t -> dom:Clock_domain.Runtime.t -> Variable.t
    end
  end
end
