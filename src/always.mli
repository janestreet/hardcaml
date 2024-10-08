(** [Always] is a DSL that lets one describe a circuit in the same style as a Verliog
    [always] block.

    [if] and [switch] control constructs are provided.  (<--) is used for assignment.

    Code is written as lists of assignments, if and control statements.

    variables;

    {[
      let r_sync = Reg_spec.create ~clock ~clear in
      let var = wire (zero 8) in
      let var = reg r_sync enable 8 in
    ]}

    assignment;

    {[
      var <-- exp;
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

open Base

(** The type of variables in guarded assignments.  Variables may be asychronous
    [wire]s, or synchronous [reg]s.  The current value of the variable may be
    accessed through the [value] field below. *)
module Variable : sig
  type internal

  type t = private
    { value : Signal.t
    ; internal : internal
    }
  [@@deriving fields ~getters, sexp_of]

  (** create a wire *)
  val wire : default:Signal.t -> t

  (** create a register *)
  val reg : (width:int -> t) Signal.with_register_spec

  (** create a pipeline of registers *)
  val pipeline : (width:int -> depth:int -> t) Signal.with_register_spec
end

type t = private
  | Assign of Variable.t * Signal.t
  | If of Signal.t * t list * t list
  | Switch of Signal.t * (Signal.t * t list) list
[@@deriving sexp_of]

type always := t
type 'a case = 'a * t list
type 'a cases = 'a case list

(** if statement *)
val if_ : Signal.t -> t list -> t list -> t

(** else if branch *)
val elif : Signal.t -> t list -> t list -> t list

(** else branch (for readability) *)
val else_ : t list -> t list

(** if sel then [...] else [] *)
val when_ : Signal.t -> t list -> t

(** if sel then [] else [...] *)
val unless : Signal.t -> t list -> t

(** switch statement *)
val switch : Signal.t -> Signal.t cases -> t

(** Allows sequences of expressions to be inserted into the code; a syntactic nicety. *)
val proc : t list -> t

(** assignment *)
val ( <-- ) : Variable.t -> Signal.t -> t

(** assignment with an integer constant - width is inferred *)
val ( <--. ) : Variable.t -> int -> t

(** increment (defaults to 1) *)
val incr : ?by:int -> Variable.t -> t

(** decrement (defaults to 1) *)
val decr : ?by:int -> Variable.t -> t

module State_machine : sig
  type 'a t =
    { current : Signal.t
    ; is : 'a -> Signal.t
    ; set_next : 'a -> always
    (** [switch cases] does a switch on all possible states.  The cases must be exhaustive
        and irredundant.  If the cases are non-exhaustive, one can supply [~default] to
        make them exhaustive. *)
    ; switch : ?default:always list -> 'a cases -> always
    }
  [@@deriving sexp_of]

  module Encoding : sig
    type t =
      | Binary
      | Gray
      | Onehot
    [@@deriving sexp_of]

    val to_string : t -> string
  end

  module type State = sig
    type t [@@deriving compare, enumerate, sexp_of]
  end

  (** [create reg_spec ~e] creates a new state machine where the state is stored in a
      register created from [reg_spec] and [e].

      [encoding] chooses the state encoding from binary, gray or onehot. Generally binary
      is correctly identified by synthesizers and transformed to onehot.

      [auto_wave_format] will automatically make state names show in waveforms.
  *)
  val create
    :  ?encoding:Encoding.t (** default is [Binary] *)
    -> ?auto_wave_format:bool (** default is [true] *)
    -> ?enable:Signal.t
    -> (module State with type t = 'a)
    -> Reg_spec.t
    -> 'a t
end

(** compile to structural code *)
val compile : t list -> unit
