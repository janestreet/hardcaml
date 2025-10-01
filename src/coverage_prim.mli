(** Core coverage types used across different stages of coverage analysis *)

open! Core0

module State : sig
  module Named : sig
    type t =
      { name : string
      ; value : int
      }
    [@@deriving equal, sexp_of, hash, compare]

    include Comparator.S with type t := t
  end

  type t =
    { name : string option
    ; value : int
    }
  [@@deriving sexp_of, equal]

  val of_named : Named.t -> t
end

module Case : sig
  module Positional : sig
    type t =
      | Specified of int
      | Default
    [@@deriving equal, sexp_of, hash, compare]

    include Comparator.S with type t := t
  end

  module Positional_with_state : sig
    type t =
      | Specified of
          { position : int
          ; state : State.Named.t option
          }
      | Default of { states : State.Named.t list }
    [@@deriving sexp_of, to_string]

    val to_positional : t -> Positional.t
    val of_positional : Positional.t -> t
  end

  type t =
    | Positional of int
    | State of State.Named.t
    | Default
  [@@deriving bin_io, sexp_of, to_string, equal]
end

module Cases : sig
  type t [@@deriving sexp_of]

  val create
    :  non_default_cases:[ `Positional | `State of State.Named.t ] list
    -> default_states:State.Named.t list
    -> t

  val create_from_count : non_default_case_count:int -> t
  val to_case_list : t -> Case.Positional_with_state.t list
  val length : t -> int
end

module Toggle : sig
  type t =
    { bit : int
    ; on : bool
    }
  [@@deriving bin_io, sexp_of, equal, hash, compare, to_string]

  include Comparator.S with type t := t

  val all_bits_up_to_excl : int -> t list
end

module Transition : sig
  type 'a t =
    { from : 'a
    ; to_ : 'a
    }
  [@@deriving bin_io, sexp_of, compare, equal, hash]

  val to_string : 'a t -> f:('a -> string) -> string
  val map : 'a t -> f:('a -> 'b) -> 'b t

  module Value : sig
    type nonrec t = int t [@@deriving sexp_of, compare, equal, hash]

    include Comparator.S with type t := t
  end

  module State : sig
    type nonrec t = State.t t [@@deriving sexp_of, equal, to_string]
  end
end

module Waiver : sig
  type 'a t [@@deriving bin_io, sexp_of]

  (** {2 Creation} *)

  val none : unit -> _ t

  val only_expect
    :  ?warn_on_waived_but_observed:bool
         (** Display a warning if a waived value is observed. Default true *)
    -> 'a list
    -> 'a t

  val exclude
    :  ?warn_on_waived_but_observed:bool
         (** Display a warning if a waived value is observed. Default true *)
    -> 'a list
    -> 'a t

  val join : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> 'a t
  val join_multi : 'a t list -> equal:('a -> 'a -> bool) -> 'a t

  (** {2 Accessors / utils} *)

  val is_none : _ t -> bool
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val concat_map : 'a t -> f:('a -> 'b list) -> 'b t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val waived_list : 'a t -> all:'a list -> equal:('a -> 'a -> bool) -> 'a list
  val warn_on_waived_but_observed : _ t -> bool
  val to_string : 'a t -> f:('a -> string) -> string
end

module Always_metadata : sig
  module Variable : sig
    module State_reg : sig
      type t =
        { creation_pos : Source_code_position.t
        ; states_by_value : State.Named.t Map.M(Int).t
        ; transitions_by_value : (int, int Hash_set.t) Hashtbl.t
        ; initial_value : int
        }
      [@@deriving sexp_of]
    end

    type t =
      | User_created of Source_code_position.t
      | State_machine_state of State_reg.t
    [@@deriving bin_io, sexp_of]
  end

  module Target : sig
    type t =
      { variable : Variable.t
      ; names : Name_and_loc.t list
      }
    [@@deriving sexp_of]
  end

  module If : sig
    module Kind : sig
      type t =
        | If
        | Elif
        | When
        | Unless
        | Proc
      [@@deriving bin_io, sexp_of, to_string]
    end

    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; kind : Kind.t
      }
    [@@deriving bin_io, sexp_of]
  end

  module Switch_cases : sig
    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; cases : Cases.t
      }
    [@@deriving bin_io, sexp_of]
  end

  module Switch_mux : sig
    type t =
      { creation_pos : Source_code_position.t
      ; target : Target.t
      ; case : Case.t
      }
    [@@deriving bin_io, sexp_of]
  end
end
