open! Core0
include Always_prim_intf

module Make (Signal : Signal.S) = struct
  module Signal = Signal

  module Variable = struct
    module Internal = struct
      type t =
        { assigns_to_wire : Signal.t
        ; default : Signal.t
        ; debug_info : Coverage_prim.Always_metadata.Variable.t
        }
      [@@deriving sexp_of]
    end

    type t =
      { value : Signal.t
      ; internal : Internal.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  module Assign_internal = struct
    type t = { state_metadata : Coverage_prim.State.Named.t option } [@@deriving sexp_of]
  end

  module If_internal = struct
    module Kind = struct
      type t =
        | Condition of Coverage_prim.Always_metadata.If.Kind.t
        | One_hot_switch of
            { var : Variable.t
            ; state : Coverage_prim.State.Named.t
            }
      [@@deriving sexp_of]
    end

    type t =
      { kind : Kind.t
      ; creation_pos : Source_code_position.t
      }
    [@@deriving sexp_of]
  end

  module Match_internal = struct
    type t =
      | Basic
      | Always_state of Coverage_prim.State.Named.t
    [@@deriving sexp_of]
  end

  module Switch_internal = struct
    type t =
      { creation_pos : Source_code_position.t
      ; states : Coverage_prim.State.Named.t list
      }
    [@@deriving sexp_of]
  end

  type match_ =
    { value : Signal.t * t list
    ; internal : Match_internal.t
    }
  [@@deriving sexp_of]

  and t =
    | Assign of
        { value : Variable.t * Signal.t
        ; internal : Assign_internal.t
        }
    | If of
        { value : Signal.t * t list * t list
        ; internal : If_internal.t
        }
    | Switch of
        { value : Signal.t * match_ list
        ; internal : Switch_internal.t
        }
  [@@deriving sexp_of]
end
