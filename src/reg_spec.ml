(** Definition of clock, reset and clear signals for sequential logic (ie registers). *)

type signal = Signal__type.t

type t = Signal__type.reg_spec =
  { clock : signal
  ; clock_edge : Edge.t
  ; reset : signal
  ; reset_edge : Edge.t
  ; clear : signal
  }
[@@deriving fields ~getters]

let empty = Signal__type.Empty

let sexp_of_t spec =
  Signal__type.sexp_of_register
    { Signal__type.spec
    ; enable = empty
    ; initialize_to = None
    ; reset_to = empty
    ; clear_to = empty
    }
;;

let override ?clock ?clock_edge ?reset ?reset_edge ?clear (spec : t) =
  { Signal__type.clock = Option.value clock ~default:spec.clock
  ; clock_edge = Option.value clock_edge ~default:spec.clock_edge
  ; reset = Option.value reset ~default:spec.reset
  ; reset_edge = Option.value reset_edge ~default:spec.reset_edge
  ; clear = Option.value clear ~default:spec.clear
  }
;;

let create
  ?(clock_edge = Edge.Rising)
  ?(reset = empty)
  ?(reset_edge = Edge.Rising)
  ?(clear = empty)
  ()
  ~clock
  =
  { clock; clock_edge; reset; reset_edge; clear }
;;
