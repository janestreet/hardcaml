(** Definition of clock, reset and clear signals for sequential logic (ie registers). *)

type signal = Signal__type.t

type t = Signal__type.register =
  { reg_clock : signal
  ; reg_clock_edge : Edge.t
  ; reg_reset : signal
  ; reg_reset_edge : Edge.t
  ; reg_reset_value : signal
  ; reg_clear : signal
  ; reg_clear_level : Level.t
  ; reg_clear_value : signal
  ; reg_enable : signal
  }

let sexp_of_t = Signal__type.sexp_of_register

let reg_empty : t =
  { reg_clock = Empty
  ; reg_clock_edge = Rising
  ; reg_reset = Empty
  ; reg_reset_edge = Rising
  ; reg_reset_value = Empty
  ; reg_clear = Empty
  ; reg_clear_level = High
  ; reg_clear_value = Empty
  ; reg_enable = Empty
  }
;;

let override
  ?clock
  ?clock_edge
  ?reset
  ?reset_edge
  ?reset_to
  ?clear
  ?clear_level
  ?clear_to
  ?global_enable
  (spec : t)
  =
  { Signal__type.reg_clock = Option.value clock ~default:spec.reg_clock
  ; reg_clock_edge = Option.value clock_edge ~default:spec.reg_clock_edge
  ; reg_reset = Option.value reset ~default:spec.reg_reset
  ; reg_reset_edge = Option.value reset_edge ~default:spec.reg_reset_edge
  ; reg_reset_value = Option.value reset_to ~default:spec.reg_reset_value
  ; reg_clear = Option.value clear ~default:spec.reg_clear
  ; reg_clear_level = Option.value clear_level ~default:spec.reg_clear_level
  ; reg_clear_value = Option.value clear_to ~default:spec.reg_clear_value
  ; reg_enable = Option.value global_enable ~default:spec.reg_enable
  }
;;

let create ?clear ?reset () ~clock =
  let spec =
    match clear, reset with
    | None, None -> reg_empty
    | None, Some reset -> { reg_empty with reg_reset = reset }
    | Some clear, None -> { reg_empty with reg_clear = clear }
    | Some clear, Some reset -> { reg_empty with reg_reset = reset; reg_clear = clear }
  in
  { spec with reg_clock = clock }
;;

let clock (spec : t) = spec.reg_clock
let clear (spec : t) = spec.reg_clear
let reset (spec : t) = spec.reg_reset
