(** Definition of clock, reset and clear signals for sequential logic (ie registers). *)

open! Core0

module type Signal = Reg_spec_intf.Signal
module type S = Reg_spec_intf.S

module Make (Signal : Signal) = struct
  type signal = Signal.t [@@deriving sexp_of]

  type t =
    { clock : signal
    ; clock_edge : Edge.t
    ; reset : signal option
    ; reset_edge : Edge.t
    ; clear : signal option
    }
  [@@deriving fields ~getters, sexp_of]

  let reset_exn t = reset t |> Option.value_exn
  let clear_exn t = clear t |> Option.value_exn

  let assert_non_empty typ_ t =
    if Signal.is_empty t
    then raise_s [%message "[Reg_spec] signal must not be empty" typ_ (t : signal)]
  ;;

  let assert_non_empty_or_option typ_ t = Option.iter t ~f:(assert_non_empty typ_)

  let validate { clock; clock_edge = _; reset; reset_edge = _; clear } =
    assert_non_empty "clock" clock;
    assert_non_empty_or_option "reset" reset;
    assert_non_empty_or_option "clear" clear
  ;;

  let create
    ?(clock_edge = Edge.Rising)
    ?reset
    ?(reset_edge = Edge.Rising)
    ?clear
    ()
    ~clock
    =
    let t = { clock; clock_edge; reset; reset_edge; clear } in
    validate t;
    t
  ;;

  let override ?clock ?clock_edge ?reset ?reset_edge ?clear (spec : t) =
    let t =
      { clock =
          (match clock with
           | None -> spec.clock
           | Some clock -> clock)
      ; clock_edge = Option.value clock_edge ~default:spec.clock_edge
      ; reset =
          (match reset with
           | None -> spec.reset
           | Some reset -> Some reset)
      ; reset_edge = Option.value reset_edge ~default:spec.reset_edge
      ; clear =
          (match clear with
           | None -> spec.clear
           | Some clear -> Some clear)
      }
    in
    validate t;
    t
  ;;
end
