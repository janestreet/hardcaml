open! Core0

module type S = Clocking_intf.S

module Untyped = Clocking_intf.Untyped

module Generate_functions
    (Signal : Signal.S)
    (Always : Always.S with module Signal := Signal) =
struct
  open Untyped

  let of_untyped x = x
  let to_untyped x = x
  let add_clear t clear = { t with clear = Signal.( |: ) t.clear clear }
  let to_spec t = Signal.Reg_spec.create ~clock:t.clock ~clear:t.clear ()
  let to_spec_no_clear t = Signal.Reg_spec.create ~clock:t.clock ()
  let reg t ?enable ?clear ?clear_to d = Signal.reg ?enable ?clear ?clear_to (to_spec t) d
  let reg_no_clear t ?enable d = Signal.reg ?enable (to_spec_no_clear t) d

  let reg_fb ?enable ?clear ?clear_to t ~width ~f =
    Signal.reg_fb ?enable ?clear ?clear_to (to_spec t) ~width ~f
  ;;

  let reg_fb_no_clear ?enable ?clear ?clear_to t ~width ~f =
    Signal.reg_fb ?enable ?clear ?clear_to (to_spec_no_clear t) ~width ~f
  ;;

  let pipeline ?attributes ?enable ?clear ?clear_to t ~n d =
    Signal.pipeline ?attributes ?clear ?enable ?clear_to (to_spec t) ~n d
  ;;

  let pipeline_no_clear ?attributes ?enable t ~n d =
    Signal.pipeline ?attributes ?enable (to_spec_no_clear t) ~n d
  ;;

  let cut_through_reg t ?clear ?clear_to ~enable d =
    Signal.cut_through_reg ?clear ?clear_to (to_spec t) ~enable d
  ;;

  module Cdc = struct
    let stretch spec ~n d =
      let open Signal in
      assert (width d = 1);
      if n <= 1
      then raise_s [%message "stretch length must be >1" (n : int)]
      else (
        let n = n - 1 in
        d |: lsb (reg_fb spec ~width:n ~f:(fun s -> mux2 d (ones n) (srl s ~by:1))))
    ;;

    let stretch_no_clear t ~n d = stretch (to_spec_no_clear t) ~n d
    let stretch t ~n d = stretch (to_spec t) ~n d

    let detect_rising_edge t s =
      let open Signal in
      let prev_s = reg_no_clear t s in
      ~:prev_s &: s
    ;;

    let with_valid_pulse_detect_rising_edge t (v : _ With_valid.t) =
      { v with valid = detect_rising_edge t v.valid }
    ;;

    let reg_no_clear_with_async_reg_annotation ~num_additional_pipeline_stages t d =
      let attribute = Rtl_attribute.Vivado.async_reg true in
      let d_path =
        Signal.reg (to_spec_no_clear t) d |> Fn.flip Signal.add_attribute attribute
      in
      let attributes = [ attribute ] in
      Signal.pipeline
        ~attributes
        (to_spec_no_clear t)
        ~n:num_additional_pipeline_stages
        d_path
    ;;
  end

  module Var = struct
    let reg ?enable ?clear ?clear_to clocking ~width =
      Always.Variable.reg ?enable ?clear ?clear_to (to_spec clocking) ~width
    ;;

    let reg_no_clear ?enable clocking ~width =
      Always.Variable.reg ?enable (to_spec_no_clear clocking) ~width
    ;;

    let cut_through_reg ?enable ?clear ?clear_to clocking ~width =
      Always.Variable.cut_through_reg ?enable ?clear ?clear_to (to_spec clocking) ~width
    ;;

    let reg_with_int_default ?enable ?clear clocking ~width ~clear_to =
      let clear_to = Signal.of_int_trunc ~width clear_to in
      reg ?enable ?clear ~clear_to clocking ~width
    ;;
  end
end

module Implementation_for_untyped = struct
  include Untyped
  include Generate_functions (Signal) (Always)

  module Clocked = struct
    include Untyped
    include Generate_functions (Clocked_signal) (Always.Clocked)
  end
end

(* This implementation has Make().t = Untyped.t to be the same type, but this equality
   isn't exposed in the interface file, so from the perspective of the user, these two
   types won't unify.
*)

module Make () = Implementation_for_untyped
include Implementation_for_untyped
