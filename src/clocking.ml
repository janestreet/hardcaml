open Base

module type S = Clocking_intf.S

module Make () = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml]

  let add_clear t clear = { t with clear = Signal.( |: ) t.clear clear }
  let to_spec t = Reg_spec.create ~clock:t.clock ~clear:t.clear ()
  let to_spec_no_clear t = Reg_spec.create ~clock:t.clock ()
  let reg t ?enable d = Signal.reg ?enable (to_spec t) d
  let reg_no_clear t ?enable d = Signal.reg ?enable (to_spec_no_clear t) d
  let reg_fb ?enable t ~width ~f = Signal.reg_fb ?enable (to_spec t) ~width ~f

  let pipeline ?attributes ?enable t ~n d =
    Signal.pipeline ?attributes ?enable (to_spec t) ~n d
  ;;

  module Cdc = struct
    let stretch spec ~n d =
      let open Signal in
      assert (width d = 1);
      if n <= 1
      then Core.(raise_s [%message "stretch length must be >1" (n : int)])
      else (
        let n = n - 1 in
        d |: lsb (reg_fb spec ~width:n ~f:(fun s -> mux2 d (ones n) (srl s 1))))
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
      let d_false_path =
        Signal.reg (to_spec_no_clear t) d |> Fn.flip Signal.add_attribute attribute
      in
      let attributes = [ attribute ] in
      Signal.pipeline
        ~attributes
        (to_spec_no_clear t)
        ~n:num_additional_pipeline_stages
        d_false_path
    ;;
  end

  module Var = struct
    let reg ?enable clocking ~width =
      Always.Variable.reg ?enable (to_spec clocking) ~width
    ;;

    let reg_with_default ?enable clocking ~width ~clear_to =
      Always.Variable.reg
        ?enable
        (Reg_spec.override ~clear_to:(Signal.of_int ~width clear_to) (to_spec clocking))
        ~width
    ;;
  end
end

include Make ()
