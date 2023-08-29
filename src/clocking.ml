module type S = Clocking_intf.S

module Make () = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let add_clear t clear = { t with clear = Signal.( |: ) t.clear clear }
  let to_spec t = Reg_spec.create ~clock:t.clock ~clear:t.clear ()
  let to_spec_no_clear t = Reg_spec.create ~clock:t.clock ()
  let reg t ?enable d = Signal.reg ?enable (to_spec t) d
  let reg_no_clear t ?enable d = Signal.reg ?enable (to_spec_no_clear t) d
  let reg_fb ?enable t ~width ~f = Signal.reg_fb ?enable (to_spec t) ~width ~f
  let pipeline ?enable t ~n d = Signal.pipeline ?enable (to_spec t) ~n d

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
