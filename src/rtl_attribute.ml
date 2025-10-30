open! Core0

module Value = struct
  module T = struct
    type t =
      | Int of int
      | String of string
      | Bool of bool
    [@@deriving bin_io, sexp_of, compare ~localize, equal ~localize, hash]
  end

  include T
  include Comparator.Make (T)
end

module Applies_to = struct
  module T = struct
    type t =
      | Non_wires
      | Regs
      | Memories
      | Instantiations
    [@@deriving bin_io, sexp_of, compare ~localize, equal ~localize, hash]
  end

  include T
  include Comparator.Make (T)
end

module T = struct
  type t =
    { name : string
    ; value : Value.t option
    ; applies_to : Applies_to.t list
    }
  [@@deriving bin_io, sexp_of, compare ~localize, equal ~localize, hash]
end

include T
include Comparator.Make (T)

let create ?(applies_to = []) ?value name = { name; value; applies_to }
let name t = t.name
let value t = t.value
let applies_to t = t.applies_to

module Vivado = struct
  (* see ug901 vivado synthesis guide chapter 2. *)

  let true_or_false_string : _ -> Value.t = function
    | true -> String "TRUE"
    | false -> String "FALSE"
  ;;

  let yes_or_no_string : _ -> Value.t = function
    | true -> String "yes"
    | false -> String "no"
  ;;

  let async_reg b =
    create "ASYNC_REG" ~applies_to:[ Regs ] ~value:(true_or_false_string b)
  ;;

  let user_sll_reg b =
    create "USER_SLL_REG" ~applies_to:[ Regs ] ~value:(true_or_false_string b)
  ;;

  let dont_touch b = create "dont_touch" ~value:(true_or_false_string b)

  let keep_hierarchy b =
    create "keep_hierarchy" ~applies_to:[ Instantiations ] ~value:(yes_or_no_string b)
  ;;

  let use_dsp b = create "USE_DSP" ~value:(yes_or_no_string b)

  let fsm_encoding enc =
    create
      "fsm_encoding"
      ~applies_to:[ Regs ]
      ~value:
        (String
           (match enc with
            | `one_hot -> "one_hot"
            | `sequential -> "sequential"
            | `johnson -> "johnson"
            | `gray -> "gray"
            | `auto -> "auto"
            | `none -> "none"))
  ;;

  let mark_debug b = create "mark_debug" ~value:(true_or_false_string b)
  let keep b = create "keep" ~value:(true_or_false_string b)

  let io_buffer_type typ =
    create
      "io_buffer_type"
      ~value:
        (Value.String
           (match typ with
            | `IBUF -> "IBUF"
            | `OBUF -> "OBUF"
            | `None -> "none"))
  ;;

  let max_fanout n = create "max_fanout" ~value:(Value.Int n)
  let retiming_forward n = create "retiming_forward" ~value:(Value.Int n)
  let retiming_backward n = create "retiming_backward" ~value:(Value.Int n)

  let extract_enable b =
    create "extract_enable" ~applies_to:[ Regs ] ~value:(true_or_false_string b)
  ;;

  let extract_reset b =
    create "extract_reset" ~applies_to:[ Regs ] ~value:(true_or_false_string b)
  ;;

  let critical_sig_opt b =
    create "critical_sig_opt" ~applies_to:[ Regs ] ~value:(true_or_false_string b)
  ;;

  let cascade_height x = create "CASCADE_HEIGHT" ~applies_to:[ Memories ] ~value:(Int x)

  module Ram_style = struct
    let block = create "RAM_STYLE" ~applies_to:[ Memories ] ~value:(String "block")

    let distributed =
      create "RAM_STYLE" ~applies_to:[ Memories ] ~value:(String "distributed")
    ;;

    let registers =
      create "RAM_STYLE" ~applies_to:[ Memories ] ~value:(String "registers")
    ;;

    let ultra = create "RAM_STYLE" ~applies_to:[ Memories ] ~value:(String "ultra")
  end

  module Srl_style = struct
    let register =
      create "SRL_STYLE" ~applies_to:[ Regs; Instantiations ] ~value:(String "register")
    ;;

    let srl =
      create "SRL_STYLE" ~applies_to:[ Regs; Instantiations ] ~value:(String "srl")
    ;;

    let srl_reg =
      create "SRL_STYLE" ~applies_to:[ Regs; Instantiations ] ~value:(String "srl_reg")
    ;;

    let reg_srl =
      create "SRL_STYLE" ~applies_to:[ Regs; Instantiations ] ~value:(String "reg_srl")
    ;;

    let reg_srl_reg =
      create
        "SRL_STYLE"
        ~applies_to:[ Regs; Instantiations ]
        ~value:(String "reg_srl_reg")
    ;;

    let block =
      create "SRL_STYLE" ~applies_to:[ Regs; Instantiations ] ~value:(String "block")
    ;;
  end
end
