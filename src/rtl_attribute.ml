open Base

module Value = struct
  type t =
    | Int of int
    | String of string
    | Bool of bool
  [@@deriving sexp_of, compare, equal, hash]
end

type t =
  { name : string
  ; value : Value.t option
  }
[@@deriving sexp_of, compare, equal, hash]

let create ?value name = { name; value }
let name t = t.name
let value t = t.value

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

  let async_reg b = create "ASYNC_REG" ~value:(true_or_false_string b)
  let dont_touch b = create "dont_touch" ~value:(true_or_false_string b)
  let keep_hierarchy b = create "keep_hierarchy" ~value:(yes_or_no_string b)

  let fsm_encoding enc =
    create
      "fsm_encoding"
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

  module Ram_style = struct
    let block = create "RAM_STYLE" ~value:(String "block")
    let distributed = create "RAM_STYLE" ~value:(String "distributed")
    let registers = create "RAM_STYLE" ~value:(String "registers")
    let ultra = create "RAM_STYLE" ~value:(String "ultra")
  end

  module Srl_style = struct
    let register = create "SRL_STYLE" ~value:(String "register")
    let srl = create "SRL_STYLE" ~value:(String "srl")
    let srl_reg = create "SRL_STYLE" ~value:(String "srl_reg")
    let reg_srl = create "SRL_STYLE" ~value:(String "reg_srl")
    let reg_srl_reg = create "SRL_STYLE" ~value:(String "reg_srl_reg")
    let block = create "SRL_STYLE" ~value:(String "block")
  end
end
