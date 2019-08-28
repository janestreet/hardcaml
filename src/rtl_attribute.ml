open! Import


module Value = struct
  type t =
    | Int of int
    | String of string
    | Bool of bool
  [@@deriving sexp_of]
end

type t =
  { name : string
  ; value : Value.t option
  }
[@@deriving sexp_of]

let create ?value name = { name; value }
let name t = t.name
let value t = t.value

module Vivado = struct
  (* see ug901 vivado synthesis guide chapter 2. *)

  let true_or_false_string : _ -> Value.t = function
    | true -> String "TRUE"
    | false -> String "FALSE"
  ;;

  let async_reg b = create "ASYNC_REG" ~value:(true_or_false_string b)


  let dont_touch b = create "dont_touch" ~value:(true_or_false_string b)

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

  module Ram_style = struct
    let block = create "RAM_STYLE" ~value:(String "block")
    let distributed = create "RAM_STYLE" ~value:(String "distributed")
    let registers = create "RAM_STYLE" ~value:(String "registers")
    let ultra = create "RAM_STYLE" ~value:(String "ultra")
  end
end
