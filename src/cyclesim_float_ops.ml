open! Import
include Cyclesim_float_ops_intf

(* simulation modules for floating point arithmetic *)

module Width = struct
  type t =
    | W32
    | W64
  [@@deriving sexp_of]

  let num_bits = function
    | W32 -> 32
    | W64 -> 64
  ;;

  let float_of_bits t bits =
    match t with
    | W32 -> bits |> Bits.to_int32 |> Int32.float_of_bits
    | W64 -> bits |> Bits.to_int64 |> Int64.float_of_bits
  ;;

  let bits_of_float t float =
    match t with
    | W32 -> Bits.of_int32 ~width:32 (Int32.bits_of_float float)
    | W64 -> Bits.of_int64 ~width:64 (Int64.bits_of_float float)
  ;;
end

let op2 op name width =
  let create_fn = function
    | [ x; y ] ->
      let x, y = Width.float_of_bits width x, Width.float_of_bits width y in
      [ Width.bits_of_float width (op x y) ]
    | _ -> raise_s [%message "operation requires 2 arguments" (name : string)]
  in
  let w = Width.num_bits width in
  Combinational_op.create () ~name ~input_widths:[ w; w ] ~output_widths:[ w ] ~create_fn
;;

let op1 op name width =
  let create_fn = function
    | [ x ] ->
      let x = Width.float_of_bits width x in
      [ Width.bits_of_float width (op x) ]
    | _ -> raise_s [%message "Float operation requires 1 argument" (name : string)]
  in
  let w = Width.num_bits width in
  Combinational_op.create () ~name ~input_widths:[ w ] ~output_widths:[ w ] ~create_fn
;;

module F (P : sig
    val width : Width.t
  end) =
struct
  let database = Combinational_ops_database.create ()
  let n s = String.concat ~sep:"_" [ "float"; Int.to_string (Width.num_bits P.width); s ]

  let get_output = function
    | [ a ] -> a
    | _ -> raise_s [%message "Floating point operation did not return 1 result value"]
  ;;

  let op2 op name =
    let op = op2 op (n name) P.width in
    Combinational_ops_database.insert database op;
    fun a b -> Combinational_op.instantiate op ~inputs:[ a; b ] |> get_output
  ;;

  let op1 op name =
    let op = op1 op (n name) P.width in
    Combinational_ops_database.insert database op;
    fun a -> Combinational_op.instantiate op ~inputs:[ a ] |> get_output
  ;;

  let ( +: ) = op2 ( +. ) "add"
  let ( -: ) = op2 ( -. ) "sub"
  let ( *: ) = op2 ( *. ) "mul"
  let ( /: ) = op2 ( /. ) "div"
  let ( %: ) = op2 Float.mod_float "mod"
  let ( **: ) = op2 ( ** ) "pow"
  let exp = op1 Float.exp "exp"
  let log = op1 Float.log "log"
  let log10 = op1 Float.log10 "log10"
  let cos = op1 Float.cos "cos"
  let sin = op1 Float.sin "sin"
  let tan = op1 Float.tan "tan"
  let acos = op1 Float.acos "acos"
  let asin = op1 Float.asin "asin"
  let atan = op1 Float.atan "atan"
  let atan2 = op2 Float.atan2 "atan2"
  let cosh = op1 Float.cosh "cosh"
  let sinh = op1 Float.sinh "sinh"
  let tanh = op1 Float.tanh "tanh"
  let ceil = op1 ceil "ceil"
  let floor = op1 floor "floor"
  let abs = op1 Float.abs "abs"
end

module Float = F (struct
    let width = Width.W32
  end)

module Double = F (struct
    let width = Width.W64
  end)
