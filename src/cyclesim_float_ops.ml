open Base
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
end

let op2 op name (width : Width.t) =
  let create_fn i o =
    match i, o with
    | [ x; y ], [ z ] ->
      let x = Bits.Mutable.unsafe_get_int64 x 0 in
      let y = Bits.Mutable.unsafe_get_int64 y 0 in
      (match width with
       | W32 ->
         let x = Int32.of_int64_trunc x |> Int32.float_of_bits in
         let y = Int32.of_int64_trunc y |> Int32.float_of_bits in
         let z' = op x y |> Int32.bits_of_float |> Int64.of_int32_exn in
         Bits.Mutable.unsafe_set_int64 z 0 Int64.(z' land 0xFFFF_FFFFL)
       | W64 ->
         let x = Int64.float_of_bits x in
         let y = Int64.float_of_bits y in
         let z' = op x y |> Int64.bits_of_float in
         Bits.Mutable.unsafe_set_int64 z 0 z')
    | _ ->
      raise_s [%message "operation requires 2 arguments and 1 result" (name : string)]
  in
  let w = Width.num_bits width in
  Combinational_op.create () ~name ~input_widths:[ w; w ] ~output_widths:[ w ] ~create_fn
;;

let op1 op name (width : Width.t) =
  let create_fn i o =
    match i, o with
    | [ x ], [ z ] ->
      let x = Bits.Mutable.unsafe_get_int64 x 0 in
      (match width with
       | W32 ->
         let x = Int32.of_int64_trunc x |> Int32.float_of_bits in
         let z' = op x |> Int32.bits_of_float |> Int64.of_int32_exn in
         Bits.Mutable.unsafe_set_int64 z 0 Int64.(z' land 0xFFFF_FFFFL)
       | W64 ->
         let x = Int64.float_of_bits x in
         let z' = op x |> Int64.bits_of_float in
         Bits.Mutable.unsafe_set_int64 z 0 z')
    | _ ->
      raise_s [%message "operation requires 1 arguments and 1 result" (name : string)]
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
  let ( **: ) = op2 Float.( ** ) "pow"
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
  let ceil = op1 Float.round_up "ceil"
  let floor = op1 Float.round_down "floor"
  let abs = op1 Float.abs "abs"
end

module Float = F (struct
  let width = Width.W32
end)

module Double = F (struct
  let width = Width.W64
end)
