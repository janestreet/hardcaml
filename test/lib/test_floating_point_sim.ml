(* Test floating point simulation operations *)
open! Import
module S = Cyclesim

module I = struct
  type 'a t =
    { a32 : 'a [@bits 32]
    ; b32 : 'a [@bits 32]
    ; a64 : 'a [@bits 64]
    ; b64 : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { o32 : 'a [@bits 32]
    ; o64 : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

type op1 =
  { name : string
  ; fop : float -> float
  ; hwop32 : Signal.t -> Signal.t
  ; hwop64 : Signal.t -> Signal.t
  }

type op2 =
  { name : string
  ; fop : float -> float -> float
  ; hwop32 : Signal.t -> Signal.t -> Signal.t
  ; hwop64 : Signal.t -> Signal.t -> Signal.t
  }

type result =
  { opname : string
  ; expected : float
  ; got_32bit : float
  ; got_64bit : float
  }
[@@deriving sexp_of]

type all_op_results =
  { arg_a : float
  ; arg_b : float
  ; results : (string, result) Result.t list
  }
[@@deriving sexp_of]

let sim combinational_ops_database (op1 : op1 list) (op2 : op2 list) args =
  (* make op1 compatible with op2 *)
  let op1 : op2 list =
    List.map op1 ~f:(fun op ->
      { name = op.name
      ; fop = (fun a _ -> op.fop a)
      ; hwop32 = (fun a _ -> op.hwop32 a)
      ; hwop64 = (fun a _ -> op.hwop64 a)
      })
  in
  let ops = op1 @ op2 in
  (* hardware circuit *)
  let open Signal in
  let f (i : Signal.t I.t) =
    List.mapi ops ~f:(fun j op ->
      { O.o32 = output ("o32_" ^ Int.to_string j) (op.hwop32 i.a32 i.b32)
      ; o64 = output ("o64_" ^ Int.to_string j) (op.hwop64 i.a64 i.b64)
      })
  in
  (* simulator *)
  let module Cs = Cyclesim in
  let i = I.map ~f:(fun (n, b) -> input n b) I.t in
  let circuit =
    Circuit.create_exn ~name:"test" (List.concat (List.map (f i) ~f:O.to_list))
  in
  let sim = Cs.create ~combinational_ops_database circuit in
  (* simulator ports *)
  let i : Bits.t ref I.t = I.map ~f:(fun (n, _) -> S.in_port sim n) I.t in
  let o : Bits.t ref O.t list =
    List.mapi ops ~f:(fun j _ ->
      { O.o32 = S.out_port sim ("o32_" ^ Int.to_string j)
      ; o64 = S.out_port sim ("o64_" ^ Int.to_string j)
      })
  in
  (* cycle simulator *)
  S.reset sim;
  let step (arg_a, arg_b) =
    i.a32 := Bits.of_int32 ~width:32 (Int32.bits_of_float arg_a);
    i.a64 := Bits.of_int64 ~width:64 (Int64.bits_of_float arg_a);
    i.b32 := Bits.of_int32 ~width:32 (Int32.bits_of_float arg_b);
    i.b64 := Bits.of_int64 ~width:64 (Int64.bits_of_float arg_b);
    S.cycle sim;
    (* collect results *)
    let result (({ o32; o64 } : Bits.t ref O.t), op) =
      { opname = op.name
      ; expected = op.fop arg_a arg_b
      ; got_32bit = Int32.float_of_bits (Bits.to_int32 !o32)
      ; got_64bit = Int64.float_of_bits (Bits.to_int64 !o64)
      }
    in
    let result_in_error_bounds args =
      let ({ opname; expected; got_32bit; got_64bit } as result) = result args in
      if Float.(abs (expected - got_32bit) > 1e-6)
      then Error result
      else if Float.(abs (expected - got_64bit) > 0.0)
      then Error result
      else Ok opname
    in
    { arg_a; arg_b; results = List.map (List.zip_exn o ops) ~f:result_in_error_bounds }
  in
  List.map args ~f:step
;;

let%expect_test "floating point operations in simulation" =
  let module Fops = Cyclesim_float_ops in
  let op1s : op1 list =
    [ { name = "exp"
      ; fop = Float.exp
      ; hwop32 = Fops.Float.exp
      ; hwop64 = Fops.Double.exp
      }
    ; { name = "log"
      ; fop = Float.log
      ; hwop32 = Fops.Float.log
      ; hwop64 = Fops.Double.log
      }
    ; { name = "log10"
      ; fop = Float.log10
      ; hwop32 = Fops.Float.log10
      ; hwop64 = Fops.Double.log10
      }
    ; { name = "cos"
      ; fop = Float.cos
      ; hwop32 = Fops.Float.cos
      ; hwop64 = Fops.Double.cos
      }
    ; { name = "sin"
      ; fop = Float.sin
      ; hwop32 = Fops.Float.sin
      ; hwop64 = Fops.Double.sin
      }
    ; { name = "tan"
      ; fop = Float.tan
      ; hwop32 = Fops.Float.tan
      ; hwop64 = Fops.Double.tan
      }
    ; { name = "acos"
      ; fop = Float.acos
      ; hwop32 = Fops.Float.acos
      ; hwop64 = Fops.Double.acos
      }
    ; { name = "asin"
      ; fop = Float.asin
      ; hwop32 = Fops.Float.asin
      ; hwop64 = Fops.Double.asin
      }
    ; { name = "atan"
      ; fop = Float.atan
      ; hwop32 = Fops.Float.atan
      ; hwop64 = Fops.Double.atan
      }
    ; { name = "cosh"
      ; fop = Float.cosh
      ; hwop32 = Fops.Float.cosh
      ; hwop64 = Fops.Double.cosh
      }
    ; { name = "sinh"
      ; fop = Float.sinh
      ; hwop32 = Fops.Float.sinh
      ; hwop64 = Fops.Double.sinh
      }
    ; { name = "tanh"
      ; fop = Float.tanh
      ; hwop32 = Fops.Float.tanh
      ; hwop64 = Fops.Double.tanh
      }
    ; { name = "ceil"
      ; fop = Caml.ceil
      ; hwop32 = Fops.Float.ceil
      ; hwop64 = Fops.Double.ceil
      }
    ; { name = "floor"
      ; fop = Caml.floor
      ; hwop32 = Fops.Float.floor
      ; hwop64 = Fops.Double.floor
      }
    ; { name = "abs"
      ; fop = Float.abs
      ; hwop32 = Fops.Float.abs
      ; hwop64 = Fops.Double.abs
      }
    ]
  in
  let op2s : op2 list =
    [ { name = "+"
      ; fop = Float.( + )
      ; hwop32 = Fops.Float.( +: )
      ; hwop64 = Fops.Double.( +: )
      }
    ; { name = "-"
      ; fop = Float.( - )
      ; hwop32 = Fops.Float.( -: )
      ; hwop64 = Fops.Double.( -: )
      }
    ; { name = "*"
      ; fop = Float.( * )
      ; hwop32 = Fops.Float.( *: )
      ; hwop64 = Fops.Double.( *: )
      }
    ; { name = "/"
      ; fop = Float.( / )
      ; hwop32 = Fops.Float.( /: )
      ; hwop64 = Fops.Double.( /: )
      }
    ; { name = "%"
      ; fop = Float.mod_float
      ; hwop32 = Fops.Float.( %: )
      ; hwop64 = Fops.Double.( %: )
      }
    ; { name = "**"
      ; fop = ( **. )
      ; hwop32 = Fops.Float.( **: )
      ; hwop64 = Fops.Double.( **: )
      }
    ; { name = "atan2"
      ; fop = Float.atan2
      ; hwop32 = Fops.Float.atan2
      ; hwop64 = Fops.Double.atan2
      }
    ]
  in
  let database =
    Combinational_ops_database.concat [ Fops.Float.database; Fops.Double.database ]
  in
  print_s
    [%sexp
      (sim database op1s op2s [ 0., 0.; 1.5, 2.3; -0.7, 3.4 ] : all_op_results list)];
  [%expect
    {|
    (((arg_a 0)
      (arg_b 0)
      (results (
        (Ok exp)
        (Ok log)
        (Ok log10)
        (Ok cos)
        (Ok sin)
        (Ok tan)
        (Ok acos)
        (Ok asin)
        (Ok atan)
        (Ok cosh)
        (Ok sinh)
        (Ok tanh)
        (Ok ceil)
        (Ok floor)
        (Ok abs)
        (Ok +)
        (Ok -)
        (Ok *)
        (Ok /)
        (Ok %)
        (Ok **)
        (Ok atan2))))
     ((arg_a 1.5)
      (arg_b 2.3)
      (results (
        (Ok exp)
        (Ok log)
        (Ok log10)
        (Ok cos)
        (Ok sin)
        (Ok tan)
        (Ok acos)
        (Ok asin)
        (Ok atan)
        (Ok cosh)
        (Ok sinh)
        (Ok tanh)
        (Ok ceil)
        (Ok floor)
        (Ok abs)
        (Ok +)
        (Ok -)
        (Ok *)
        (Ok /)
        (Ok %)
        (Ok **)
        (Ok atan2))))
     ((arg_a -0.7)
      (arg_b 3.4)
      (results (
        (Ok exp)
        (Ok log)
        (Ok log10)
        (Ok cos)
        (Ok sin)
        (Ok tan)
        (Ok acos)
        (Ok asin)
        (Ok atan)
        (Ok cosh)
        (Ok sinh)
        (Ok tanh)
        (Ok ceil)
        (Ok floor)
        (Ok abs)
        (Ok +)
        (Ok -)
        (Ok *)
        (Ok /)
        (Ok %)
        (Ok **)
        (Ok atan2))))) |}]
;;
