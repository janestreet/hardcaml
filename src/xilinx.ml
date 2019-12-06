(* API representing basic Xilinx primitives *)

open! Import
open! Xilinx_intf

module type S = S

(* algebra for building LUT equations *)
module LutEqn = struct
  type t =
    | Gnd
    | Vdd
    | Input of int64
    | And of t * t
    | Or of t * t
    | Xor of t * t
    | Not of t

  let i0 = Input 0L
  let i1 = Input 1L
  let i2 = Input 2L
  let i3 = Input 3L
  let i4 = Input 4L
  let i5 = Input 5L
  let gnd = Gnd
  let vdd = Vdd
  let ( &: ) a b = And (a, b)
  let ( |: ) a b = Or (a, b)
  let ( ^: ) a b = Xor (a, b)
  let ( ~: ) a = Not a
  let ( <>: ) a b = a ^: b
  let ( ==: ) a b = ~:(a <>: b)
  let ( >>. ) a b = Int64.shift_right_logical a (Int64.to_int_exn b)
  let ( <<. ) a b = Int64.shift_left a (Int64.to_int_exn b)
  let ( &. ) = Int64.logand
  let ( |. ) = Int64.logor
  let ( ^. ) = Int64.logxor
  let ( ~. ) = Int64.lognot
  let ( +. ) = Int64.add

  let eval n v =
    let n = Int64.of_int n in
    let rec eval n = function
      | Gnd -> 0L
      | Vdd -> 1L
      | Input a -> n >>. a &. 1L
      | And (a, b) -> eval n a &. eval n b
      | Or (a, b) -> eval n a |. eval n b
      | Xor (a, b) -> eval n a ^. eval n b
      | Not a -> ~.(eval n a) &. 1L
    in
    let rec evaln m w =
      if Int64.equal m (1L <<. n) then w else evaln (m +. 1L) (eval m v <<. m |. w)
    in
    evaln 0L 0L
  ;;
end

(* Hardcaml implementation of Xilinx API *)
module Hardcaml_api = struct
  open Signal

  let lut v sel =
    let n = 1 lsl width sel in
    let rec build i =
      if i = n
      then []
      else (
        let d =
          if not (Int64.equal (Int64.logand v (Int64.shift_left 1L i)) 0L)
          then vdd
          else gnd
        in
        d :: build (i + 1))
    in
    mux sel (build 0)
  ;;

  let muxcy ci di sel = mux2 sel ci di
  let inv a = ~:a
  let xorcy ci li = ci ^: li
  let muxf5 f t s = mux2 s t f
  let muxf6 f t s = mux2 s t f
  let muxf7 f t s = mux2 s t f
  let muxf8 f t s = mux2 s t f

  let fdce c ce clr d =
    reg
      (Reg_spec.override (Reg_spec.create () ~clock:c) ~reset:clr ~reset_to:gnd)
      ~enable:ce
      d
  ;;

  let fdpe c ce pre d =
    reg
      (Reg_spec.override (Reg_spec.create () ~clock:c) ~reset:pre ~reset_to:vdd)
      ~enable:ce
      d
  ;;

  let mult_and a b = a &: b

  let ram1s a d clk we =
    memory
      (1 lsl width a)
      ~write_port:
        { write_clock = clk; write_enable = we; write_address = a; write_data = d }
      ~read_address:a
  ;;
end

(* unisim based implementation of Xilinx API *)
module Unisim = struct
  open Signal

  let inv a =
    (Instantiation.create () ~name:"INV" ~inputs:[ "I", a ] ~outputs:[ "O", 1 ])#o "O"
  ;;

  let lut v sel =
    let w = width sel in
    let w' = Int.to_string w in
    let init =
      Int64.to_string v
      |> Bits.of_decimal_string ~width:(1 lsl w)
      |> Bits.reverse
      |> Bits.to_string
    in
    (Instantiation.create
       ()
       ~name:("LUT" ^ w')
       ~parameters:[ Parameter.create ~name:"INIT" ~value:(String init) ]
       ~inputs:(List.mapi (bits_lsb sel) ~f:(fun i b -> "I" ^ Int.to_string i, b))
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let muxcy ci di sel =
    (Instantiation.create
       ()
       ~name:"MUXCY"
       ~inputs:[ "CI", ci; "DI", di; "S", sel ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let xorcy ci li =
    (Instantiation.create
       ()
       ~name:"XORCY"
       ~inputs:[ "CI", ci; "LI", li ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let muxf5 f t s =
    (Instantiation.create
       ()
       ~name:"MUXF5"
       ~inputs:[ "I0", f; "I1", t; "S", s ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let muxf6 f t s =
    (Instantiation.create
       ()
       ~name:"MUXF6"
       ~inputs:[ "I0", f; "I1", t; "S", s ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let muxf7 f t s =
    (Instantiation.create
       ()
       ~name:"MUXF7"
       ~inputs:[ "I0", f; "I1", t; "S", s ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let muxf8 f t s =
    (Instantiation.create
       ()
       ~name:"MUXF8"
       ~inputs:[ "I0", f; "I1", t; "S", s ]
       ~outputs:[ "O", 1 ])
    #o
      "O"
  ;;

  let fdce c ce clr d =
    (Instantiation.create
       ()
       ~name:"FDCE"
       ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "0") ]
       ~inputs:[ "C", c; "CE", ce; "CLR", clr; "D", d ]
       ~outputs:[ "Q", 1 ])
    #o
      "Q"
  ;;

  let fdpe c ce pre d =
    (Instantiation.create
       ()
       ~name:"FDPE"
       ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "1") ]
       ~inputs:[ "C", c; "CE", ce; "D", d; "PRE", pre ]
       ~outputs:[ "Q", 1 ])
    #o
      "Q"
  ;;

  let mult_and a b =
    (Instantiation.create
       ()
       ~name:"MULT_AND"
       ~inputs:[ "I0", a; "I1", b ]
       ~outputs:[ "LO", 1 ])
    #o
      "LO"
  ;;

  let ram1s a d clk we =
    let width = width a in
    let size = 1 lsl width in
    let a = List.mapi (bits_lsb a) ~f:(fun i s -> "A" ^ Int.to_string i, s) in
    (Instantiation.create
       ()
       ~name:("RAM" ^ Int.to_string size ^ "X1S")
       ~parameters:
         [ Parameter.create
             ~name:"INIT"
             ~value:(String (Constant.of_int ~width:size 0 |> Constant.to_binary_string))
         ]
       ~inputs:([ "D", d; "WE", we; "WCLK", clk ] @ a)
       ~outputs:[ "Q", 1 ])
    #o
      "Q"
  ;;
end

module type T = T with module LutEqn := LutEqn

(* max lut size. *)
module type LutSize = sig
  val max_lut : int
end

module Lut4 = struct
  let max_lut = 4
end

module Lut6 = struct
  let max_lut = 6
end

(* Build API of Xilinx primitives *)
module XMake (X : S) (L : LutSize) = struct
  module S = Signal
  open S
  open LutEqn
  open L

  let x_lut f s =
    let i = eval (width s) f in
    X.lut i s
  ;;

  let x_map f l =
    let w = List.hd_exn l |> width in
    let lut n = x_lut f (List.map l ~f:(fun s -> select s n n) |> concat_lsb) in
    let rec build n = if n = w then [] else lut n :: build (n + 1) in
    concat_lsb (build 0)
  ;;

  let x_and a b = x_map (i0 &: i1) [ a; b ]
  let x_or a b = x_map (i0 |: i1) [ a; b ]
  let x_xor a b = x_map (i0 ^: i1) [ a; b ]
  let x_not a = bits_msb a |> List.map ~f:X.inv |> concat_msb
  let inputs n = List.take [ i0; i1; i2; i3; i4; i5 ] n

  let reduce_inputs op n =
    let args = inputs n in
    List.reduce_exn args ~f:(fun acc a -> op acc a)
  ;;

  let rec x_reduce_carry inv op mux_din carry_in a =
    let n = min max_lut (width a) in
    let op' = reduce_inputs op n in
    let op' = if inv then ~:op' else op' in
    let lut = x_lut op' (select a (n - 1) 0) in
    let carry_out = X.muxcy carry_in mux_din lut in
    if n = width a
    then carry_out
    else x_reduce_carry inv op mux_din carry_out (select a (width a - 1) n)
  ;;

  let x_and_reduce a = x_reduce_carry false ( &: ) S.gnd S.vdd a
  let x_or_reduce a = x_reduce_carry true ( |: ) S.vdd S.gnd a

  let rec x_reduce_tree op a =
    let rec level a =
      let n = min max_lut (width a) in
      let op' = reduce_inputs op n in
      let lut = x_lut op' (select a (n - 1) 0) in
      if n = width a then lut else level (select a (width a - 1) n) @: lut
    in
    if width a = 1 then a else x_reduce_tree op (level a)
  ;;

  let x_add_carry op c a b =
    let lut c a b =
      let o = x_lut op (a @: b) in
      let s = X.xorcy c o in
      let c = X.muxcy c b o in
      c, s
    in
    let r, c =
      List.fold2_exn (bits_lsb a) (bits_lsb b) ~init:([], c) ~f:(fun (r, c) a b ->
        let c, s = lut c a b in
        s :: r, c)
    in
    c, concat_msb r
  ;;

  let x_add a b = snd (x_add_carry (i0 ^: i1) S.gnd a b)
  let x_sub a b = snd (x_add_carry ~:(i0 ^: i1) S.vdd b a)

  let x_mux_add_carry op c x (a, a') b =
    let lut op x c (a, a') b =
      let o = x_lut op (x @: b @: a' @: a) in
      let s = X.xorcy c o in
      let c = X.muxcy c b o in
      c, s
    in
    let zip a b = List.map2_exn a b ~f:(fun a b -> a, b) in
    let r, c =
      List.fold2_exn
        (zip (bits_lsb a) (bits_lsb a'))
        (bits_lsb b)
        ~init:([], c)
        ~f:(fun (r, c) (a, a') b ->
          let c, s = lut op x c (a, a') b in
          s :: r, c)
    in
    c, concat_msb r
  ;;

  let x_mux_add x (a, a') b =
    let add_lut_op = (i0 &: i3 |: (i1 &: ~:i3)) ^: i2 in
    snd (x_mux_add_carry add_lut_op S.gnd x (a, a') b)
  ;;

  let x_mux_sub x a (b, b') =
    let sub_lut_op = ~:((i0 &: i3 |: (i1 &: ~:i3)) ^: i2) in
    snd (x_mux_add_carry sub_lut_op S.vdd x (b, b') a)
  ;;

  let x_eq a b =
    let rec eq l =
      match l with
      | [] -> []
      | a :: b :: t -> ~:(a ^: b) :: eq t
      | _ -> failwith "x_eq expecting even length list"
    in
    let eq l = List.fold (eq l) ~init:vdd ~f:( &: ) in
    let eq_lut a b =
      match width a with
      | 1 -> x_lut (eq [ i0; i1 ]) (b @: a)
      | 2 -> x_lut (eq [ i0; i2; i1; i3 ]) (b @: a)
      | 3 -> x_lut (eq [ i0; i3; i1; i4; i2; i5 ]) (b @: a)
      | _ -> failwith "x_eq invalid signal width"
    in
    let size = max_lut / 2 in
    let rec mk a b =
      assert (width a = width b);
      if width a <= size
      then [ eq_lut a b ]
      else
        eq_lut (select a (size - 1) 0) (select b (size - 1) 0)
        :: mk (select a (width a - 1) size) (select b (width b - 1) size)
    in
    let c = mk a b in
    List.fold c ~init:S.vdd ~f:(fun cin c -> X.muxcy cin S.gnd c)
  ;;

  let x_lt a b = fst (x_add_carry ~:(i0 ^: i1) S.vdd b a)

  (* muxes - Lut6 version doesnt work... *)

  (* basic lut4/6 structures *)
  let x_lut4_mux2 sel d0 d1 = x_lut (~:i0 &: i1 |: (i0 &: i2)) (d1 @: d0 @: sel)

  let x_lut6_mux4 sel d0 d1 d2 d3 =
    x_lut
      (~:i1 &: ~:i0 &: i2 |: (~:i1 &: i0 &: i3) |: (i1 &: ~:i0 &: i4) |: (i1 &: i0 &: i5))
      (d3 @: d2 @: d1 @: d0 @: sel)
  ;;

  let split n d =
    let rec f m d l =
      if n = m
      then List.rev l, d
      else (
        match d with
        | [] -> List.rev l, []
        | h :: t -> f (m + 1) t (h :: l))
    in
    f 0 d []
  ;;

  let x_mux_2 s d def =
    match d with
    | [] -> def
    | [ d ] -> x_lut4_mux2 s d def
    | [ d0; d1 ] -> x_lut4_mux2 s d0 d1
    | _ -> failwith "x_mux2"
  ;;

  let x_mux_4 s d def =
    match d with
    | [] -> def
    | [ d ] -> x_lut4_mux2 s d def
    | [ d0; d1 ] -> x_lut4_mux2 s d0 d1
    | [ d0; d1; d2 ] -> x_lut6_mux4 s d0 d1 d2 def
    | [ d0; d1; d2; d3 ] -> x_lut6_mux4 s d0 d1 d2 d3
    | _ -> failwith "x_mux4"
  ;;

  let rec x_mux_n n mf s d def =
    if n <= 4 && max_lut >= 6
    then x_mux_4 s d def
    else if n <= 2
    then x_mux_2 s d def
    else (
      let a, b = split (n / 2) d in
      (List.hd_exn mf)
        (msb s)
        (x_mux_n (n / 2) (List.tl_exn mf) (lsbs s) a def)
        (x_mux_n (n / 2) (List.tl_exn mf) (lsbs s) b def))
  ;;

  let muxfn n =
    let f m s d0 d1 = if Uid.equal (uid d0) (uid d1) then d0 else m d0 d1 s in
    let d =
      match n with
      | 5 -> assert false
      | 4 -> [ X.muxf8; X.muxf7; X.muxf6; X.muxf5 ]
      | 3 -> [ X.muxf7; X.muxf6; X.muxf5 ]
      | 2 -> [ X.muxf6; X.muxf5 ]
      | 1 -> [ X.muxf5 ]
      | _ -> []
    in
    List.map d ~f
  ;;

  (* This assumes that all arch's have muxf5/6/7/8, but they dont.  V5 seems to only have
     muxf7/8 ??? *)
  let x_mux_bit s d =
    let l_max, l_off = if max_lut >= 6 then 6, 2 else 5, 1 in
    let def = List.hd_exn (List.rev d) in
    let rec build s d =
      let l = width s in
      let l = min l_max l in
      let n = 1 lsl l in
      let muxfn = muxfn (l - l_off) in
      let rec build2 s d =
        match d with
        | [] -> []
        | _ ->
          let a, b = split (1 lsl l) d in
          x_mux_n n muxfn s a def :: build2 s b
      in
      let d = build2 (select s (l - 1) 0) d in
      if l = width s then List.hd_exn d else build (select s (width s - 1) l) d
    in
    build s d
  ;;

  let x_mux s d =
    let w = width (List.hd_exn d) in
    let rec mux_bits i =
      if i = w
      then []
      else (
        let d = List.map d ~f:(fun s -> bit s i) in
        x_mux_bit s d :: mux_bits (i + 1))
    in
    mux_bits 0 |> List.rev |> Signal.concat_msb
  ;;

  (* multiplier *)
  let x_mul sign a b =
    let out_width = width a + width b in
    let ex a = if sign then msb a else S.gnd in
    let x_mul_lut a0 a1 b0 b1 carry =
      let o = x_lut ((i0 &: i1) ^: (i2 &: i3)) (b0 @: a1 @: b1 @: a0) in
      let a = X.mult_and a0 b1 in
      let c = X.muxcy carry a o in
      let s = X.xorcy carry o in
      c, s
    in
    let x_mul_2 a b =
      let a1 = concat_msb [ ex a; ex a; a ] |> bits_lsb in
      let a0 = concat_msb [ ex a; a; S.gnd ] |> bits_lsb in
      let rec build a0 a1 b0 b1 c =
        match a0, a1 with
        | [], [] -> []
        | [ a0 ], [ a1 ] -> [ snd (x_mul_lut a0 a1 b0 b1 c) ]
        | a0 :: a0t, a1 :: a1t ->
          let c, s = x_mul_lut a0 a1 b0 b1 c in
          s :: build a0t a1t b0 b1 c
        | _ -> failwith "x_mul_2"
      in
      build a0 a1 (bit b 0) (bit b 1) S.gnd |> concat_lsb
    in
    let x_mul_1 a b =
      let a = concat_msb [ ex a; ex a; a ] in
      x_and a (repeat b (width a))
    in
    let rec build_products i a b =
      match width b with
      | 1 -> [ i, x_mul_1 a b ]
      | 2 -> [ i, x_mul_2 a (select b 1 0) ]
      | _ -> (i, x_mul_2 a (select b 1 0)) :: build_products (i + 2) a (msbs (msbs b))
    in
    let rec adder_tree pp =
      let rec adder' level pp =
        match pp with
        | [] -> []
        | [ (i, p) ] -> [ i, p @: zero level ]
        | (_, p0) :: (i1, p1) :: tl ->
          (i1, x_add (repeat (ex p0) level @: p0) (p1 @: zero level)) :: adder' level tl
      in
      match pp with
      | [] -> failwith "adder_tree"
      | [ a ] -> a
      | (i0, _) :: (i1, _) :: _ -> adder_tree (adder' (i1 - i0) pp)
    in
    select (snd (adder_tree (build_products 0 a b))) (out_width - 1) 0
  ;;

  let x_mulu a b = x_mul false a b

  let x_muls a b =
    (* note; use x_mux_sub below instead *)
    match width b with
    | 0 -> failwith "x_muls 'b' is empty"
    | 1 ->
      let z = zero (width a + width b) in
      x_mux b [ z; x_sub z (msb a @: a) ]
    | _ ->
      let m = x_mul true a (lsbs b) in
      x_sub
        (msb m @: m)
        (x_mux (msb b) [ zero (width a + width b); msb a @: a @: zero (width b - 1) ])
  ;;
end

(* Generate full Comb.S API for Xilinx primitives *)
module XComb (Synth : T) = struct
  type t = Signal.t

  include (Signal : Comb.Primitives with type t := t)

  let ( &: ) = Synth.x_and
  let ( |: ) = Synth.x_or
  let ( ^: ) = Synth.x_xor
  let ( ~: ) = Synth.x_not
  let ( +: ) = Synth.x_add
  let ( -: ) = Synth.x_sub
  let ( ==: ) = Synth.x_eq
  let ( <: ) = Synth.x_lt
  let ( *: ) = Synth.x_mulu
  let ( *+ ) = Synth.x_muls
  let mux = Synth.x_mux
end

module XSynthesizeComb (X : S) (L : LutSize) =
  Transform.MakeCombTransform (XComb (XMake (X) (L)))

module XSynthesize (X : S) (L : LutSize) = struct
  module C = Comb.Make (XComb (XMake (X) (L)))
  open C
  module T = Transform.MakeCombTransform (C)

  let transform find (signal : Signal.t) =
    match signal with
    | Reg { register = r; d; _ } ->
      let find_uid x = Signal.uid x |> find in
      let r =
        { r with
          (* note; level constants are copied *)
          reg_clock = find_uid r.reg_clock
        ; reg_reset = find_uid r.reg_reset
        ; reg_reset_value = find_uid r.reg_reset_value
        ; reg_clear = find_uid r.reg_clear
        ; reg_clear_value = find_uid r.reg_clear_value
        ; reg_enable = find_uid r.reg_enable
        }
      in
      let vreset i =
        if is_empty r.reg_reset_value
        then false
        else (
          let c = Signal.const_value r.reg_reset_value |> Bits.to_bstr in
          Char.equal c.[i] '1')
        (* note; not [w-i-1] because we map backwards... *)
      in
      let reset =
        if is_empty r.reg_reset
        then gnd
        else (
          match r.reg_reset_edge with
          | Falling -> ~:(r.reg_reset)
          | Rising -> r.reg_reset)
      in
      let clear =
        if is_empty r.reg_clear
        then gnd
        else (
          match r.reg_clear_level with
          | Low -> ~:(r.reg_clear)
          | High -> r.reg_clear)
      in
      let clk =
        match r.reg_clock_edge with
        | Falling -> ~:(r.reg_clock)
        | Rising -> r.reg_clock
      in
      let d = find_uid d in
      let ena, d =
        if is_empty r.reg_clear
        then r.reg_enable, d
        else if is_empty r.reg_clear_value
        then r.reg_enable |: clear, mux2 clear (zero (width signal)) d
        else r.reg_enable |: clear, mux2 clear r.reg_clear_value d
      in
      List.mapi (bits_msb d) ~f:(fun i d ->
        if vreset i then X.fdpe clk ena reset d else X.fdce clk ena reset d)
      |> concat_msb
    | _ -> T.transform find signal
  ;;
end
