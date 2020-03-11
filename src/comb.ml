open! Import
include Comb_intf

module Make_primitives (Gates : Gates) = struct
  include Gates

  (* utils *)
  let vdd = of_constant (Constant.of_int ~width:1 1)
  let gnd = of_constant (Constant.of_int ~width:1 0)

  let bits_lsb x =
    let w = width x in
    Array.to_list (Array.init w ~f:(fun i -> select x i i))
  ;;

  let reduce_bits def op a = List.fold (bits_lsb a) ~init:def ~f:op

  let repeat s n =
    if n <= 0 then empty else concat_msb (Array.to_list (Array.init n ~f:(fun _ -> s)))
  ;;

  let concat_msb_e l =
    let x = List.filter l ~f:(fun t -> not (is_empty t)) in
    if List.is_empty x then empty else concat_msb x
  ;;

  let ripple_carry_adder cin a b =
    let fa cin a b =
      let sum = (a ^: b) ^: cin in
      let carry = a &: b |: (b &: cin) |: (cin &: a) in
      sum, carry
    in
    let a = bits_lsb a in
    let b = bits_lsb b in
    let sum, _ =
      List.fold2_exn a b ~init:([], cin) ~f:(fun (sum_in, carry_in) a b ->
        let sum, carry_out = fa carry_in a b in
        sum :: sum_in, carry_out)
    in
    concat_msb sum
  ;;

  (** addition *)
  let ( +: ) a b = ripple_carry_adder gnd a b

  (** subtraction *)
  let ( -: ) a b = ripple_carry_adder vdd a ~:b

  (** unsigned multiplication *)
  let ( *: ) a b =
    let _, r =
      List.fold
        (bits_lsb b)
        ~init:(0, repeat gnd (width a))
        ~f:(fun (i, acc) b ->
          let acc = concat_msb_e [ gnd; acc ] in
          let a = concat_msb_e [ gnd; a; repeat gnd i ] in
          i + 1, acc +: (a &: repeat b (width a)))
    in
    r
  ;;

  (** signed multiplication *)
  let ( *+ ) a b =
    let last = width b - 1 in
    let msb x = select x (width x - 1) (width x - 1) in
    let _, r =
      List.fold
        (bits_lsb b)
        ~init:(0, repeat gnd (width a))
        ~f:(fun (i, acc) b ->
          let acc = concat_msb_e [ msb acc; acc ] in
          let a = concat_msb_e [ msb a; a; repeat gnd i ] in
          i + 1, (if i = last then (-:) else (+:)) acc (a &: repeat b (width a)))
    in
    r
  ;;

  (** equality *)
  let ( ==: ) a b =
    let eq = ~:(a &: ~:b) &: ~:(~:a &: b) in
    reduce_bits vdd ( &: ) eq
  ;;

  (** less than *)
  let ( <: ) a b =
    let w = width a in
    let a, b = concat_msb [ gnd; a ], concat_msb [ gnd; b ] in
    let d = a -: b in
    select d w w
  ;;

  (** multiplexer *)
  let mux s d =
    let mux2 sel a b =
      assert (width sel = 1);
      let s = repeat sel (width a) in
      s &: a |: (~:s &: b)
    in
    let d' = List.hd_exn (List.rev d) in
    (* Generate the 'n' input mux structure 'bottom-up'.  it works from the lsb of the
       select signal.  Pairs from the data list are mux'd together and we recurse until
       the select is complete.  Proper 'default' handling is included with the '[a]' case
       in 'zip'. *)
    let rec build s d =
      match s with
      | [] -> List.hd_exn d
      | s :: s' ->
        let rec zip l =
          match l with
          | [] -> []
          | [ a ] -> [ mux2 s d' a ]
          (* | [ a ] -> [ a ] simpler *)
          | a :: b :: tl -> mux2 s b a :: zip tl
        in
        build s' (zip d)
    in
    build (bits_lsb s) d
  ;;
end

module Make (Prims : Primitives) = struct
  type t = Prims.t

  let equal = Prims.equal
  let empty = Prims.empty
  let is_empty = Prims.is_empty
  let ( -- ) a b = Prims.( -- ) a b
  let width = Prims.width


  let[@cold] raise_arg_greater_than_zero fn x =
    raise_s
      [%message.omit_nil
        ("arg to [" ^ fn ^ "] must be >= 0")
          ~got:(x : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let address_bits_for x =
    if x < 0 then raise_arg_greater_than_zero "address_bits_for" x;
    if x <= 1 then 1 else Int.ceil_log2 x
  ;;

  let rec num_bits_to_represent x =
    if x < 0 then raise_arg_greater_than_zero "num_bits_to_represent" x;
    match x with
    | 0 | 1 -> 1
    | x -> 1 + num_bits_to_represent (x / 2)
  ;;

  let of_constant = Prims.of_constant
  let to_constant = Prims.to_constant

  (* constant generation *)

  let[@cold] raise_of_bit_string_bad_char b =
    raise_s
      [%message.omit_nil
        "[of_bit_string] got invalid binary constant"
          ~_:(b : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  module type Sexp_of_t = sig
    type t [@@deriving sexp_of]
  end

  let[@cold] raise_const_width_greater_than_zero
               (type t)
               (module X : Sexp_of_t with type t = t)
               width
               (const : t)
    =
    raise_s
      [%message.omit_nil
        "Width of constant must be greater than zero"
          (width : int)
          (const : X.t)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let of_bit_string b =
    if String.length b = 0
    then raise_const_width_greater_than_zero (module String) (String.length b) b;
    String.iter b ~f:(function
      | '0' | '1' -> ()
      | _ -> raise_of_bit_string_bad_char b);
    Prims.of_constant (Constant.of_binary_string b)
  ;;

  let of_int ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int) width v;
    of_constant (Constant.of_int ~width v)
  ;;

  let of_int32 ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int32) width v;
    of_constant (Constant.of_int32 ~width v)
  ;;

  let of_int64 ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int64) width v;
    of_constant (Constant.of_int64 ~width v)
  ;;

  let of_hex ?(signedness = Constant.Signedness.Unsigned) ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module String) width v;
    of_constant (Constant.of_hex_string ~signedness ~width v)
  ;;

  let of_bit_list b =
    if List.length b = 0
    then
      raise_const_width_greater_than_zero
        (module struct
          type t = int list [@@deriving sexp_of]
        end)
        (List.length b)
        b;
    of_constant (Constant.of_bit_list b)
  ;;

  let of_char c = of_int ~width:8 (Char.to_int c)
  let constb = of_bit_string
  let consti = of_int
  let consti32 = of_int32
  let consti64 = of_int64
  let consthu = of_hex ~signedness:Unsigned
  let consths = of_hex ~signedness:Signed
  let constibl = of_bit_list

  let[@cold] raise_concat_empty s =
    raise_s
      [%message.omit_nil
        "[concat] got [empty] input"
          ~_:(s : Prims.t list)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_concat_empty_list () =
    raise_s
      [%message.omit_nil
        "[concat] got empty list" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let concat_check_not_empty s x = if is_empty x then raise_concat_empty s

  let concat_msb s =
    List.iter s ~f:(concat_check_not_empty s);
    if List.is_empty s then raise_concat_empty_list ();
    Prims.concat_msb s
  ;;

  let concat_lsb s = concat_msb (List.rev s)
  let ( @: ) a b = concat_msb [ a; b ]
  let concat_msb_e s = concat_msb (List.filter s ~f:(fun b -> not (Prims.is_empty b)))
  let concat_lsb_e s = concat_lsb (List.filter s ~f:(fun b -> not (Prims.is_empty b)))
  let vdd = of_bit_string "1" -- "vdd"
  let gnd = of_bit_string "0" -- "gnd"
  let is_vdd t = equal t vdd
  let is_gnd t = equal t gnd
  let zero w = if w = 0 then empty else of_bit_string (String.init w ~f:(fun _ -> '0'))
  let ones w = if w = 0 then empty else of_bit_string (String.init w ~f:(fun _ -> '1'))

  let one w =
    match w with
    | 0 -> empty
    | 1 -> vdd
    | _ -> zero (w - 1) @: vdd
  ;;

  let[@cold] raise_select_hi_lo hi lo =
    raise_s
      [%message.omit_nil
        "[select] got [hi < lo]"
          (hi : int)
          (lo : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_select_out_of_bounds a hi lo =
    raise_s
      [%message.omit_nil
        "[select] indices are out of bounds"
          ~input_width:(width a : int)
          (hi : int)
          (lo : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let select a hi lo =
    if hi < lo then raise_select_hi_lo hi lo;
    if hi >= width a || lo >= width a || hi < 0 || lo < 0
    then raise_select_out_of_bounds a hi lo;
    if lo = 0 && hi = width a - 1 then a else Prims.select a hi lo
  ;;

  let select_e a hi lo =
    try select a hi lo with
    | _ -> Prims.empty
  ;;

  let msb a = select a (width a - 1) (width a - 1)
  let lsbs a = select a (width a - 2) 0
  let lsb a = select a 0 0
  let msbs a = select a (width a - 1) 1
  let bit s n = select s n n
  let drop_bottom x n = select x (width x - 1) n
  let drop_top x n = select x (width x - 1 - n) 0
  let sel_bottom x n = select x (n - 1) 0
  let sel_top x n = select x (width x - 1) (width x - n)
  let ( .:() ) x i = bit x i
  let ( .:[] ) x (hi, lo) = select x hi lo

  let ( .:+[] ) x (lsb, w) =
    match w with
    | None -> drop_bottom x lsb
    | Some width -> select x (lsb + width - 1) lsb
  ;;

  let ( .:-[] ) x (msb, w) =
    match msb with
    | None -> sel_top x w
    | Some msb -> select x msb (msb - w + 1)
  ;;

  let[@cold] raise_insert_below_0 at_offset =
    raise_s
      [%message.omit_nil
        "[insert] below bit 0"
          ~_:(at_offset : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_insert_above_msb width_from width_target at_offset =
    raise_s
      [%message.omit_nil
        "[insert] above msb of target"
          (width_from : int)
          (width_target : int)
          (at_offset : int)
          ~highest_inserted_bit:(width_from + at_offset : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let insert ~into:t f ~at_offset =
    let wt, wf = width t, width f in
    if at_offset < 0
    then raise_insert_below_0 at_offset
    else if wt < wf + at_offset
    then raise_insert_above_msb wf wt at_offset
    else if wt = wf && at_offset = 0
    then f
    else if at_offset = 0
    then select t (wt - 1) wf @: f
    else if wt = wf + at_offset
    then f @: select t (wt - wf - 1) 0
    else select t (wt - 1) (wf + at_offset) @: f @: select t (at_offset - 1) 0
  ;;

  let[@cold] raise_assert_widths_same_not_empty function_ =
    raise_s
      [%message.omit_nil
        ""
          ~_:(String.concat [ "["; function_; "] got empty list" ] : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_widths_are_different function_ inputs =
    raise_s
      [%message.omit_nil
        ""
          ~_:
            (String.concat [ "["; function_; "] got inputs of different widths" ]
             : string)
          ~_:(inputs : Prims.t list)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let width_check function_ inputs w t =
    if width t <> w then raise_widths_are_different function_ inputs
  ;;

  (* error checking *)
  let assert_widths_same function_ inputs =
    match inputs with
    | [] -> raise_assert_widths_same_not_empty function_
    | t :: ts ->
      let w = width t in
      List.iter ts ~f:(width_check function_ inputs w)
  ;;

  let[@cold] raise_operator_widths_are_different function_ arg1 arg2 =
    let inputs = [ arg1; arg2 ] in
    raise_s
      [%message.omit_nil
        ""
          ~_:
            (String.concat [ "["; function_; "] got inputs of different widths" ]
             : string)
          ~_:(inputs : Prims.t list)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let assert_operator_widths_same function_ arg1 arg2 =
    if width arg1 <> width arg2
    then raise_operator_widths_are_different function_ arg1 arg2
  ;;

  let[@cold] raise_width_not_one msg =
    raise_s
      [%message.omit_nil
        "" ~_:(msg : string) ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let assert_width_one t msg = if not (width t = 1) then raise_width_not_one msg
  let op_int_right op a b = op a (of_int ~width:(width a) b)

  let[@cold] raise_mux_too_many_inputs inputs_provided maximum_expected =
    raise_s
      [%message.omit_nil
        "[mux] got too many inputs"
          (inputs_provided : int)
          (maximum_expected : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_mux_too_few_inputs inputs_provided =
    raise_s
      [%message.omit_nil
        "[mux] got fewer than 2 inputs"
          (inputs_provided : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  (* mux *)
  let mux sel l =
    let els = List.length l in
    let max_els = 1 lsl width sel in
    assert_widths_same "mux" l;
    if els > max_els then raise_mux_too_many_inputs els max_els;
    if els < 2 then raise_mux_too_few_inputs els;
    Prims.mux sel l
  ;;

  let mux2 sel a b =
    assert_width_one sel "[mux] got select argument that is not one bit";
    mux sel [ b; a ]
  ;;

  let mux_init sel n ~f = mux sel (Array.to_list (Array.init n ~f))

  (* logical *)
  let ( &: ) a b =
    assert_operator_widths_same "&:" a b;
    Prims.( &: ) a b
  ;;

  let ( |: ) a b =
    assert_operator_widths_same "|:" a b;
    Prims.( |: ) a b
  ;;

  let ( ^: ) a b =
    assert_operator_widths_same "^:" a b;
    Prims.( ^: ) a b
  ;;

  let ( ~: ) = Prims.( ~: )
  let ( &:. ) a b = op_int_right ( &: ) a b
  let ( |:. ) a b = op_int_right ( |: ) a b
  let ( ^:. ) a b = op_int_right ( ^: ) a b

  (* arithmetic *)
  let ( +: ) a b =
    assert_operator_widths_same "+:" a b;
    Prims.( +: ) a b
  ;;

  let ( -: ) a b =
    assert_operator_widths_same "-:" a b;
    Prims.( -: ) a b
  ;;

  let ( +:. ) a b = op_int_right ( +: ) a b
  let ( -:. ) a b = op_int_right ( -: ) a b
  let negate a = zero (width a) -: a
  let ( *: ) = Prims.( *: )
  let ( *+ ) = Prims.( *+ )

  (* comparison *)
  let ( ==: ) a b =
    assert_operator_widths_same "==:" a b;
    Prims.( ==: ) a b
  ;;

  let ( <>: ) a b =
    assert_operator_widths_same "<>:" a b;
    ~:(a ==: b)
  ;;

  let ( <: ) a b =
    assert_operator_widths_same "<:" a b;
    Prims.( <: ) a b
  ;;

  let lt = ( <: )
  let ( >: ) a b = b <: a
  let ( <=: ) a b = ~:(a >: b)
  let ( >=: ) a b = ~:(a <: b)

  let ( <+ ) a b =
    let f a = ~:(msb a) @: lsbs a in
    if width a = 1 then a &: ~:b else f a <: f b
  ;;

  let ( >+ ) a b =
    let f a = ~:(msb a) @: lsbs a in
    if width a = 1 then b &: ~:a else f a >: f b
  ;;

  let ( <=+ ) a b =
    let f a = ~:(msb a) @: lsbs a in
    if width a = 1 then ~:(a >+ b) else f a <=: f b
  ;;

  let ( >=+ ) a b =
    let f a = ~:(msb a) @: lsbs a in
    if width a = 1 then ~:(a <+ b) else f a >=: f b
  ;;

  let ( ==:. ) a b = op_int_right ( ==: ) a b
  let ( <>:. ) a b = op_int_right ( <>: ) a b
  let ( <:. ) a b = op_int_right ( <: ) a b
  let ( >:. ) a b = op_int_right ( >: ) a b
  let ( <=:. ) a b = op_int_right ( <=: ) a b
  let ( >=:. ) a b = op_int_right ( >=: ) a b
  let ( <+. ) a b = op_int_right ( <+ ) a b
  let ( >+. ) a b = op_int_right ( >+ ) a b
  let ( <=+. ) a b = op_int_right ( <=+ ) a b
  let ( >=+. ) a b = op_int_right ( >=+ ) a b
  let to_string a = Prims.to_string a
  let to_int a = to_constant a |> Constant.to_int
  let to_bstr a = to_constant a |> Constant.to_binary_string
  let sexp_of_t = Prims.sexp_of_t
  let bits_lsb s = List.init (width s) ~f:(bit s)
  let bits_msb s = bits_lsb s |> List.rev
  let to_array b = Array.of_list (bits_lsb b)
  let of_array l = concat_lsb (Array.to_list l)

  (* {[
       let rec repeat s n =
         if n = 0
         then empty
         else if n = 1
         then s
         else concat [ s; repeat s (n-1) ]
     ]} *)

  (* a smarter repeat function which generates log2 as much code *)
  let repeat s n =
    match n with
    | 0 -> empty
    | 1 -> s
    | _ ->
      let rec build pwr rep_s res_s n =
        if n = 0
        then res_s
        else if pwr land n <> 0
        then build (pwr * 2) (rep_s @: rep_s) (concat_msb_e [ rep_s; res_s ]) (n - pwr)
        else build (pwr * 2) (rep_s @: rep_s) res_s n
      in
      build 1 s empty n
  ;;

  (* It doesn't seem worth providing an [_lsb] variant for this function - it just flips
     the order of the tuple which can be done in the let binding anyway. *)
  let split_in_half_msb s =
    let w = width s in
    select s (w - 1) (w / 2), select s ((w / 2) - 1) 0
  ;;

  let[@cold] raise_split_empty_input () =
    raise_s
      [%message.omit_nil
        "[split] got [empty] input" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_split_part_width part_width =
    raise_s
      [%message.omit_nil
        "[split] got [part_width <= 0]"
          (part_width : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_split_inexact_split t_in part_width t =
    raise_s
      [%message.omit_nil
        "[split ~exact:true] unable to split exactly"
          ~input_width:(width t_in : int)
          (part_width : int)
          ~width_of_last_part:(width t : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let split ?(exact = true) ~part_width ~sel ~drop t_in =
    if is_empty t_in then raise_split_empty_input ();
    if part_width <= 0 then raise_split_part_width part_width;
    let rec split t =
      if width t < part_width && exact then raise_split_inexact_split t_in part_width t;
      if width t <= part_width
      then [ t ]
      else sel t part_width :: split (drop t part_width)
    in
    split t_in
  ;;

  let split_lsb ?exact ~part_width =
    split ?exact ~part_width ~sel:sel_bottom ~drop:drop_bottom
  ;;

  let split_msb ?exact ~part_width = split ?exact ~part_width ~sel:sel_top ~drop:drop_top

  let bswap x =
    let actual_width = width x in
    if actual_width % 8 <> 0
    then
      raise_s
        [%message
          "bswap argument must be a multiple of 8 bits width" (actual_width : int)];
    split_lsb ~exact:true ~part_width:8 x |> concat_msb
  ;;

  let[@cold] raise_shift_negative op shift =
    raise_s
      [%message.omit_nil
        (op ^ " got negative shift")
          ~_:(shift : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let sll a shift =
    if shift < 0 then raise_shift_negative "[sll]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then zero (width a)
    else concat_msb [ select a (width a - 1 - shift) 0; zero shift ]
  ;;

  let srl a shift =
    if shift < 0 then raise_shift_negative "[srl]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then zero (width a)
    else concat_msb [ zero shift; select a (width a - 1) shift ]
  ;;

  let sra a shift =
    if shift < 0 then raise_shift_negative "[sra]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then repeat (msb a) (width a)
    else concat_msb [ repeat (msb a) shift; select a (width a - 1) shift ]
  ;;

  let rec rotl d shift =
    let width = width d in
    if shift < 0
    then raise_shift_negative "[rotl]" shift
    else if shift = 0
    then d
    else if shift >= width
    then rotl d (shift % width)
    else select d (width - shift - 1) 0 @: select d (width - 1) (width - shift)
  ;;

  let rec rotr d shift =
    let width = width d in
    if shift < 0
    then raise_shift_negative "[rotr]" shift
    else if shift = 0
    then d
    else if shift >= width
    then rotr d (shift % width)
    else select d (shift - 1) 0 @: select d (width - 1) shift
  ;;

  let log_shift op a b =
    let rec sft a n =
      if n = width b
      then a
      else (
        let s = mux2 (bit b n) (op a (1 lsl n)) a in
        sft s (n + 1))
    in
    sft a 0
  ;;

  let uresize s w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat_msb [ repeat gnd (w - x); s ]
    else select s (w - 1) 0
  ;;

  let sresize s w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat_msb [ repeat (msb s) (w - x); s ]
    else select s (w - 1) 0
  ;;

  let ue s = uresize s (width s + 1)
  let se s = sresize s (width s + 1)

  let resize_list ~resize l =
    let w = List.fold l ~init:0 ~f:(fun w e -> max (width e) w) in
    List.map l ~f:(fun e -> resize e w)
  ;;

  let resize_op2 ~resize f a b =
    let w = max (width a) (width b) in
    let a, b = resize a w, resize b w in
    f a b
  ;;

  let to_sint a = to_int (sresize a Int.num_bits)

  let[@cold] raise_reduce_empty_list () =
    raise_s
      [%message.omit_nil
        "[reduce] got empty list" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let reduce ~f:op s =
    match List.length s with
    | 0 -> raise_reduce_empty_list ()
    | _ -> List.reduce_exn s ~f:(fun acc x -> op acc x)
  ;;

  let ( ||: ) a b = reduce ~f:( |: ) (bits_msb a) |: reduce ~f:( |: ) (bits_msb b)
  let ( &&: ) a b = reduce ~f:( |: ) (bits_msb a) &: reduce ~f:( |: ) (bits_msb b)
  let reverse a = concat_msb (bits_lsb a)

  let[@cold] raise_mod_counter_limit limit max_value =
    raise_s
      [%message
        "mod counter limit is great than max counter value"
          (limit : int)
          (max_value : int)]
  ;;

  let mod_counter ~max c =
    let w = width c in
    let counter_max_value = (1 lsl w) - 1 in
    if max > counter_max_value
    then raise_mod_counter_limit max counter_max_value
    else if counter_max_value = max
    then c +: one w
    else mux2 (c ==: of_int ~width:w max) (zero w) (c +: one w)
  ;;

  let[@cold] raise_tree_invalid_arity () =
    raise_s
      [%message.omit_nil
        "[tree] got [arity <= 1]" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_tree_empty_list () =
    raise_s
      [%message.omit_nil
        "[tree] got empty list" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let rec tree ~arity ~f l =
    if arity <= 1 then raise_tree_invalid_arity ();
    let split l n =
      let lh, ll, _ =
        List.fold l ~init:([], [], 0) ~f:(fun (l0, l1, m) e ->
          if m < n then e :: l0, l1, m + 1 else l0, e :: l1, m + 1)
      in
      List.rev lh, List.rev ll
    in
    let rec t0 l =
      let l0, l1 = split l arity in
      if List.is_empty l1 then [ f l0 ] else f l0 :: t0 l1
    in
    match l with
    | [] -> raise_tree_empty_list ()
    | [ a ] -> a
    | _ -> tree ~arity ~f (t0 l)
  ;;

  let[@cold] raise_tree_or_reduce_empty_list () =
    raise_s
      [%message.omit_nil
        "[tree_or_reduce_binary_operator] got empty list"
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_tree_or_reduce_branching_factor branching_factor =
    raise_s
      [%message.omit_nil
        "[tree_or_reduce_binary_operator] got [branching_factor < 1]"
          (branching_factor : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let tree_or_reduce_binary_operator ?(branching_factor = 2) ~f data =
    if List.is_empty data then raise_tree_or_reduce_empty_list ();
    if branching_factor < 1 then raise_tree_or_reduce_branching_factor branching_factor;
    if branching_factor = 1
    then reduce ~f data
    else tree ~arity:branching_factor ~f:(reduce ~f) data
  ;;

  let priority_select ?branching_factor ts =
    tree_or_reduce_binary_operator ts ?branching_factor ~f:(fun (a : t With_valid.t) b ->
      { With_valid.valid = a.valid |: b.valid; value = mux2 a.valid a.value b.value })
  ;;

  let priority_select_with_default ?branching_factor data ~default =
    let d = priority_select data ?branching_factor in
    mux2 d.valid d.value default
  ;;

  let onehot_select ?branching_factor (ts : t With_valid.t list) =
    List.map ts ~f:(fun d -> sresize d.valid (width d.value) &: d.value)
    |> tree_or_reduce_binary_operator ?branching_factor ~f:( |: )
  ;;

  let[@cold] raise_of_empty function_ =
    raise_s
      [%message.omit_nil
        "" ~_:(function_ ^ " of [empty]") ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let popcount ?branching_factor t =
    let width = width t in
    if width = 0 then raise_of_empty "[popcount]";
    let result_width = Int.ceil_log2 (width + 1) in
    tree_or_reduce_binary_operator
      ?branching_factor
      ~f:( +: )
      (List.map (bits_msb t) ~f:(fun d -> uresize d result_width))
  ;;

  let leading_zeros_of_bits_list ?branching_factor d =
    let result_width = num_bits_to_represent (List.length d) in
    List.mapi d ~f:(fun i valid ->
      { With_valid.valid; value = of_int ~width:result_width i })
    |> priority_select_with_default
         ?branching_factor
         ~default:(of_int ~width:result_width (List.length d))
  ;;

  let leading_ones ?branching_factor t =
    if width t = 0 then raise_of_empty "[leading_ones]";
    leading_zeros_of_bits_list (bits_msb ~:t) ?branching_factor
  ;;

  let trailing_ones ?branching_factor t =
    if width t = 0 then raise_of_empty "[trailing_ones]";
    leading_zeros_of_bits_list (bits_lsb ~:t) ?branching_factor
  ;;

  let leading_zeros ?branching_factor t =
    if width t = 0 then raise_of_empty "[leading_zeros]";
    leading_zeros_of_bits_list (bits_msb t) ?branching_factor
  ;;

  let trailing_zeros ?branching_factor t =
    if width t = 0 then raise_of_empty "[trailing_zeros]";
    leading_zeros_of_bits_list (bits_lsb t) ?branching_factor
  ;;

  let is_pow2 ?branching_factor t =
    if width t = 0 then raise_of_empty "[is_pow2]";
    if width t = 1 then t else popcount ?branching_factor t ==:. 1
  ;;

  let floor_log2 ?branching_factor t : t With_valid.t =
    let width = width t in
    if width = 0 then raise_of_empty "[floor_log2]";
    let leading_zeros = leading_zeros t ?branching_factor in
    let result_width = max 1 (Int.ceil_log2 width) in
    { valid = t <>:. 0
    ; value =
        of_int ~width:result_width (width - 1) -: uresize leading_zeros result_width
    }
  ;;

  let ceil_log2 ?branching_factor t =
    if width t = 0 then raise_of_empty "[ceil_log2]";
    let is_pow2 = is_pow2 ?branching_factor t in
    let floor_log2 = floor_log2 ?branching_factor t in
    let value = ue floor_log2.value in
    { floor_log2 with value = mux2 is_pow2 value (value +:. 1) }
  ;;

  let binary_to_onehot s =
    let rec build = function
      | [] -> []
      | [ a ] -> [ a; ~:a ]
      | a :: b ->
        let b = build b in
        let l2 = List.map b ~f:(( &: ) ~:a) in
        let l1 = List.map b ~f:(( &: ) a) in
        l1 @ l2
    in
    concat_msb (build (bits_msb s))
  ;;

  let onehot_to_binary x =
    let n = num_bits_to_represent (width x - 1) in
    let x = bits_lsb x in
    let rec f i =
      if i = n
      then []
      else (
        let rec g j = function
          | [] -> []
          | h :: t ->
            let c = j land (1 lsl i) <> 0 in
            if c then h :: g (j + 1) t else g (j + 1) t
        in
        let g = g 0 x in
        match g with
        | [] -> gnd :: f (i + 1)
        | _ -> reduce ~f:( |: ) g :: f (i + 1))
    in
    concat_lsb (f 0)
  ;;

  let binary_to_gray b =
    if width b < 1
    then raise_s [%message "[binary_to_gray] width < 1"]
    else if width b = 1
    then b
    else b ^: srl b 1
  ;;

  let gray_to_binary b =
    if width b < 1
    then raise_s [%message "[gray_to_binary] width < 1"]
    else if width b = 1
    then b
    else (
      let ue x = uresize x (width b) in
      let rec f b mask =
        let b = b ^: ue mask in
        if width mask = 1 then b else f b (msbs mask)
      in
      f b (msbs b))
  ;;

  let[@cold] raise_of_decimal_string_invalid_decimal_char v =
    raise_s
      [%message.omit_nil
        "[of_decimal_string] got invalid decimal char"
          ~_:(v : char)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_of_decimal_string_empty_string () =
    raise_s
      [%message.omit_nil
        "[of_decimal_string] got empty string"
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  (* complex constant generators *)
  let rec of_decimal_string ~width:bits v =
    let l = String.length v in
    let decimal v =
      match v with
      | '0' -> of_int ~width:4 0
      | '1' -> of_int ~width:4 1
      | '2' -> of_int ~width:4 2
      | '3' -> of_int ~width:4 3
      | '4' -> of_int ~width:4 4
      | '5' -> of_int ~width:4 5
      | '6' -> of_int ~width:4 6
      | '7' -> of_int ~width:4 7
      | '8' -> of_int ~width:4 8
      | '9' -> of_int ~width:4 9
      | _ -> raise_of_decimal_string_invalid_decimal_char v
    in
    let ( +: ) a b =
      let w = max (width a) (width b) + 1 in
      let a, b = uresize a w, uresize b w in
      a +: b
    in
    let ten = of_int ~width:4 10 in
    if l = 0
    then raise_of_decimal_string_empty_string ()
    else if Char.equal v.[0] '-'
    then zero bits -: of_decimal_string ~width:bits (String.sub v ~pos:1 ~len:(l - 1))
    else (
      (* convert *)
      let rec sum i mulfac prod =
        if i < 0
        then prod
        else sum (i - 1) (mulfac *: ten) (prod +: (decimal v.[i] *: mulfac))
      in
      uresize (sum (l - 1) (of_int ~width:1 1) (of_int ~width:1 0)) bits)
  ;;

  let[@cold] raise_of_verilog_format_missing_tick s =
    raise_s
      [%message.omit_nil
        "[of_verilog_format] missing [']"
          ~_:(s : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_of_verilog_format_missing_count s =
    raise_s
      [%message.omit_nil
        "[of_verilog_format] missing bit count"
          ~_:(s : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_of_verilog_format_too_short s =
    raise_s
      [%message.omit_nil
        "[of_verilog_format] value shorter than 2 characters"
          ~_:(s : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_of_verilog_format_bad_control_char s =
    raise_s
      [%message.omit_nil
        "[of_verilog_format] bad control character"
          ~const:(s : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let of_verilog_format s =
    let slen, sval =
      let rec split2 n c s t =
        if Char.equal t.[n] c
        then s, String.sub t ~pos:(n + 1) ~len:(String.length t - n - 1)
        else split2 (n + 1) c (s ^ String.make 1 t.[n]) t
      in
      let s0, s1 =
        try split2 0 '\'' "" s with
        | _ -> raise_of_verilog_format_missing_tick s
      in
      if String.length s0 = 0 then raise_of_verilog_format_missing_count s;
      if String.length s1 < 2 then raise_of_verilog_format_too_short s;
      s0, s1
    in
    let len = Int.of_string slen in
    let ctrl = sval.[0] in
    let sval = String.sub sval ~pos:1 ~len:(String.length sval - 1) in
    match ctrl with
    | 'd' -> of_decimal_string ~width:len sval
    | 'x' | 'h' -> of_hex ~signedness:Unsigned ~width:len sval
    | 'X' | 'H' -> of_hex ~signedness:Signed ~width:len sval
    | 'b' ->
      let slen = String.length sval in
      if slen < len
      then of_bit_string (String.make (len - slen) '0' ^ sval)
      else if slen > len
      then of_bit_string (String.sub sval ~pos:(slen - len) ~len)
      else of_bit_string sval
    | 'B' ->
      let slen = String.length sval in
      if slen < len
      then of_bit_string (String.make (len - slen) sval.[0] ^ sval)
      else if slen > len
      then of_bit_string (String.sub sval ~pos:(slen - len) ~len)
      else of_bit_string sval
    | _ -> raise_of_verilog_format_bad_control_char s
  ;;

  let[@cold] raise_of_string_convert_error const =
    raise_s
      [%message.omit_nil
        "[of_string] could not convert constant"
          (const : string)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let of_string b =
    let b =
      String.filter b ~f:(function
        | '_' -> false
        | _ -> true)
    in
    try
      try of_verilog_format b with
      | _ -> of_bit_string b
    with
    | _ -> raise_of_string_convert_error b
  ;;

  let const = of_string
  let constv = of_verilog_format
  let constd = of_decimal_string

  let rec random ~width =
    if width <= 16
    then of_int ~width (Random.int (1 lsl width))
    else of_int ~width:16 (Random.int (1 lsl 16)) @: random ~width:(width - 16)
  ;;

  let to_int32 c = to_constant c |> Constant.to_int64 |> Int64.to_int32_trunc
  let to_sint32 c = sresize c Int32.num_bits |> to_constant |> Constant.to_int32
  let to_int64 c = to_constant c |> Constant.to_int64
  let to_sint64 c = sresize c Int64.num_bits |> to_constant |> Constant.to_int64

  let to_char x =
    let actual_width = width x in
    if actual_width <> 8
    then raise_s [%message "[to_char] signal must be 8 bits wide" (actual_width : int)];
    Char.of_int_exn (to_int x)
  ;;

  module type TypedMath = TypedMath with type t := t

  (* General arithmetic on unsigned signals.  Operands and results are resized to fit a
     appropriate. *)
  module Unsigned = struct
    type v = t

    let of_signal s = s
    let to_signal s = s
    let resize s i = uresize s i

    let re size op a b =
      let wa, wb = width a, width b in
      let w = size wa wb in
      let a, b = resize a w, resize b w in
      op a b
    ;;

    let re0 = re max
    let re1 = re (fun a b -> max a b + 1)
    let ( +: ) = re1 ( +: )
    let ( -: ) = re1 ( -: )
    let ( *: ) = ( *: )
    let ( <: ) = re0 ( <: )
    let ( >: ) = re0 ( >: )
    let ( <=: ) = re0 ( <=: )
    let ( >=: ) = re0 ( >=: )
    let ( ==: ) = re0 ( ==: )
    let ( <>: ) = re0 ( <>: )
  end

  module Signed = struct
    type v = t

    let of_signal s = s
    let to_signal s = s
    let resize s i = sresize s i

    let re size op a b =
      let wa, wb = width a, width b in
      let w = size wa wb in
      let a, b = resize a w, resize b w in
      op a b
    ;;

    let re0 = re max
    let re1 = re (fun a b -> max a b + 1)
    let ( +: ) = re1 ( +: )
    let ( -: ) = re1 ( -: )
    let ( *: ) = ( *+ )
    let ( <: ) = re0 ( <+ )
    let ( >: ) = re0 ( >+ )
    let ( <=: ) = re0 ( <=+ )
    let ( >=: ) = re0 ( >=+ )
    let ( ==: ) = re0 ( ==: )
    let ( <>: ) = re0 ( <>: )
  end

  module Uop = Unsigned
  module Sop = Signed
end
