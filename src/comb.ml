[@@@ocaml.flambda_o3]

open! Core0
include Comb_intf

module Gen_cases_from_mux (Comb : Comb_intf.Gen_cases_from_mux) = struct
  open Comb

  let cases ~default select cases =
    List.fold_right ~init:default cases ~f:(fun (match_with, value) acc ->
      mux (select ==: match_with) [ acc; value ])
  ;;
end

module Make_primitives (Gates : Gates) = struct
  include Gates

  (* utils *)
  let vdd = of_constant (Constant.of_int ~width:1 1) -- "vdd"
  let gnd = of_constant (Constant.of_int ~width:1 0) -- "gnd"

  let bits_lsb x =
    let w = width x in
    List.init w ~f:(fun i -> select x ~high:i ~low:i)
  ;;

  let reduce_bits def op a = List.fold (bits_lsb a) ~init:def ~f:op
  let repeat s n = if n <= 0 then empty else concat_msb (List.init n ~f:(fun _ -> s))

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
    let msb x = select x ~high:(width x - 1) ~low:(width x - 1) in
    let _, r =
      List.fold
        (bits_lsb b)
        ~init:(0, repeat gnd (width a))
        ~f:(fun (i, acc) b ->
          let acc = concat_msb_e [ msb acc; acc ] in
          let a = concat_msb_e [ msb a; a; repeat gnd i ] in
          i + 1, (if i = last then ( -: ) else ( +: )) acc (a &: repeat b (width a)))
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
    select d ~high:w ~low:w
  ;;

  (** multiplexer *)
  let mux s d =
    let mux2 sel a b =
      assert (width sel = 1);
      let s = repeat sel (width a) in
      s &: a |: (~:s &: b)
    in
    let d' = List.hd_exn (List.rev d) in
    (* Generate the 'n' input mux structure 'bottom-up'. it works from the lsb of the
       select signal. Pairs from the data list are mux'd together and we recurse until the
       select is complete. Proper 'default' handling is included with the '[a]' case in
       'zip'. *)
    let rec build s d =
      match s with
      | [] -> List.hd_exn d
      | s :: s' ->
        let rec zip l =
          match l with
          | [] -> []
          | [ a ] -> [ mux2 s d' a ]
          (*=| [ a ] -> [ a ] simpler *)
          | a :: b :: tl -> mux2 s b a :: zip tl
        in
        build s' (zip d)
    in
    build (bits_lsb s) d
  ;;

  include Gen_cases_from_mux (struct
      type nonrec t = t

      let mux = mux
      let ( ==: ) = ( ==: )
    end)
end

type nonrec ('a, 'b) with_valid2 = ('a, 'b) with_valid2 =
  { valid : 'a
  ; value : 'b
  }

type nonrec 'a with_valid = 'a with_valid

module Make (Prims : Primitives) = struct
  type t = Prims.t [@@deriving equal ~localize]

  let empty = Prims.empty
  let is_empty = Prims.is_empty
  let ( -- ) ~(loc : [%call_pos]) a b = Prims.( -- ) ~loc a b
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

  let of_int_trunc ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int) width v;
    of_constant (Constant.of_int ~width v)
  ;;

  let of_int = of_int_trunc

  let of_unsigned
    (type a)
    (module Int : Base.Int.S with type t = a)
    (f : width:int -> a -> t)
    ~width
    x
    =
    if Int.( < ) x Int.zero
    then raise_s [%message "[of_unsigned_int] input value is less than 0" (x : Int.t)];
    let max_value =
      if width >= Int.(num_bits |> to_int_exn) - 1
      then Int.max_value
      else Int.((Int.one lsl width) - Int.one)
    in
    if Int.( > ) x max_value
    then
      raise_s
        [%message
          "[of_unsigned_int] input value is too large for given width"
            (width : int)
            (max_value : Int.t)
            (x : Int.t)];
    f ~width x
  ;;

  let of_signed
    (type a)
    (module Int : Base.Int.S with type t = a)
    (f : width:int -> a -> t)
    ~width
    x
    =
    let max_value, min_value =
      if width >= Int.(num_bits |> to_int_exn)
      then Int.max_value, Int.min_value
      else (
        let width = width - 1 in
        Int.((Int.one lsl width) - Int.one), Int.(-(Int.one lsl width)))
    in
    if Int.( > ) x max_value
    then
      raise_s
        [%message
          "[of_signed_int] input value is too large for given width"
            (width : int)
            (max_value : Int.t)
            (x : Int.t)];
    if Int.( < ) x min_value
    then
      raise_s
        [%message
          "[of_signed_int] input value is too small for given width"
            (width : int)
            (min_value : Int.t)
            (x : Int.t)];
    f ~width x
  ;;

  let of_int32_trunc ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int32) width v;
    of_constant (Constant.of_int32 ~width v)
  ;;

  let of_int32 = of_int32_trunc

  let of_int64_trunc ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module Int64) width v;
    of_constant (Constant.of_int64 ~width v)
  ;;

  let of_int64 = of_int64_trunc
  let of_unsigned_int = of_unsigned (module Int) of_int_trunc
  let of_unsigned_int32 = of_unsigned (module Int32) of_int32_trunc
  let of_unsigned_int64 = of_unsigned (module Int64) of_int64_trunc
  let of_signed_int = of_signed (module Int) of_int_trunc
  let of_signed_int32 = of_signed (module Int32) of_int32_trunc
  let of_signed_int64 = of_signed (module Int64) of_int64_trunc

  let of_hex ?(signedness = Signedness.Unsigned) ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module String) width v;
    of_constant (Constant.of_hex_string ~signedness ~width v)
  ;;

  let of_octal ?(signedness = Signedness.Unsigned) ~width v =
    if width <= 0 then raise_const_width_greater_than_zero (module String) width v;
    of_constant (Constant.of_octal_string ~signedness ~width v)
  ;;

  let of_bigint ~width v = of_constant (Constant.of_bigint ~width v)

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

  let of_char c = of_int_trunc ~width:8 (Char.to_int c)

  let of_ascii_string_msb s =
    if String.is_empty s
    then
      raise_s
        [%message.omit_nil
          "[of_ascii_string] got empty string"
            ~loc:(Caller_id.get () : Caller_id.t option)];
    s |> String.to_list |> List.map ~f:of_char |> Prims.concat_msb
  ;;

  let of_ascii_string_lsb s = String.rev s |> of_ascii_string_msb

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
  let vdd = Prims.vdd
  let gnd = Prims.gnd
  let is_vdd t = equal t vdd
  let is_gnd t = equal t gnd

  let[@cold] raise_zero_width fn =
    raise_s
      [%message.omit_nil
        "" ~_:(fn ^ " has zero width") ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let zero w =
    if w = 0
    then raise_zero_width "[zero]"
    else of_bit_string (String.init w ~f:(fun _ -> '0'))
  ;;

  let ones w =
    if w = 0
    then raise_zero_width "[ones]"
    else of_bit_string (String.init w ~f:(fun _ -> '1'))
  ;;

  let one w =
    match w with
    | 0 -> raise_zero_width "[one]"
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

  let select a ~high:hi ~low:lo =
    if hi < lo then raise_select_hi_lo hi lo;
    if hi >= width a || lo >= width a || hi < 0 || lo < 0
    then raise_select_out_of_bounds a hi lo;
    if lo = 0 && hi = width a - 1 then a else Prims.select a ~high:hi ~low:lo
  ;;

  let msb a = select a ~high:(width a - 1) ~low:(width a - 1)
  let lsbs a = select a ~high:(width a - 2) ~low:0
  let lsb a = select a ~high:0 ~low:0
  let msbs a = select a ~high:(width a - 1) ~low:1
  let bit s ~pos:n = select s ~high:n ~low:n
  let drop_bottom x ~width:n = select x ~high:(width x - 1) ~low:n
  let drop_top x ~width:n = select x ~high:(width x - 1 - n) ~low:0
  let sel_bottom x ~width:n = select x ~high:(n - 1) ~low:0
  let sel_top x ~width:n = select x ~high:(width x - 1) ~low:(width x - n)
  let ( .:() ) x i = bit x ~pos:i
  let ( .:[] ) x (high, low) = select x ~high ~low

  let ( .:+[] ) x (lsb, w) =
    match w with
    | None -> drop_bottom x ~width:lsb
    | Some width -> select x ~high:(lsb + width - 1) ~low:lsb
  ;;

  let ( .:-[] ) x (msb, w) =
    match msb with
    | None -> sel_top x ~width:w
    | Some msb -> select x ~high:msb ~low:(msb - w + 1)
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
    then select t ~high:(wt - 1) ~low:wf @: f
    else if wt = wf + at_offset
    then f @: select t ~high:(wt - wf - 1) ~low:0
    else
      select t ~high:(wt - 1) ~low:(wf + at_offset)
      @: f
      @: select t ~high:(at_offset - 1) ~low:0
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

  let op_int_right_unsigned op_name op left right =
    match Or_error.try_with (fun () -> of_unsigned_int ~width:(width left) right) with
    | Ok right -> op left right
    | Error conversion_error ->
      raise_s
        [%message
          "Failed to perform unsigned integer conversion on dotted operator"
            (op_name : string)
            (conversion_error : Error.t)]
  ;;

  let op_int_right_signed op_name op left right =
    match Or_error.try_with (fun () -> of_signed_int ~width:(width left) right) with
    | Ok right -> op left right
    | Error conversion_error ->
      raise_s
        [%message
          "Failed to perform signed integer conversion on dotted operator"
            (op_name : string)
            (conversion_error : Error.t)]
  ;;

  let op_int_right op_name op_unsigned op_signed =
    op_int_right_unsigned op_name op_unsigned, op_int_right_signed op_name op_signed
  ;;

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

  let[@cold] raise_mux_no_inputs () =
    raise_s
      [%message.omit_nil
        "[mux] got no data inputs" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_mux_not_exact width_of_sel number_of_data_elements =
    raise_s
      [%message.omit_nil
        "[mux] select does not exactly index the number of data elements"
          (width_of_sel : int)
          (number_of_data_elements : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  (* We rely on being able to convert the select to an unsigned integer. This is slightly
     more restrictive than necessary so we can do [1 lsl max_mux_select_bits]. *)
  let max_mux_select_bits = Int.num_bits - 2

  let raise_width_of_sel_too_large width_of_sel =
    raise_s
      [%message.omit_nil
        "[mux] select width must fit within an (unsigned) integer value"
          (width_of_sel : int)
          (max_mux_select_bits : int)]
  ;;

  (* mux *)
  let mux sel l =
    let width_of_sel = width sel in
    if width_of_sel > max_mux_select_bits then raise_width_of_sel_too_large width_of_sel;
    let number_of_data_elements = List.length l in
    let max_number_of_data_elements = 1 lsl width_of_sel in
    assert_widths_same "mux" l;
    if number_of_data_elements > max_number_of_data_elements
    then raise_mux_too_many_inputs number_of_data_elements max_number_of_data_elements;
    match number_of_data_elements with
    | 0 -> raise_mux_no_inputs ()
    | 1 -> List.hd_exn l
    | _ -> Prims.mux sel l
  ;;

  let mux_strict sel l =
    let width_of_sel = width sel in
    if width_of_sel > max_mux_select_bits then raise_width_of_sel_too_large width_of_sel;
    let number_of_data_elements = List.length l in
    (match number_of_data_elements with
     | 0 | 1 -> raise_mux_too_few_inputs number_of_data_elements
     | _ ->
       let expected_number_of_data_elements = 1 lsl width_of_sel in
       if expected_number_of_data_elements <> number_of_data_elements
       then raise_mux_not_exact (width sel) number_of_data_elements);
    mux sel l
  ;;

  let mux2 sel a b =
    assert_width_one sel "[mux2] got select argument that is not one bit";
    mux sel [ b; a ]
  ;;

  let mux_init sel n ~f = mux sel (List.init n ~f)

  let cases ~default select cases =
    if List.length cases = 0 then raise_s [%message "[cases] no cases specified]"];
    assert_widths_same "cases" (default :: List.map cases ~f:snd);
    List.iter cases ~f:(fun (match_width, _) ->
      if width match_width <> width select
      then raise_s [%message "[cases] match width is not equal to select width"]);
    Prims.cases ~default select cases
  ;;

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
  let ( &:. ), ( &+. ) = op_int_right "and" ( &: ) ( &: )
  let ( |:. ), ( |+. ) = op_int_right "or" ( |: ) ( |: )
  let ( ^:. ), ( ^+. ) = op_int_right "xor" ( ^: ) ( ^: )

  (* arithmetic *)
  let ( +: ) a b =
    assert_operator_widths_same "+:" a b;
    Prims.( +: ) a b
  ;;

  let ( -: ) a b =
    assert_operator_widths_same "-:" a b;
    Prims.( -: ) a b
  ;;

  let ( +:. ), ( ++. ) = op_int_right "add" ( +: ) ( +: )
  let ( -:. ), ( -+. ) = op_int_right "sub" ( -: ) ( -: )
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

  let ( ==:. ), ( ==+. ) = op_int_right "equals" ( ==: ) ( ==: )
  let ( <>:. ), ( <>+. ) = op_int_right "not equals" ( <>: ) ( <>: )
  let ( <:. ), ( <+. ) = op_int_right "less than" ( <: ) ( <+ )
  let ( >:. ), ( >+. ) = op_int_right "greater than" ( >: ) ( >+ )
  let ( <=:. ), ( <=+. ) = op_int_right "less than or equal" ( <=: ) ( <=+ )
  let ( >=:. ), ( >=+. ) = op_int_right "greater than or equal" ( >=: ) ( >=+ )

  (* propositional logic implication *)
  let ( -->: ) a b = ~:a |: b
  let to_string a = Prims.to_string a
  let to_int_trunc a = to_constant a |> Constant.to_int
  let to_int = to_int_trunc
  let to_bstr a = to_constant a |> Constant.to_binary_string
  let sexp_of_t = Prims.sexp_of_t
  let bits_lsb s = List.init (width s) ~f:(fun pos -> s.:(pos))
  let bits_msb s = bits_lsb s |> List.rev
  let to_array b = Array.of_list (bits_lsb b)
  let of_array l = concat_lsb (Array.to_list l)
  let to_bigint b = to_constant b |> Constant.to_bigint
  let of_bool b = if b then vdd else gnd

  (* {[
       let rec repeat s n =
         if n = 0 then empty else if n = 1 then s else concat [ s; repeat s (n - 1) ]
       ;;
     ]} *)

  let[@cold] raise_repeat_negative_times () =
    raise_s
      [%message.omit_nil
        "Cannot [repeat] negative times" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_repeat_zero_times () =
    raise_s
      [%message.omit_nil
        "Cannot [repeat] zero times" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let[@cold] raise_repeat_empty_signal () =
    raise_s
      [%message.omit_nil
        "Cannot [repeat] empty signal" ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  (* a smarter repeat function which generates log2 as much code *)
  let repeat s ~count:n =
    if is_empty s
    then raise_repeat_empty_signal ()
    else (
      match n with
      | _ when n < 0 -> raise_repeat_negative_times ()
      | 0 -> raise_repeat_zero_times ()
      | 1 -> s
      | n ->
        let cat t s =
          match t, s with
          | t, None -> Some t
          | t, Some s -> Some (t @: s)
        in
        let rec build pwr rep_s res_s n =
          if n = 0
          then Option.value_exn res_s
          else if pwr land n <> 0
          then build (pwr * 2) (rep_s @: rep_s) (cat rep_s res_s) (n - pwr)
          else build (pwr * 2) (rep_s @: rep_s) res_s n
        in
        build 1 s None n)
  ;;

  let split_in_half_msb ?msbs s =
    let msbs = Option.value msbs ~default:((width s + 1) / 2) in
    sel_top s ~width:msbs, drop_top s ~width:msbs
  ;;

  let split_in_half_lsb ?lsbs s =
    let lsbs = Option.value lsbs ~default:((width s + 1) / 2) in
    drop_bottom s ~width:lsbs, sel_bottom s ~width:lsbs
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
      else sel t ~width:part_width :: split (drop t ~width:part_width)
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

  let sll a ~by:shift =
    if shift < 0 then raise_shift_negative "[sll]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then zero (width a)
    else concat_msb [ select a ~high:(width a - 1 - shift) ~low:0; zero shift ]
  ;;

  let srl a ~by:shift =
    if shift < 0 then raise_shift_negative "[srl]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then zero (width a)
    else concat_msb [ zero shift; select a ~high:(width a - 1) ~low:shift ]
  ;;

  let sra a ~by:shift =
    if shift < 0 then raise_shift_negative "[sra]" shift;
    if shift = 0
    then a
    else if shift >= width a
    then repeat (msb a) ~count:(width a)
    else
      concat_msb [ repeat (msb a) ~count:shift; select a ~high:(width a - 1) ~low:shift ]
  ;;

  let rec rotl d ~by:shift =
    let width = width d in
    if shift < 0
    then raise_shift_negative "[rotl]" shift
    else if shift = 0
    then d
    else if shift >= width
    then rotl d ~by:(shift % width)
    else
      select d ~high:(width - shift - 1) ~low:0
      @: select d ~high:(width - 1) ~low:(width - shift)
  ;;

  let rec rotr d ~by:shift =
    let width = width d in
    if shift < 0
    then raise_shift_negative "[rotr]" shift
    else if shift = 0
    then d
    else if shift >= width
    then rotr d ~by:(shift % width)
    else select d ~high:(shift - 1) ~low:0 @: select d ~high:(width - 1) ~low:shift
  ;;

  let log_shift ~f:op a ~by:b =
    let rec sft a n =
      if n = width b
      then a
      else (
        let s = mux2 b.:(n) (op a ~by:(1 lsl n)) a in
        sft s (n + 1))
    in
    sft a 0
  ;;

  let uresize s ~width:w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat_msb [ repeat gnd ~count:(w - x); s ]
    else select s ~high:(w - 1) ~low:0
  ;;

  let sresize s ~width:w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat_msb [ repeat (msb s) ~count:(w - x); s ]
    else select s ~high:(w - 1) ~low:0
  ;;

  let[@cold] raise_extend_tried_to_shrink op signal_width width =
    raise_s
      [%message.omit_nil
        (op ^ " got a width smaller than the original signal width")
          (signal_width : int)
          (width : int)
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  let uextend s ~width:w =
    let x = width s in
    if w < x
    then raise_extend_tried_to_shrink "[uextend]" x w
    else (uresize [@inlined hint]) s ~width:w
  ;;

  let sextend s ~width:w =
    let x = width s in
    if w < x
    then raise_extend_tried_to_shrink "[sextend]" x w
    else (sresize [@inlined hint]) s ~width:w
  ;;

  let ue s = uresize s ~width:(width s + 1)
  let se s = sresize s ~width:(width s + 1)

  let resize_list ~resize l =
    let w = List.fold l ~init:0 ~f:(fun w e -> max (width e) w) in
    List.map l ~f:(fun e -> resize e w)
  ;;

  let resize_op2 ~resize f a b =
    let w = max (width a) (width b) in
    let a, b = resize a w, resize b w in
    f a b
  ;;

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
    else mux2 (c ==: of_int_trunc ~width:w max) (zero w) (c +: one w)
  ;;

  let compute_arity ~steps num_leaves =
    let rec croot root =
      if Int.pow root steps >= num_leaves then root else croot (root + 1)
    in
    if steps <= 0
    then raise_s [%message "[compute_arity] number of steps must be > 0"]
    else if steps = 1
    then num_leaves
    else if num_leaves = 1
    then 1
    else max 1 (croot 2)
  ;;

  let rec compute_tree_branches ~steps num =
    if num = 0
    then []
    else (
      match steps with
      | 0 -> []
      | 1 -> [ num ]
      | _ ->
        let branches = compute_arity ~steps num in
        branches
        :: compute_tree_branches ~steps:(steps - 1) ((num + branches - 1) / branches))
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
    let rec t0 l =
      let l0, l1 = List.split_n l arity in
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
    tree_or_reduce_binary_operator ts ?branching_factor ~f:(fun (a : t with_valid) b ->
      { valid = a.valid |: b.valid; value = mux2 a.valid a.value b.value })
  ;;

  let priority_select_with_default ?branching_factor data ~default =
    let d = priority_select data ?branching_factor in
    mux2 d.valid d.value default
  ;;

  let onehot_select ?branching_factor (ts : t with_valid list) =
    List.map ts ~f:(fun d -> sresize d.valid ~width:(width d.value) &: d.value)
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
      (List.map (bits_msb t) ~f:(fun d -> uresize d ~width:result_width))
  ;;

  let leading_zeros_of_bits_list ?branching_factor d =
    let result_width = num_bits_to_represent (List.length d) in
    List.mapi d ~f:(fun i valid -> { valid; value = of_int_trunc ~width:result_width i })
    |> priority_select_with_default
         ?branching_factor
         ~default:(of_int_trunc ~width:result_width (List.length d))
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

  let floor_log2 ?branching_factor t : t with_valid =
    let width = width t in
    if width = 0 then raise_of_empty "[floor_log2]";
    let leading_zeros = leading_zeros t ?branching_factor in
    let result_width = max 1 (Int.ceil_log2 width) in
    { valid = t <>:. 0
    ; value =
        of_int_trunc ~width:result_width (width - 1)
        -: uresize leading_zeros ~width:result_width
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
    else b ^: srl b ~by:1
  ;;

  let gray_to_binary b =
    if width b < 1
    then raise_s [%message "[gray_to_binary] width < 1"]
    else if width b = 1
    then b
    else (
      let ue x = uresize x ~width:(width b) in
      let rec f b mask =
        let b = b ^: ue mask in
        if width mask = 1 then b else f b (msbs mask)
      in
      f b (msbs b))
  ;;

  let gray_increment g ~by = binary_to_gray (gray_to_binary g +:. by)

  let[@cold] raise_of_decimal_string_empty_string () =
    raise_s
      [%message.omit_nil
        "[of_decimal_string] got empty string"
          ~loc:(Caller_id.get () : Caller_id.t option)]
  ;;

  (* complex constant generators *)
  let of_decimal_string ~width v =
    if String.is_empty v
    then raise_of_decimal_string_empty_string ()
    else of_bigint ~width (Bigint.of_string v)
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
    | 'o' -> of_octal ~signedness:Unsigned ~width:len sval
    | 'O' -> of_octal ~signedness:Signed ~width:len sval
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

  let rec random ~width =
    if width <= 16
    then of_int_trunc ~width (Random.int (1 lsl width))
    else of_int_trunc ~width:16 (Random.int (1 lsl 16)) @: random ~width:(width - 16)
  ;;

  let to_bool c =
    if width c <> 1
    then raise_s [%message "Cannot convert a multi-bit value to a bool" (c : t)];
    to_int_trunc c <> 0
  ;;

  let rec to_signed ~num_bits ~to_int_type_trunc t =
    if width t > num_bits
    then (
      let top = drop_bottom t ~width:(num_bits - 1) in
      if to_bool (top ==+. -1) || to_bool (top ==:. 0)
      then to_signed ~num_bits ~to_int_type_trunc (sresize t ~width:num_bits)
      else raise_s [%message "Failed to convert value to signed integer type"])
    else to_int_type_trunc (sresize t ~width:num_bits)
  ;;

  let rec to_unsigned ~num_bits ~to_int_type_trunc t =
    if width t > num_bits
    then (
      let top = drop_bottom t ~width:num_bits in
      if to_bool (top ==:. 0)
      then to_unsigned ~num_bits ~to_int_type_trunc (uresize t ~width:num_bits)
      else raise_s [%message "Failed to convert value to unsigned integer type"])
    else to_int_type_trunc t
  ;;

  let to_signed_int = to_signed ~num_bits:Int.num_bits ~to_int_type_trunc:to_int_trunc

  let to_unsigned_int =
    to_unsigned ~num_bits:(Int.num_bits - 1) ~to_int_type_trunc:to_int_trunc
  ;;

  let to_int32_trunc c = to_constant c |> Constant.to_int64 |> Int64.to_int32_trunc
  let to_int32 = to_int32_trunc

  let to_signed_int32 =
    to_signed ~num_bits:Int32.(num_bits |> to_int_trunc) ~to_int_type_trunc:to_int32_trunc
  ;;

  let to_unsigned_int32 =
    to_unsigned
      ~num_bits:(Int32.(num_bits |> to_int_trunc) - 1)
      ~to_int_type_trunc:to_int32_trunc
  ;;

  let to_int64_trunc c = to_constant c |> Constant.to_int64
  let to_int64 = to_int64_trunc

  let to_signed_int64 =
    to_signed ~num_bits:Int64.(num_bits |> to_int_trunc) ~to_int_type_trunc:to_int64_trunc
  ;;

  let to_unsigned_int64 =
    to_unsigned
      ~num_bits:(Int64.(num_bits |> to_int_trunc) - 1)
      ~to_int_type_trunc:to_int64_trunc
  ;;

  let to_char x =
    let actual_width = width x in
    if actual_width <> 8
    then raise_s [%message "[to_char] signal must be 8 bits wide" (actual_width : int)];
    Char.of_int_exn (to_int_trunc x)
  ;;

  let to_ascii_string_lsb x =
    let actual_width = width x in
    if actual_width % 8 <> 0
    then
      raise_s
        [%message
          "[to_ascii_string] signal must be a multiple of 8 bits wide"
            (actual_width : int)];
    x |> split_lsb ~exact:true ~part_width:8 |> List.map ~f:to_char |> String.of_char_list
  ;;

  let to_ascii_string_msb x = to_ascii_string_lsb x |> String.rev

  module With_zero_width = struct
    type non_zero_width = t [@@deriving sexp_of]
    type t = non_zero_width option [@@deriving sexp_of]

    let of_non_zero_width t =
      if is_empty t
      then raise_s [%message "[With_zero_width.of_non_zero_width] empty is not allowed"];
      Some t
    ;;

    let to_non_zero_width ?default t =
      match default, t with
      | None, None ->
        raise_s [%message "[With_zero_width.to_non_zero_width] cannot convert 0 width"]
      | Some default, None -> default
      | (None | Some _), Some t -> t
    ;;

    let zero_width = None

    let[@cold] raise_const_width_less_than_zero m requested_width =
      raise_s
        [%message
          (m ^ " requires width greater than or equal to 0") (requested_width : int)]
    ;;

    let zero = function
      | 0 -> None
      | x when x < 0 -> raise_const_width_less_than_zero "[zero]" x
      | x -> Some (zero x)
    ;;

    let one = function
      | 0 -> None
      | x when x < 0 -> raise_const_width_less_than_zero "[one]" x
      | x -> Some (one x)
    ;;

    let ones = function
      | 0 -> None
      | x when x < 0 -> raise_const_width_less_than_zero "[ones]" x
      | x -> Some (ones x)
    ;;

    let concat_msb t =
      match List.filter_opt t with
      | [] -> None
      | l -> Some (concat_msb l)
    ;;

    let concat_lsb t =
      match List.filter_opt t with
      | [] -> None
      | l -> Some (concat_lsb l)
    ;;

    let select t ~high:hi ~low:lo =
      match t with
      | None ->
        raise_s
          [%message
            "Cannot select a non-zero width into zero width signal" (hi : int) (lo : int)]
      | Some t ->
        if hi - lo = -1
        then
          (* zero width result. Ensure the indices are within bounds. We allow selection 0
             bits below or above the given signal. So one of lo or hi should be within the
             signal. *)
          if lo < 0 || lo > width t
          then
            raise_s
              [%message
                "[With_zero_width.select] indices are out of bound" (hi : int) (lo : int)]
          else None
        else Some (select t ~high:hi ~low:lo)
    ;;

    let lsbs = function
      | None -> raise_s [%message "cannot take lsbs of zero width signal"]
      | Some t when width t = 1 -> None
      | Some t -> Some (lsbs t)
    ;;

    let msbs = function
      | None -> raise_s [%message "cannot take msbs of zero width signal"]
      | Some t when width t = 1 -> None
      | Some t -> Some (msbs t)
    ;;

    let drop_bottom t ~width:n =
      match t, n with
      | None, 0 -> None
      | None, _ ->
        raise_s
          [%message
            "Cannot [drop_bottom] non-zero bits from a zero width signal" (n : int)]
      | Some t, n when width t = n -> None
      | Some t, _ -> Some (drop_bottom t ~width:n)
    ;;

    let drop_top t ~width:n =
      match t, n with
      | None, 0 -> None
      | None, _ ->
        raise_s
          [%message "Cannot [drop_top] non-zero bits from a zero width signal" (n : int)]
      | Some t, n when width t = n -> None
      | Some t, _ -> Some (drop_top t ~width:n)
    ;;

    let sel_bottom t ~width:n =
      match t, n with
      | _, 0 -> None
      | None, _ ->
        raise_s
          [%message
            "Cannot [sel_bottom] non-zero bits from a zero width signal" (n : int)]
      | Some t, _ -> Some (sel_bottom t ~width:n)
    ;;

    let sel_top t ~width:n =
      match t, n with
      | _, 0 -> None
      | None, _ ->
        raise_s
          [%message "Cannot [sel_top] non-zero bits from a zero width signal" (n : int)]
      | Some t, _ -> Some (sel_top t ~width:n)
    ;;

    let repeat t ~count:n =
      match t, n with
      | None, _ -> None
      | _, 0 -> None
      | Some t, n -> Some (repeat t ~count:n)
    ;;

    let raise_too_many_mux_inputs () =
      raise_s
        [%message
          "[With_zero_width.mux] select is 0 width but there is more than 1 input"]
    ;;

    let mux sel l =
      match sel with
      | None ->
        (match l with
         | [] -> raise_mux_no_inputs ()
         | [ h ] -> h
         | _ -> raise_too_many_mux_inputs ())
      | Some sel -> mux sel l
    ;;
  end

  module type Typed_math = Typed_math with type t := t

  let any_bit_set t = if width t = 1 then t else t <>:. 0
  let all_bits_set t = if width t = 1 then t else t ==+. -1
  let no_bits_set t = if width t = 1 then ~:t else t ==:. 0
  let incr ?(by = 1) x = x +:. by
  let decr ?(by = 1) x = x -:. by

  let check_invalid_truncation_width ~output_width ~input_width =
    if output_width <= 0
    then
      raise_s
        [%message "[Typed_math.truncate] output width must be >= 0" (output_width : int)];
    if output_width > input_width
    then
      raise_s
        [%message
          "[Typed_math.truncate] Output width must be less than or equal to input width"
            (output_width : int)
            (input_width : int)]
  ;;

  (* General arithmetic on unsigned signals. Operands and results are resized to fit a
     appropriate. *)
  module Unsigned = struct
    let resize s i = uresize s ~width:i

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

    let truncate v ~width:output_width =
      let input_width = width v in
      check_invalid_truncation_width ~output_width ~input_width;
      if input_width = output_width
      then { valid = vdd; value = v }
      else (
        let top, bottom = split_in_half_lsb ~lsbs:output_width v in
        let valid = ~:(any_bit_set top) in
        { valid; value = bottom })
    ;;
  end

  module Signed = struct
    let resize s i = sresize s ~width:i

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

    let truncate v ~width:output_width =
      let input_width = width v in
      check_invalid_truncation_width ~output_width ~input_width;
      if input_width = output_width
      then { valid = vdd; value = v }
      else (
        let top, bottom = split_in_half_lsb ~lsbs:output_width v in
        let valid = top ==: repeat (msb bottom) ~count:(width top) in
        { valid; value = bottom })
    ;;
  end
end

module Expert = struct
  module Gen_cases_from_mux = Gen_cases_from_mux
end
