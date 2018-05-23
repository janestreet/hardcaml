open! Import

include Comb_intf

module Make_primitives (Gates : Gates) = struct
  include Gates

  (* utils *)
  let vdd = const "1"
  let gnd = const "0"
  let bits_lsb x =
    let w = width x in
    Array.to_list (Array.init w ~f:(fun i -> select x i i))
  let reduce_bits def op a = List.fold (bits_lsb a) ~init:def ~f:op
  let repeat s n =
    if n <= 0
    then empty
    else concat (Array.to_list (Array.init n ~f:(fun _ -> s)))
  let concat_e l =
    let x = List.filter l ~f:(fun t -> not (is_empty t)) in
    if List.is_empty x
    then empty
    else concat x

  let ripple_carry_adder cin a b =
    let fa cin a b =
      let sum = (a ^: b) ^: cin in
      let carry = (a &: b) |: (b &: cin) |: (cin &: a) in
      sum, carry
    in
    let a = bits_lsb a in
    let b = bits_lsb b in
    let sum, _ =
      List.fold2_exn a b ~init:([], cin) ~f:(fun (sum_in, carry_in) a b ->
        let sum, carry_out = fa carry_in a b in
        sum :: sum_in, carry_out)
    in
    concat sum

  (** addition *)
  let (+:) a b = ripple_carry_adder gnd a b

  (** subtraction *)
  let (-:) a b = ripple_carry_adder vdd a (~: b)

  (** unsigned multiplication *)
  let ( *: ) a b =
    let _, r =
      List.fold
        (bits_lsb b)
        ~init:(0, (repeat gnd (width a)))
        ~f:(fun (i, acc) b ->
          let acc = concat_e [ gnd; acc ] in
          let a = concat_e [ gnd; a; repeat gnd i ] in
          i+1, (+:) acc ((&:) a (repeat b (width a))))
    in
    r

  (** signed multiplication *)
  let ( *+ ) a b =
    let last = (width b) - 1 in
    let msb x = select x (width x - 1) (width x -1 ) in
    let _, r =
      List.fold
        (bits_lsb b)
        ~init:(0, (repeat gnd (width a)))
        ~f:(fun (i, acc) b ->
          let acc = concat_e [ msb acc; acc ] in
          let a = concat_e [ msb a; a; repeat gnd i ] in
          i+1, (if i = last then (-:) else (+:)) acc ((&:) a (repeat b (width a))))
    in
    r

  (** equality *)
  let (==:) a b =
    let eq = (~: (a &: (~: b))) &: (~: ((~: a) &: b)) in
    reduce_bits vdd (&:) eq

  (** less than *)
  let (<:) a b =
    let w = width a in
    let a, b = concat [ gnd; a ], concat [ gnd; b ] in
    let d = a -: b in
    select d w w

  (** multiplexer *)
  let mux s d =
    let mux2 sel a b =
      assert (width sel = 1);
      let s = repeat sel (width a) in
      (s &: a) |: ((~: s) &: b)
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
end

module Make (Bits : Primitives) = struct

  open Utils

  type t = Bits.t

  let equal = Bits.equal

  let empty = Bits.empty

  let is_empty = Bits.is_empty

  let (--) a b = Bits.(--) a b

  let width = Bits.width

  (* constant generation *)
  let constb b =
    String.iter b ~f:(function
      | '0' | '1' -> ()
      | _ -> raise_s [%message "[constb] got invalid binary constant" ~_:(b : string)]);
    Bits.const b
  let consti l v = constb (bstr_of_int l v)
  let consti32 l v = constb (bstr_of_int32 l v)
  let consti64 l v = constb (bstr_of_int64 l v)
  let consthu l v = constb (bstr_of_hstr Unsigned l v)
  let consths l v = constb (bstr_of_hstr Signed l v)
  let constibl b = constb (Utils.bstr_of_intbitslist b)

  let concat s =
    List.iter s ~f:(fun x ->
      if is_empty x
      then raise_s [%message "[concat] got [empty] input" ~_:(s : Bits.t list)]);
    if List.is_empty s then raise_s [%message "[concat] got empty list"];
    Bits.concat s
  let (@:) a b = concat [ a; b ]
  let concat_e s = Bits.concat (List.filter s ~f:(fun b -> not (Bits.is_empty b)))

  let vdd = constb "1" -- "vdd"
  let gnd = constb "0" -- "gnd"

  let is_vdd t = equal t vdd
  let is_gnd t = equal t gnd

  let zero w = if w = 0 then empty else constb (String.init w ~f:(fun _ -> '0'))
  let ones w = if w = 0 then empty else constb (String.init w ~f:(fun _ -> '1'))
  let one w =
    match w with
    | 0 -> empty
    | 1 -> vdd
    | _ -> (zero (w-1)) @: vdd

  let select a hi lo =
    if hi < lo
    then raise_s [%message "[select] got [hi < lo]" (hi : int) (lo : int)];
    if hi >= width a || lo >= width a || hi < 0 || lo < 0
    then
      raise_s [%message "[select] indices are out of bounds"
                          ~input_width:(width a : int)
                          (hi : int)
                          (lo : int)];
    if lo = 0 && hi = (width a-1)
    then a
    else Bits.select a hi lo

  let select_e a hi lo =
    try select a hi lo
    with _ -> Bits.empty

  let msb  a   = select a (width a - 1) (width a - 1)
  let lsbs a   = select a (width a - 2) 0
  let lsb  a   = select a 0             0
  let msbs a   = select a (width a - 1) 1
  let bit  s n = select s n             n

  let drop_bottom x n = select x (width x - 1)     n
  let drop_top    x n = select x (width x - 1 - n) 0
  let sel_bottom  x n = select x (n-1)             0
  let sel_top     x n = select x (width x - 1)     (width x - n)

  let insert ~t ~f n =
    let wt, wf = width t, width f in
    if n < 0
    then raise_s [%message "[insert] below bit 0" ~_:(n : int)]
    else if wt < (wf + n)
    then raise_s [%message "[insert] above msb of target"
                             ~width_from:(wf : int)
                             ~width_target:(wt : int)
                             ~at_pos:(n : int)
                             ~highest_inserted_bit:(wf + n : int)]
    else if wt = wf && n = 0
    then f
    else if n=0
    then select t (wt - 1) wf @: f
    else if wt = (wf + n)
    then f @: select t (wt - wf - 1) 0
    else select t (wt - 1) (wf + n) @: f @: select t (n-1) 0

  let sel x (h, l) = select x h l

  (* error checking *)
  let assert_widths_same function_ inputs =
    match inputs with
    | [] ->
      raise_s [%message
        "" ~_:(String.concat [ "["; function_; "] got empty list" ] : string)]
    | t :: ts ->
      let w = width t in
      List.iter ts ~f:(fun t ->
        if width t <> w
        then raise_s [%message
               ""
                 ~_:(String.concat ["["; function_; "] got inputs of different widths"]
                     : string)
                 ~_:(inputs : Bits.t list)])

  let assert_width_one t msg =
    if not (width t = 1)
    then raise_s [%message "" ~_:(msg : string)]

  let op_int_right op a b = op a (consti (width a) b)

  (* mux *)
  let mux sel l =
    let els = List.length l in
    let max_els = 1 lsl (width sel) in
    assert_widths_same "mux" l;
    if els > max_els then raise_s [%message "[mux] got too many inputs"
                                              ~inputs_provided:(els:int)
                                              ~maximum_expected:(max_els : int)];
    if els < 2 then raise_s [%message "[mux] got fewer than 2 inputs"
                                        ~inputs_provided:(els:int)];
    Bits.mux sel l

  let mux2 sel a b =
    assert_width_one sel "[mux] got select argument that is not one bit";
    mux sel [ b; a ]

  let mux_init sel n f = mux sel (Array.to_list (Array.init n ~f))

  let cases sel default l =
    let max = 1 + List.fold l ~init:0 ~f:(fun acc (i, _) -> max i acc) in
    let a = Array.create ~len:max default in
    List.iter l ~f:(fun (i, x) -> a.(i) <- x);
    if 1 lsl (width sel) = max
    then mux sel (Array.to_list a)
    else mux sel (Array.to_list a @ [ default ])

  let matches
        ?(resize=(fun s _ -> s))
        ?default
        sel
        cases =
    (* sort cases *)
    let cases = List.sort cases ~compare:(fun (a, _) (b, _) -> compare a b) in
    (* check cases are unique *)
    let check_unique_cases cases =
      let add s (i, _) = Set.add s i in
      let s = List.fold cases ~init:(Set.empty (module Int)) ~f:add in
      if Set.length s <> List.length cases
      then raise_s [%message "[matches] cases must be unique"]
    in
    check_unique_cases cases;
    (* resize cases and default *)
    let w = List.fold cases ~init:0 ~f:(fun w (_, d) -> max w (width d)) in
    let w, default =
      match default with
      | None -> w, zero w
      | Some d ->
        let w = max w (width d) in
        w, resize d w
    in
    let cases = List.map cases ~f:(fun (c, d) -> c, resize d w) in
    (* filter cases that cannot be indexed by sel *)
    let out_of_range_cases sgn sel cases =
      let w = width sel in
      let min, max, msk =
        if sgn
        then
          let w = 1 lsl (w-1) in
          -w, w-1, (w lsl 1)-1
        else
          let w = 1 lsl w in
          0, w-1, w-1
      in
      List.map ~f:(fun (x, y) -> x land msk, y) @@
      List.filter cases ~f:(fun (x, _) -> x >= min && x <= max)
    in
    let sgn = (fst @@ List.hd_exn cases) < 0 in
    let cases = out_of_range_cases sgn sel cases in
    (* generate mux tree *)
    let rec f sel c def =
      match width sel, c with
      | 0, [] -> def
      | 0, ((0, d) :: _) -> d
      | 0, _ -> def
      | _, [] -> def
      | _ ->
        let e, o = List.partition_tf c ~f:(fun (i, _) -> (i land 1) = 0) in
        let e, o =
          let shift (a, b) = a lsr 1, b in
          List.map e ~f:shift, List.map o ~f:shift
        in
        let sel2 = lsb sel in
        let sel = try msbs sel with _ -> empty in
        mux2 sel2 (f sel o def) (f sel e def)
    in
    f sel cases default

  (* logical *)
  let (&:) a b =
    assert_widths_same "&:" [ a; b ];
    Bits.(&:) a b

  let (|:) a b =
    assert_widths_same "|:" [ a; b ];
    Bits.(|:) a b

  let (^:) a b =
    assert_widths_same "^:" [ a; b ];
    Bits.(^:) a b

  let (~:) = Bits.(~:)

  let ( &:. ) a b = op_int_right (&:) a b
  let ( |:. ) a b = op_int_right (|:) a b
  let ( ^:. ) a b = op_int_right (^:) a b

  (* arithmetic *)
  let (+:) a b =
    assert_widths_same "+:" [ a; b ];
    Bits.(+:) a b

  let (-:) a b =
    assert_widths_same "-:" [ a; b ];
    Bits.(-:) a b

  let (+:.) a b = op_int_right (+:) a b
  let (-:.) a b = op_int_right (-:) a b

  let negate a = (zero (width a)) -: a

  let ( *: ) = Bits.( *: )

  let ( *+ ) = Bits.( *+ )

  (* comparison *)
  let (==:) a b =
    assert_widths_same "==:" [ a; b ];
    Bits.(==:) a b

  let (<>:) a b =
    assert_widths_same "<>:" [ a; b ];
    ~: (a ==: b)

  let (<:) a b =
    assert_widths_same "<:" [ a; b ];
    Bits.(<:) a b

  let lt = (<:)

  let (>:) a b = b <: a
  let (<=:) a b = ~: (a >: b)
  let (>=:) a b = ~: (a <: b)

  let (<+) a b =
    let f a = (~: (msb a)) @: (lsbs a) in
    if width a = 1
    then a &: (~: b)
    else (f a) <: (f b)

  let (>+) a b =
    let f a = (~: (msb a)) @: (lsbs a) in
    if width a = 1
    then b &: (~: a)
    else (f a) >: (f b)

  let (<=+) a b =
    let f a = (~: (msb a)) @: (lsbs a) in
    if width a = 1
    then ~: (a >+ b)
    else (f a) <=: (f b)

  let (>=+) a b =
    let f a = (~: (msb a)) @: (lsbs a) in
    if width a = 1
    then ~: (a <+ b)
    else (f a) >=: (f b)

  let (==:.) a b = op_int_right (==:) a b
  let (<>:.) a b = op_int_right (<>:) a b
  let (<:.) a b = op_int_right (<:) a b
  let (>:.) a b = op_int_right (>:) a b
  let (<=:.) a b = op_int_right (<=:) a b
  let (>=:.) a b = op_int_right (>=:) a b
  let (<+.) a b = op_int_right (<+) a b
  let (>+.) a b = op_int_right (>+) a b
  let (<=+.) a b = op_int_right (<=+) a b
  let (>=+.) a b = op_int_right (>=+) a b

  let to_string a = Bits.to_string a
  let to_int a = Bits.to_int a
  let to_bstr a = Bits.to_bstr a
  let sexp_of_t = Bits.sexp_of_t

  let bits s =
    let a = Array.init (width s) ~f:(fun i -> bit s i) in
    List.rev (Array.to_list a)

  let to_array b = Array.of_list (List.rev (bits b))
  let of_array l = concat (List.rev (Array.to_list l))

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
        then build (pwr*2) (rep_s @: rep_s) (concat_e [rep_s; res_s]) (n-pwr)
        else build (pwr*2) (rep_s @: rep_s) res_s n
      in
      build 1 s empty n

  let split_in_half s =
    let w = width s in
    select s (w-1) (w/2), select s ((w/2)-1) 0

  let split ?(exact = true) ~part_width t_in =
    if is_empty t_in
    then raise_s [%message "[split] got [empty] input"];
    if part_width <= 0
    then raise_s [%message "[split] got [part_width <= 0]" (part_width : int)];
    let rec split t =
      if width t < part_width && exact
      then raise_s [%message
             "[split ~exact:true] unable to split exactly"
               ~input_width:(width t_in : int)
               (part_width : int)
               ~width_of_last_part:(width t : int)];
      if width t <= part_width
      then [t]
      else sel_bottom t part_width :: split (drop_bottom t part_width)
    in
    split t_in

  let sll a shift =
    if shift < 0
    then raise_s [%message "[sll] got negative shift" ~_:(shift : int)];
    if shift = 0
    then a
    else if shift >= (width a)
    then zero (width a)
    else concat [ (select a ((width a) - 1 - shift) 0); (zero shift) ]

  let srl a shift =
    if shift < 0
    then raise_s [%message "[srl] got positive shift" ~_:(shift : int)];
    if shift = 0
    then a
    else if shift >= (width a)
    then zero (width a)
    else concat [ (zero shift); (select a ((width a) - 1) shift) ]

  let sra a shift =
    if shift < 0
    then raise_s [%message "[sra] got negative shift" ~_:(shift : int)];
    if shift = 0
    then a
    else if shift >= (width a)
    then repeat (msb a) (width a)
    else concat [ (repeat (msb a) shift); (select a ((width a) - 1) shift) ]

  let log_shift op a b =
    let rec sft a n =
      if n = width b
      then a
      else
        let s = mux2 (bit b n) (op a (1 lsl n)) a in
        sft s (n+1)
    in
    sft a 0

  let uresize s w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat[ (repeat gnd (w-x)); s ]
    else select s (w-1) 0

  let sresize s w =
    let x = width s in
    if w = x
    then s
    else if w > x
    then concat[ (repeat (msb s) (w-x)); s ]
    else select s (w-1) 0

  let ue s = uresize s ((width s)+1)
  let se s = sresize s ((width s)+1)

  let resize_list ~resize l =
    let w = List.fold l ~init:0 ~f:(fun w e -> max (width e) w) in
    List.map l ~f:(fun e -> resize e w)

  let resize_op2 ~resize f a b =
    let w = max (width a) (width b) in
    let a, b = resize a w, resize b w in
    f a b

  let to_sint a = to_int (sresize a Int.num_bits)

  let reduce op s =
    match List.length s with
    | 0 -> raise_s [%message "[reduce] got empty list"]
    | _ -> List.reduce_exn s ~f:(fun acc x -> op acc x)

  let (||:) a b = (reduce (|:) (bits a)) |: (reduce (|:) (bits b))
  let (&&:) a b = (reduce (|:) (bits a)) &: (reduce (|:) (bits b))


  let pmux list last =
    List.fold (List.rev list) ~init:last ~f:(fun ac (c, d) -> mux2 c d ac)

  let pmuxl list = snd (reduce (fun (s0, d0) (s1, d1) -> (s0 |: s1), mux2 s0 d0 d1) list)

  let pmux1h list =
    reduce (|:) (List.map list ~f:(fun (s, d) -> sresize s (width d) &: d))

  let reverse a = concat (List.rev (bits a))

  let mod_counter max c =
    let w = width c in
    let lmax = 1 lsl w in
    if lmax = (max + 1)
    then c +: (one w)
    else mux2 (c ==: (consti w max)) (zero w) (c +: (one w))

  let rec tree arity ops l =
    if arity <= 1 then raise_s [%message "[tree] got [arity <= 1]"];
    let split l n =
      let (lh, ll, _) =
        List.fold l ~init:([], [], 0) ~f:(fun (l0, l1, m) e ->
          if m < n then ((e :: l0), l1, m+1) else (l0, e :: l1, m+1))
      in
      (List.rev lh, List.rev ll)
    in
    let rec t0 l =
      let l0, l1 = split l arity in
      if List.is_empty l1
      then [ ops l0 ]
      else (ops l0) :: (t0 l1)
    in
    match l with
    | [] -> raise_s [%message "[tree] got empty list"]
    | [ a ] -> a
    | _ -> tree arity ops (t0 l)

  let tree_or_reduce_binary_operator ?(branching_factor = 2) ~f data =
    if List.is_empty data
    then raise_s [%message "[tree_or_reduce_binary_operator] got empty list"];
    if branching_factor < 1
    then raise_s [%message
           "[tree_or_reduce_binary_operator] got [branching_factor < 1]"
             (branching_factor : int) ];
    if branching_factor = 1
    then reduce f data
    else tree branching_factor (reduce f) data

  let priority_select ?branching_factor ts =
    tree_or_reduce_binary_operator ts ?branching_factor
      ~f:(fun (a : t With_valid.t) b ->
        { With_valid.
          valid = a.valid |: b.valid
        ; value = mux2 a.valid a.value b.value })

  let priority_select_with_default ?branching_factor data ~default =
    let d = priority_select data ?branching_factor in
    mux2 d.valid d.value default

  let onehot_select ?branching_factor (ts : t With_valid.t list) =
    List.map ts ~f:(fun d -> sresize d.valid (width d.value) &: d.value)
    |> tree_or_reduce_binary_operator ?branching_factor ~f:(|:)

  let popcount ?branching_factor t =
    let width = width t in
    if width = 0 then raise_s [%message "[popcount] of [empty]"];
    let result_width = Int.ceil_log2 (width + 1) in
    tree_or_reduce_binary_operator ?branching_factor ~f:(+:)
      (List.map (bits t) ~f:(fun d -> uresize d result_width))

  let leading_zeros_of_bits_list ?branching_factor d =
    let result_width = Utils.nbits (List.length d) in
    List.mapi d ~f:(fun i valid -> { With_valid. valid; value = consti result_width i })
    |> priority_select_with_default ?branching_factor
         ~default:(consti result_width (List.length d))

  let leading_ones ?branching_factor t =
    if width t = 0 then raise_s [%message "[leading_ones] of [empty]"];
    leading_zeros_of_bits_list (bits (~: t)) ?branching_factor

  let trailing_ones ?branching_factor t =
    if width t = 0 then raise_s [%message "[trailing_ones] of [empty]"];
    leading_zeros_of_bits_list (bits (~: t) |> List.rev) ?branching_factor

  let leading_zeros ?branching_factor t =
    if width t = 0 then raise_s [%message "[leading_zeros] of [empty]"];
    leading_zeros_of_bits_list (bits t) ?branching_factor

  let trailing_zeros ?branching_factor t =
    if width t = 0 then raise_s [%message "[trailing_zeros] of [empty]"];
    leading_zeros_of_bits_list (bits t |> List.rev) ?branching_factor

  let is_pow2 ?branching_factor t =
    if width t = 0 then raise_s [%message "[is_pow2] of [empty]"];
    if width t = 1
    then t
    else popcount ?branching_factor t ==:. 1

  let floor_log2 ?branching_factor t : t With_valid.t =
    let width = width t in
    if width = 0 then raise_s [%message "[floor_log2] of [empty]"];
    let leading_zeros = leading_zeros t ?branching_factor in
    let result_width = max 1 (Int.ceil_log2 width) in
    { valid = t <>:. 0
    ; value = consti result_width (width - 1) -: uresize leading_zeros result_width}

  let ceil_log2 ?branching_factor t =
    if width t = 0 then raise_s [%message "[ceil_log2] of [empty]"];
    let is_pow2 = is_pow2 ?branching_factor t in
    let floor_log2 = floor_log2 ?branching_factor t in
    let value = ue floor_log2.value in
    { floor_log2 with value = mux2 is_pow2 value (value +:. 1) }

  let binary_to_onehot s =
    let rec build = function
      | [] -> []
      | a :: [] -> [ a; ~:a ]
      | a :: b ->
        let b1 = build b in
        let l2 = List.map b1 ~f:((&:) (~: a)) in
        let b2 = build b in
        let l1 = List.map b2 ~f:((&:) a) in
        l1 @ l2
    in
    concat (build (bits s))

  let onehot_to_binary x =
    let n = Utils.nbits (width x - 1) in
    let x = List.rev (bits x) in
    let rec f i =
      if i=n
      then []
      else
        let rec g j = function
          | [] -> []
          | h :: t ->
            let c = j land (1 lsl i) <> 0 in
            if c
            then h :: g (j+1) t
            else g (j+1) t
        in
        let g = g 0 x in
        match g with
        | [] -> gnd :: f (i+1)
        | _ -> reduce (|:) g :: f (i+1)
    in
    concat List.(rev (f 0))

  let binary_to_gray b = b ^: (srl b 1)

  let gray_to_binary b =
    let ue x = uresize x (width b) in
    let rec f b mask =
      let b = b ^: (ue mask) in
      if width mask = 1
      then b
      else f b (msbs mask)
    in
    f b (msbs b)

  (* complex constant generators *)
  let rec constd bits v =
    let l = String.length v in
    let decimal v =
      match v with
      | '0' -> consti 4 0
      | '1' -> consti 4 1
      | '2' -> consti 4 2
      | '3' -> consti 4 3
      | '4' -> consti 4 4
      | '5' -> consti 4 5
      | '6' -> consti 4 6
      | '7' -> consti 4 7
      | '8' -> consti 4 8
      | '9' -> consti 4 9
      | _ -> raise_s [%message "[constd] got invalid decimal char" ~_:(v : char)]
    in
    let (+:) a b =
      let w = max (width a) (width b) + 1 in
      let a, b = uresize a w, uresize b w in
      a +: b
    in
    let ten = consti 4 10 in
    if l=0
    then raise_s [%message "[constd] got empty string"]
    else
    if Char.equal v.[0] '-'
    then zero bits -: (constd bits (String.sub v ~pos:1 ~len:(l-1)))
    else
      (* convert *)
      let rec sum i mulfac prod =
        if i<0
        then prod
        else
          sum (i-1) (mulfac *: ten)
            (prod +: (decimal v.[i] *: mulfac))
      in
      uresize
        (sum (l-1) (consti 1 1) (consti 1 0))
        bits

  let constv s =
    let slen, sval =
      let rec split2 n c s t =
        if Char.equal t.[n] c
        then s, String.sub t ~pos:(n + 1) ~len:(String.length t - n - 1)
        else split2 (n+1) c (s ^ (String.make 1 t.[n])) t
      in
      let s0, s1 =
        try split2 0 '\'' "" s
        with _ -> raise_s [%message "[constv] missing [']" ~_:(s : string) ]
      in
      if String.length s0 = 0
      then raise_s [%message "[constv] missing bit count" ~_:(s : string)];
      if String.length s1 < 2
      then raise_s [%message "[constv] value shorter than 2 characters" ~_:(s : string)];
      s0, s1
    in
    let len = Int.of_string slen in
    let ctrl = sval.[0] in
    let sval = String.sub sval ~pos:1 ~len:(String.length sval - 1) in
    match ctrl with
    | 'd' -> constd len sval
    | 'x' | 'h' -> consthu len sval
    | 'X' | 'H' -> consths len sval
    | 'b' ->
      let slen = String.length sval in
      if slen < len
      then constb ((String.make (len-slen) '0' ) ^ sval)
      else if slen > len
      then constb (String.sub sval ~pos:(slen-len) ~len)
      else constb sval
    | 'B' ->
      let slen = String.length sval in
      if slen < len
      then constb ((String.make (len-slen) sval.[0]) ^ sval)
      else if slen > len
      then constb (String.sub sval ~pos:(slen-len) ~len)
      else constb sval
    | _ -> raise_s [%message "[constv] bad control character" ~const:(s : string)]

  let const b =
    let b = String.filter b ~f:(function '_' -> false | _ -> true) in
    try
      try constv b
      with _ -> constb b
    with _ ->
      raise_s [%message "[const] could not convert constant" ~const:(b : string)]

  let rec srand bits =
    if bits <= 16
    then consti bits (Random.int (1 lsl bits))
    else
      consti 16 (Random.int (1 lsl 16)) @: srand (bits-16)

  let to_int32' resize x =
    let x = resize x 32 in
    let a = Int32.of_int_exn (to_int (select x 15  0)) in
    let b = Int32.of_int_exn (to_int (select x 31 16)) in
    fst (
      List.fold [a; b] ~init:(0l, 0) ~f:(fun (acc, n) a ->
        Int32.(logor (shift_left a n) acc), n+16))

  let to_int64' resize x =
    let x = resize x 64 in
    let a = Int64.of_int (to_int (select x 15  0)) in
    let b = Int64.of_int (to_int (select x 31 16)) in
    let c = Int64.of_int (to_int (select x 47 32)) in
    let d = Int64.of_int (to_int (select x 63 48)) in
    fst (
      List.fold [ a; b; c; d ] ~init:(0L, 0) ~f:(fun (acc, n) a ->
        Int64.(logor (shift_left a n) acc), n+16))

  let to_int32 = to_int32' uresize
  let to_sint32 = to_int32' sresize
  let to_int64 = to_int64' uresize
  let to_sint64 = to_int64' sresize

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
    let re0 = re max
    let re1 = re (fun a b -> (max a b) + 1)

    let (+:) = re1 (+:)
    let (-:) = re1 (-:)
    let ( *: ) = ( *: )
    let (<:) = re0 (<:)
    let (>:) = re0 (>:)
    let (<=:) = re0 (<=:)
    let (>=:) = re0 (>=:)
    let (==:) = re0 (==:)
    let (<>:) = re0 (<>:)
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
    let re0 = re max
    let re1 = re (fun a b -> (max a b) + 1)

    let (+:) = re1 (+:)
    let (-:) = re1 (-:)
    let ( *: ) = ( *+ )
    let (<:) = re0 (<+)
    let (>:) = re0 (>+)
    let (<=:) = re0 (<=+)
    let (>=:) = re0 (>=+)
    let (==:) = re0 (==:)
    let (<>:) = re0 (<>:)
  end

  module Uop = Unsigned
  module Sop = Signed
end
