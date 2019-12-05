
open! Import

let ( mod ) = Caml.( mod )
let nbits = Int.num_bits - 1
let words bits = (bits + nbits - 1) / nbits
let word_index bit = bit / nbits
let m = -1 lsr 1 (* 62-bit mask *)

let mask_bits n = if n = 0 then m else m lsr (nbits - n)
let mask_bit n = 1 lsl n
let bo2 = nbits / 2
let mask_lo = mask_bits bo2
let mask_hi = mask_bits bo2 lsl bo2
let half_lo x = x land mask_lo
let half_hi x = (x land mask_hi) lsr bo2
let cbit = mask_bit bo2
let create_words bits = Array.create ~len:(words bits) 0

let words_of_int bits i =
  let b = create_words bits in
  b.(0) <- i land m;
  b
;;

let get_word a n = a.(n)
let set_word a n w = a.(n) <- w land m

module type Integer = sig
  include Int.S

  val nbits : int
end

let convert_abits_and_string (type t) (module X : Integer with type t = t) =
  let ( |. ) = X.( lor ) in
  let ( &. ) = X.( land ) in
  let ( <<. ) = X.shift_left in
  let one = X.of_int_exn 1 in
  let of_bstr b =
    let width = String.length b in
    let words = (width + X.nbits - 1) / X.nbits in
    let a = Array.create ~len:words X.zero in
    let rec build n =
      let word = n / X.nbits in
      let bit = n mod X.nbits in
      if Char.equal b.[width - n - 1] '1' then a.(word) <- a.(word) |. (one <<. bit);
      if n <> 0 then build (n - 1)
    in
    build (width - 1);
    a
  in
  let to_bstr ~width a =
    if width = 0
    then ""
    else (
      let b = Bytes.make width '0' in
      let rec build n =
        let word = n / X.nbits in
        let bit = n mod X.nbits in
        if not (X.equal (a.(word) &. (one <<. bit)) X.zero)
        then Bytes.set b (width - n - 1) '1';
        if n <> 0 then build (n - 1)
      in
      build (width - 1);
      Bytes.to_string b)
  in
  of_bstr, to_bstr
;;

module XInt = struct
  include Int

  let nbits = num_bits - 1
end

let abits_int_of_bstr, bstr_of_abits_int = convert_abits_and_string (module XInt)

module T = struct
  (* This only uses non-negative ints, which have the range [0 .. Int.max_value]. *)
  type t = int array * int [@@deriving compare]

  let equal = [%compare.equal: t]
  let empty = create_words 0, 0
  let width = snd
  let is_empty (_, width) = width = 0
  let to_string x = bstr_of_abits_int ~width:(width x) (fst x)

  let to_int (data, width) =
    (* Ensure we fill a complete OCaml integer. *)
    if width <= nbits then data.(0) else data.(0) lor (data.(1) lsl nbits)
  ;;

  let to_bstr x = bstr_of_abits_int ~width:(width x) (fst x)
  let sexp_of_t s = [%sexp (to_bstr s : string)]

  let mask s =
    let width = width s in
    let top_word = word_index (width - 1) in
    let s = fst s in
    let mask = mask_bits (width mod nbits) in
    set_word s top_word (get_word s top_word land mask);
    s, width
  ;;

  let of_constant c = Constant.to_binary_string c |> abits_int_of_bstr, Constant.width c
  let to_constant (b, width) = bstr_of_abits_int ~width b |> Constant.of_binary_string
  let ( -- ) a _ = a

  let vdd =
    let c = create_words 1 in
    set_word c 0 (mask_bit 0);
    c, 1
  ;;

  let gnd = create_words 1, 1

  let op2 op a b =
    let width = width a in
    let words = words width in
    let a = fst a in
    let b = fst b in
    let c = create_words width in
    for i = 0 to words - 1 do
      set_word c i (op (get_word a i) (get_word b i))
    done;
    mask (c, width)
  ;;

  let ( &: ) = op2 ( land )
  let ( |: ) = op2 ( lor )
  let ( ^: ) = op2 ( lxor )

  let ( ~: ) a =
    let width = width a in
    let words = words width in
    let a = fst a in
    let c = create_words width in
    for i = 0 to words - 1 do
      set_word c i (lnot (get_word a i))
    done;
    mask (c, width)
  ;;

  let ( +: ) a b =
    let carry = ref zero in
    let set_carry x = if not (x land cbit = 0) then carry := 1 else carry := 0 in
    op2
      (fun a b ->
         let a' = half_lo a in
         let b' = half_lo b in
         let c_lo = a' + b' + !carry in
         set_carry c_lo;
         let a' = half_hi a in
         let b' = half_hi b in
         let c_hi = a' + b' + !carry in
         set_carry c_hi;
         (c_hi lsl bo2) lor half_lo c_lo)
      a
      b
  ;;

  let ( -: ) a b =
    let borrow = ref zero in
    let set_borrow x = if not (x land cbit = 0) then borrow := 1 else borrow := 0 in
    op2
      (fun a b ->
         let a' = half_lo a in
         let b' = half_lo b in
         let c_lo = a' - b' - !borrow in
         set_borrow c_lo;
         let a' = half_hi a in
         let b' = half_hi b in
         let c_hi = a' - b' - !borrow in
         set_borrow c_hi;
         (c_hi lsl bo2) lor half_lo c_lo)
      a
      b
  ;;

  let iterback op a b =
    let words = words (width a) in
    let a = fst a in
    let b = fst b in
    for i = words - 1 downto 0 do
      op (get_word a i) (get_word b i)
    done
  ;;

  let ( ==: ) a b =
    let eq = ref true in
    iterback (fun a b -> if not (a = b) then eq := false) a b;
    if !eq then vdd else gnd
  ;;

  let ( <: ) a b =
    let cmp = ref 0 in
    let set_cmp a b =
      if !cmp = 0
      then
        if Int.compare a b > 0 then cmp := 1 else if Int.compare a b < 0 then cmp := -1
    in
    iterback
      (fun a b ->
         let a' = half_hi a in
         let b' = half_hi b in
         set_cmp a' b';
         let a' = half_lo a in
         let b' = half_lo b in
         set_cmp a' b')
      a
      b;
    if !cmp = -1 then vdd else gnd
  ;;

  let mux sel l =
    let len = List.length l in
    let idx = to_int sel in
    let idx = if idx >= len then len - 1 else idx in
    List.nth_exn l idx
  ;;

  (* XXX some of the harder operations *)

  let concat_msb l =
    let c_width = List.fold l ~init:0 ~f:(fun a b -> a + width b) in
    let c_words = words c_width in
    let c = create_words c_width in
    let cat a b =
      let a_width, a_words = width a, words (width a) in
      let b_width, b_words = width b, words (width b) in
      let a_bits = a_width mod nbits in
      let a, b = fst a, fst b in
      if a_bits = 0
      then
        for (* aligned *)
          i = 0 to b_words - 1 do
          set_word a (a_words + i) (get_word b i)
        done
      else (
        (* not aligned *)
        let merge x y = x lor (y lsl a_bits), y lsr (nbits - a_bits) in
        for i = 0 to b_words - 1 do
          let x, y = merge (get_word a (a_words - 1 + i)) (get_word b i) in
          set_word a (a_words - 1 + i) x;
          if a_words + i < c_words then set_word a (a_words + i) y
        done);
      c, a_width + b_width
    in
    let l = List.rev l in
    if List.is_empty l
    then empty
    else (
      let c = cat (c, 0) (List.hd_exn l) in
      List.fold (List.tl_exn l) ~init:c ~f:(fun c a -> cat c a))
  ;;

  let select s h l =
    let c_width = h - l + 1 in
    let c_words = words c_width in
    let s_bits = l mod nbits in
    let c = create_words c_width in
    let lo_word = word_index l in
    let hi_word = word_index h in
    let s = fst s in
    let merge i =
      let a = get_word s i in
      let b = if i >= hi_word then zero else get_word s (i + 1) in
      (a lsr s_bits) lor (b lsl (nbits - s_bits))
    in
    if s_bits = 0
    then
      for i = 0 to c_words - 1 do
        set_word c i (get_word s (lo_word + i))
      done
    else
      for i = 0 to c_words - 1 do
        set_word c i (merge (lo_word + i))
      done;
    mask (c, h - l + 1)
  ;;

  (* a few functions introduced a earlier for the multipliers *)
  let ( @: ) a b = concat_msb [ a; b ]
  let of_int bits n = words_of_int bits n, bits
  let zero n = of_int n 0

  let uresize x w =
    if width x = w
    then x
    else if width x > w
    then select x (w - 1) 0
    else zero (w - width x) @: x
  ;;

  let msb a =
    let n = width a - 1 in
    select a n n
  ;;

  let negate x = zero (width x) -: x
  let se a = msb a @: a

  let umul n a b =
    let wc = width a + width b in
    (* we need some API functions that dont exist yet... *)
    let ( +: ) a b =
      let w = max (width a) (width b) + 1 in
      let a, b = uresize a w, uresize b w in
      a +: b
    in
    let rec split s =
      if width s <= n
      then [ s ]
      else select s (n - 1) 0 :: split (select s (width s - 1) n)
    in
    let a, b = split a, split b in
    (* Create_words products and sum equal powers.

       Each partial product is implicitly multiplied by (2^n)^m where m depends on the
       position within a and b.  During the outer recursion 'prod_b' we multiply each part
       of b with all of a.

       This will start by generating m=0, 1, 2, ...

       On the 2nd recursion we use the next part of b and generate m=1, 2, 3...

       What we do is keep the paritial products form the previous step in prod_b,
       excluding the 1st product (which is fully generated), and sum it with the current
       products.  At the end we will have summed all products. For example, if a has 3
       parts, and b has 4 parts the recursion will be:

       m       prev
       [0;1;2] []
       [1;2;3] [1;2]
       [2;3;4] [2;3]
       [3;4;5] [3;4] *)
    let sums =
      let ( * ) a b = of_int (n * 2) (to_int a * to_int b) in
      let rec map2 f a b =
        match a, b with
        | a' :: a'', b' :: b'' -> f a' b' :: map2 f a'' b''
        | [], _ -> b
        | _, [] -> a
      in
      let rec prod_a a b =
        match a with
        | a' :: a'' -> (a' * b) :: prod_a a'' b
        | [] -> []
      in
      let rec prod_b a b prev =
        match b with
        | b' :: b'' ->
          let a' = prod_a a b' in
          let a' = map2 ( +: ) a' prev in
          (* add powers from previous step*)
          (* drop lowest power and recurse *)
          List.hd_exn a' :: prod_b a b'' (List.tl_exn a')
        | [] -> prev
      in
      prod_b a b []
    in
    (* add powers *)
    let scale s n = if n = 0 then s else s @: zero n in
    let c, _ =
      List.fold sums ~init:(gnd, 0) ~f:(fun (acc, scl) e -> acc +: scale e scl, scl + n)
    in
    let c = select c (wc - 1) 0 in
    c
  ;;

  let smul n a b =
    let to_bool x = to_int x <> 0 in
    let wa, wb = width a, width b in
    let na, nb = to_bool (msb a), to_bool (msb b) in
    let a = if na then negate (se a) else a in
    let b = if nb then negate (se b) else b in
    let c = umul n a b in
    let c = if Bool.equal na nb then c else negate c in
    select c (wa + wb - 1) 0
  ;;

  let ( *: ) = umul 15
  let ( *+ ) = smul 15
end

let compare = T.compare

include Comb.Make (T)
include Comparator.Make (T)

module Mutable = struct
  module T = struct
    type t = T.t [@@deriving compare]

    let equal = T.equal
    let empty = T.empty
    let is_empty = T.is_empty
    let width = T.width
    let data s = fst s
    let to_string = T.to_string
    let to_int = T.to_int
    let to_bstr = T.to_bstr

    let mask s =
      let width = width s in
      let top_word = word_index (width - 1) in
      let mask = mask_bits (width mod nbits) in
      set_word (data s) top_word (get_word (data s) top_word land mask)
    ;;

    let create w = create_words w, w

    let copy ~src ~dst =
      assert (width src = width dst);
      for i = 0 to words (width src) - 1 do
        set_word (data dst) i (get_word (data src) i)
      done
    ;;

    let copy_bits = copy

    let to_bits t =
      let result = create (width t) in
      copy ~src:t ~dst:result;
      result
    ;;

    let wire _ = empty
    let ( -- ) a _ = a
    let vdd = of_bit_string "1"
    let gnd = of_bit_string "0"

    let op2 op c a b =
      let width = width a in
      let words = words width in
      for i = 0 to words - 1 do
        set_word (data c) i (op (get_word (data a) i) (get_word (data b) i))
      done;
      mask c
    ;;

    let ( &: ) = op2 ( land )
    let ( |: ) = op2 ( lor )
    let ( ^: ) = op2 ( lxor )

    let ( ~: ) c a =
      let width = width a in
      let words = words width in
      let a = data a in
      for i = 0 to words - 1 do
        set_word (data c) i (lnot (get_word a i))
      done;
      mask c
    ;;

    let ( +: ) c a b =
      let carry = ref 0 in
      let set_carry x = if not (x land cbit = 0) then carry := 1 else carry := 0 in
      op2
        (fun a b ->
           let a' = half_lo a in
           let b' = half_lo b in
           let c_lo = a' + b' + !carry in
           set_carry c_lo;
           let a' = half_hi a in
           let b' = half_hi b in
           let c_hi = a' + b' + !carry in
           set_carry c_hi;
           (c_hi lsl bo2) lor half_lo c_lo)
        c
        a
        b
    ;;

    let ( -: ) c a b =
      let borrow = ref 0 in
      let set_borrow x = if not (x land cbit = 0) then borrow := 1 else borrow := 0 in
      op2
        (fun a b ->
           let a' = half_lo a in
           let b' = half_lo b in
           let c_lo = a' - b' - !borrow in
           set_borrow c_lo;
           let a' = half_hi a in
           let b' = half_hi b in
           let c_hi = a' - b' - !borrow in
           set_borrow c_hi;
           (c_hi lsl bo2) lor half_lo c_lo)
        c
        a
        b
    ;;

    let iterback op a b =
      let words = words (width a) in
      let a = data a in
      let b = data b in
      for i = words - 1 downto 0 do
        op (get_word a i) (get_word b i)
      done
    ;;

    let ( ==: ) c a b =
      let eq = ref true in
      iterback (fun a b -> if not (a = b) then eq := false) a b;
      if !eq then copy ~dst:c ~src:vdd else copy ~dst:c ~src:gnd
    ;;

    let ( <>: ) c a b =
      let neq = ref false in
      iterback (fun a b -> if not (a = b) then neq := true) a b;
      if !neq then copy ~dst:c ~src:vdd else copy ~dst:c ~src:gnd
    ;;

    let ( <: ) c a b =
      let cmp = ref 0 in
      let set_cmp a b =
        if !cmp = 0
        then
          if Int.compare a b > 0 then cmp := 1 else if Int.compare a b < 0 then cmp := -1
      in
      iterback
        (fun a b ->
           let a' = half_hi a in
           let b' = half_hi b in
           set_cmp a' b';
           let a' = half_lo a in
           let b' = half_lo b in
           set_cmp a' b')
        a
        b;
      if !cmp = -1 then copy ~dst:c ~src:vdd else copy ~dst:c ~src:gnd
    ;;

    let mux c sel l =
      let len = List.length l in
      let idx = to_int sel in
      let idx = if idx >= len then len - 1 else idx in
      copy ~dst:c ~src:(List.nth_exn l idx)
    ;;

    let concat c l =
      let c_width = List.fold l ~init:0 ~f:(fun a b -> a + width b) in
      assert (c_width = width c);
      let c_words = words c_width in
      let c_with l = data c, l in
      let cat a b =
        let a_width, a_words = width a, words (width a) in
        let b_width, b_words = width b, words (width b) in
        let a_bits = a_width mod nbits in
        let a, b = data a, data b in
        if a_bits = 0
        then
          for (* aligned *)
            i = 0 to b_words - 1 do
            set_word a (a_words + i) (get_word b i)
          done
        else (
          (* not aligned *)
          let merge x y = x lor (y lsl a_bits), y lsr (nbits - a_bits) in
          for i = 0 to b_words - 1 do
            let x, y = merge (get_word a (a_words - 1 + i)) (get_word b i) in
            set_word a (a_words - 1 + i) x;
            if a_words + i < c_words then set_word a (a_words + i) y
          done);
        c_with (a_width + b_width)
      in
      let l = List.rev l in
      if List.is_empty l
      then ()
      else (
        let c = cat (c_with 0) (List.hd_exn l) in
        ignore (List.fold (List.tl_exn l) ~init:c ~f:(fun c a -> cat c a) : t))
    ;;

    let select c s h l =
      let c_width = h - l + 1 in
      assert (c_width = width c);
      let c_words = words c_width in
      let s_bits = l mod nbits in
      let lo_word = word_index l in
      let hi_word = word_index h in
      let s = data s in
      let merge i =
        let a = get_word s i in
        let b = if i >= hi_word then 0 else get_word s (i + 1) in
        (a lsr s_bits) lor (b lsl (nbits - s_bits))
      in
      if s_bits = 0
      then
        for i = 0 to c_words - 1 do
          set_word (data c) i (get_word s (lo_word + i))
        done
      else
        for i = 0 to c_words - 1 do
          set_word (data c) i (merge (lo_word + i))
        done;
      mask c
    ;;

    let ( *: ) c a b =
      let d = T.(a *: b) in
      copy ~dst:c ~src:d
    ;;

    let ( *+ ) c a b =
      let d = T.(a *+ b) in
      copy ~dst:c ~src:d
    ;;

    let num_words t = words (width t)
    let get_word (t, _) i = get_word t i
    let set_word (t, _) i w = set_word t i w
  end

  include T

  module Comb = Comb.Make (struct
      type t = T.t

      let equal = T.equal

      let mk1 w f a =
        let x = T.create w in
        f x a;
        x
      ;;

      let mk2 w f a b =
        let x = T.create w in
        f x a b;
        x
      ;;

      let mk3 w f a b c =
        let x = T.create w in
        f x a b c;
        x
      ;;

      let empty = T.empty
      let is_empty = T.is_empty
      let width = T.width

      let of_constant c =
        Constant.to_binary_string c |> abits_int_of_bstr, Constant.width c
      ;;

      let to_constant (b, width) = bstr_of_abits_int ~width b |> Constant.of_binary_string

      let concat_msb l =
        let w = List.fold l ~init:0 ~f:(fun w y -> w + width y) in
        mk1 w T.concat l
      ;;

      let mux s l =
        let w = width (List.hd_exn l) in
        mk2 w T.mux s l
      ;;

      let select s h l =
        let w = h - l + 1 in
        mk3 w T.select s h l
      ;;

      let ( -- ) = T.( -- )
      let ( &: ) a = mk2 (width a) T.( &: ) a
      let ( |: ) a = mk2 (width a) T.( |: ) a
      let ( ^: ) a = mk2 (width a) T.( ^: ) a
      let ( ~: ) a = mk1 (width a) T.( ~: ) a
      let ( +: ) a = mk2 (width a) T.( +: ) a
      let ( -: ) a = mk2 (width a) T.( -: ) a
      let ( *: ) a b = mk2 (width a + width b) T.( *: ) a b
      let ( *+ ) a b = mk2 (width a + width b) T.( *+ ) a b
      let ( ==: ) = mk2 1 T.( ==: )
      let ( <: ) = mk2 1 T.( <: )
      let to_string = T.to_string
      let sexp_of_t s = [%sexp (to_constant s |> Constant.to_binary_string : string)]
    end)
end

let pp fmt t = Caml.Format.fprintf fmt "%s" (to_bstr t)

module PP = Pretty_printer.Register (struct
    type nonrec t = t

    let module_name = "Hardcaml.Bits"
    let to_string = to_bstr
  end)
