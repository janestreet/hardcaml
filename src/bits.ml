open! Import

type t = Bits0.t

module Unsafe = struct
  let data t = t.Bits0.data
end

module Mutable = struct
  include Bits0

  external unsafe_get : Bytes.t -> int -> int64 = "%caml_bytes_get64u"
  external unsafe_set : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"

  let unsafe_get b i = unsafe_get b (i lsl shift_bytes_to_words)
  let unsafe_set b i v = unsafe_set b (i lsl shift_bytes_to_words) v
  let is_empty a = a.width = 0
  let of_constant t = t
  let to_constant t = t
  let to_string t = Constant.to_binary_string t
  let to_int t = Constant.to_int t

  let copy ~src ~dst =
    let words = words src in
    for i = 0 to words - 1 do
      unsafe_set dst.data i (unsafe_get src.data i)
    done
  ;;

  let wire _ = empty
  let ( -- ) a _ = a
  let vdd = of_constant (Constant.of_int ~width:1 1)
  let gnd = of_constant (Constant.of_int ~width:1 0)

  let ( &: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      unsafe_set c.data i (Int64.( land ) (unsafe_get a.data i) (unsafe_get b.data i))
    done
  ;;

  let ( |: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      unsafe_set c.data i (Int64.( lor ) (unsafe_get a.data i) (unsafe_get b.data i))
    done
  ;;

  let ( ^: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      unsafe_set c.data i (Int64.( lxor ) (unsafe_get a.data i) (unsafe_get b.data i))
    done
  ;;

  external mask
    :  (int[@untagged])
    -> Bytes.t
    -> unit
    = "hardcaml_bits_mask_bc" "hardcaml_bits_mask"
  [@@noalloc]

  let ( ~: ) c a =
    let words = words a in
    for i = 0 to words - 1 do
      unsafe_set c.data i (Int64.lnot (unsafe_get a.data i))
    done;
    mask c.width c.data
  ;;

  external add
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_add_bc" "hardcaml_bits_add"
  [@@noalloc]

  let ( +: ) dst a b =
    let width = width dst in
    add width dst.data a.data b.data;
    mask width dst.data
  ;;

  external sub
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_sub_bc" "hardcaml_bits_sub"
  [@@noalloc]

  let ( -: ) dst a b =
    let width = width dst in
    sub width dst.data a.data b.data;
    mask width dst.data
  ;;

  (* Unsigned Int64 compare. *)
  external cmpu64
    :  (Int64.t[@unboxed])
    -> (Int64.t[@unboxed])
    -> (int[@untagged])
    = "hardcaml_bits_uint64_compare_bc" "hardcaml_bits_uint64_compare"
  [@@noalloc]

  let rec eq words i a b =
    if i = words
    then Int64.one
    else if Int64.equal (unsafe_get a.data i) (unsafe_get b.data i)
    then eq words (i + 1) a b
    else Int64.zero
  ;;

  let ( ==: ) c a b =
    let words = words a in
    unsafe_set c.data 0 (eq words 0 a b)
  ;;

  let rec neq words i a b =
    if i = words
    then Int64.zero
    else if Int64.equal (unsafe_get a.data i) (unsafe_get b.data i)
    then neq words (i + 1) a b
    else Int64.one
  ;;

  let ( <>: ) c a b =
    let words = words a in
    unsafe_set c.data 0 (neq words 0 a b)
  ;;

  let rec lt i a b =
    if i < 0
    then Int64.zero (* must be equal *)
    else (
      match cmpu64 (unsafe_get a.data i) (unsafe_get b.data i) with
      | -1 -> Int64.one
      | 0 -> lt (i - 1) a b
      | _ -> Int64.zero)
  ;;

  let ( <: ) c a b =
    let words = words a in
    unsafe_set c.data 0 (lt (words - 1) a b)
  ;;

  let[@cold] raise_mux_of_empty_list () =
    raise_s [%message "Bits.mux unexpected empty list"]
  ;;

  (* For [mux2] this is ever so slightly slower.  For mux16, it's slightly faster. *)
  let rec mux_find idx n l =
    match l with
    | [] -> raise_mux_of_empty_list ()
    | [ h ] -> h
    | h :: t -> if idx = n then h else mux_find idx (n + 1) t
  ;;

  let mux dst sel l =
    let idx = to_int sel in
    copy ~src:(mux_find idx 0 l) ~dst
  ;;

  let cat c_words a_width a b =
    let a_words = words_of_width a_width in
    let b_width, b_words = width b, words b in
    let a_bits = a_width land width_mask in
    let a, b = a.data, b.data in
    if a_bits = 0
    then
      (* aligned *)
      for i = 0 to b_words - 1 do
        unsafe_set a (a_words + i) (unsafe_get b i)
      done
    else
      (* not aligned *)
      for i = 0 to b_words - 1 do
        let x = unsafe_get a (a_words - 1 + i) in
        let y = unsafe_get b i in
        let x = Int64.(x lor (y lsl a_bits)) in
        unsafe_set a (a_words - 1 + i) x;
        if a_words + i < c_words
        then (
          let y = Int64.(y lsr Int.(bits_per_word - a_bits)) in
          unsafe_set a (a_words + i) y)
      done;
    a_width + b_width
  ;;

  (* This implementation allocates due to [List.rev], but is slightly faster:

     {[
       let rec cat_iter c_words width c t =
         match t with
         | [] -> ()
         | h :: t ->
           let width = cat c_words width c h in
           cat_iter c_words width c t

       let concat_fast_allocs c l =
         let c_words = words c in
         match List.rev l with
         | [] -> ()
         | h :: t ->
           copy ~src:h ~dst:c;
           cat_iter c_words (width h) c t
     ]} *)
  let rec cat_iter_back c_words width_ c l =
    match l with
    | [] -> ()
    | h :: t ->
      let width = width_ - width h in
      cat_iter_back c_words width c t;
      ignore (cat c_words width c h : int)
  ;;

  let concat c l =
    let c_words = words c in
    cat_iter_back c_words (width c) c l
  ;;

  let word w = w lsr log_bits_per_word

  let select c s h l =
    let c_width = h - l + 1 in
    let c_words = words c in
    let s_bits = l land width_mask in
    let lo_word = word l in
    let hi_word = word h in
    let s = s.data in
    if s_bits = 0
    then
      for i = 0 to c_words - 1 do
        unsafe_set c.data i (unsafe_get s (lo_word + i))
      done
    else
      for i = 0 to c_words - 1 do
        let j = lo_word + i in
        let a = unsafe_get s j in
        let b = if j >= hi_word then Int64.zero else unsafe_get s (j + 1) in
        let x = Int64.((a lsr s_bits) lor (b lsl Int.(bits_per_word - s_bits))) in
        unsafe_set c.data i x
      done;
    mask c_width c.data
  ;;

  external umul
    :  Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> (int[@untagged])
    -> (int[@untagged])
    -> unit
    = "hardcaml_bits_umul_bc" "hardcaml_bits_umul"
  [@@noalloc]

  external smul
    :  Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> (int[@untagged])
    -> (int[@untagged])
    -> unit
    = "hardcaml_bits_smul_bc" "hardcaml_bits_smul"
  [@@noalloc]

  let ( *: ) dst a b =
    umul dst.data a.data b.data (width a) (width b);
    mask (width dst) dst.data
  ;;

  let ( *+ ) dst a b =
    smul dst.data a.data b.data (width a) (width b);
    mask (width dst) dst.data
  ;;

  let num_words = words
  let get_word t = unsafe_get t.data
  let set_word t = unsafe_set t.data

  let to_bits t =
    let result = create (width t) in
    copy ~src:t ~dst:result;
    result
  ;;

  let copy_bits = copy

  module Comb = Comb.Make (struct
      type t = Bits0.t

      let equal = Bits0.Comparable.equal
      let empty = empty
      let is_empty = is_empty
      let width = width
      let of_constant = of_constant
      let to_constant = to_constant
      let add_widths w y = w + width y

      let concat_msb l =
        let w = List.fold l ~init:0 ~f:add_widths in
        let c = create w in
        concat c l;
        c
      ;;

      (* this is specialised to return an element from the input list, rather than
         construct a new output. *)
      let mux sel l =
        let idx = to_int sel in
        mux_find idx 0 l
      ;;

      let select s h l =
        let w = h - l + 1 in
        let c = create w in
        select c s h l;
        c
      ;;

      let ( -- ) = ( -- )

      let ( &: ) a b =
        let c = create (width a) in
        ( &: ) c a b;
        c
      ;;

      let ( |: ) a b =
        let c = create (width a) in
        ( |: ) c a b;
        c
      ;;

      let ( ^: ) a b =
        let c = create (width a) in
        ( ^: ) c a b;
        c
      ;;

      let ( ~: ) a =
        let c = create (width a) in
        ( ~: ) c a;
        c
      ;;

      let ( +: ) a b =
        let c = create (width a) in
        ( +: ) c a b;
        c
      ;;

      let ( -: ) a b =
        let c = create (width a) in
        ( -: ) c a b;
        c
      ;;

      let ( *: ) a b =
        let c = create (width a + width b) in
        ( *: ) c a b;
        c
      ;;

      let ( *+ ) a b =
        let c = create (width a + width b) in
        ( *+ ) c a b;
        c
      ;;

      let ( ==: ) a b =
        let c = create 1 in
        ( ==: ) c a b;
        c
      ;;

      let ( <: ) a b =
        let c = create 1 in
        ( <: ) c a b;
        c
      ;;

      let to_string = to_string
      let sexp_of_t (s : t) = [%sexp (to_constant s |> Constant.to_binary_string : string)]
    end)
end

include (Mutable.Comb : Comb.S with type t := Bits0.t)
include Bits0.Comparable

(* Override the functor implementations, as these allocate less (to_int doesn't allocate
   at all) *)
let to_int x = Constant.to_int x
let to_int32 x = Constant.to_int32 x
let zero w = Bits0.create w
let pp fmt t = Caml.Format.fprintf fmt "%s" (to_bstr t)

module PP = Pretty_printer.Register (struct
    type nonrec t = Bits0.t

    let module_name = "Hardcaml.Bits"
    let to_string = to_bstr
  end)
