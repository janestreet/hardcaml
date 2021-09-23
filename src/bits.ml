open Base

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

  external and_
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_and_bc" "hardcaml_bits_and"
  [@@noalloc]

  let ( &: ) c a b = and_ (width c) c.data a.data b.data

  external or_
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_or_bc" "hardcaml_bits_or"
  [@@noalloc]

  let ( |: ) c a b = or_ (width c) c.data a.data b.data

  external xor_
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_xor_bc" "hardcaml_bits_xor"
  [@@noalloc]

  let ( ^: ) c a b = xor_ (width c) c.data a.data b.data

  external mask
    :  (int[@untagged])
    -> Bytes.t
    -> unit
    = "hardcaml_bits_mask_bc" "hardcaml_bits_mask"
  [@@noalloc]

  external not_
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_not_bc" "hardcaml_bits_not"
  [@@noalloc]

  let ( ~: ) c a = not_ (width c) c.data a.data

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
    add width dst.data a.data b.data
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
    sub width dst.data a.data b.data
  ;;

  (* Unsigned Int64 compare. *)
  external _cmpu64
    :  (Int64.t[@unboxed])
    -> (Int64.t[@unboxed])
    -> (int[@untagged])
    = "hardcaml_bits_uint64_compare_bc" "hardcaml_bits_uint64_compare"
  [@@noalloc]

  external eq
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_eq_bc" "hardcaml_bits_eq"
  [@@noalloc]

  let ( ==: ) c a b = eq (width a) c.data a.data b.data

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

  external lt
    :  (int[@untagged])
    -> Bytes.t
    -> Bytes.t
    -> Bytes.t
    -> unit
    = "hardcaml_bits_lt_bc" "hardcaml_bits_lt"
  [@@noalloc]

  let ( <: ) c a b = lt (width a) c.data a.data b.data

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


  external cat2
    :  Bytes.t
    -> (int[@untagged])
    -> Bytes.t
    -> (int[@untagged])
    -> unit
    = "hardcaml_bits_cat2_bc" "hardcaml_bits_cat2"
  [@@noalloc]

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
  let rec cat_iter_back width_ c l =
    match l with
    | [] -> ()
    | h :: t ->
      let width_ = width_ - width h in
      cat_iter_back width_ c t;
      cat2 c.data width_ h.data (width h)
  ;;

  let concat c l = cat_iter_back (width c) c l

  external select
    :  Bytes.t
    -> Bytes.t
    -> (int[@untagged])
    -> (int[@untagged])
    -> unit
    = "hardcaml_bits_select_bc" "hardcaml_bits_select"
  [@@noalloc]

  let select c s h l = select c.data s.data h l

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
