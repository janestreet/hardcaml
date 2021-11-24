open Base

type t = Bits0.t

let number_of_data_bytes (t : t) = Bits0.number_of_data_bytes t
let unsafe_get_int64 = Bits0.unsafe_get_int64
let unsafe_set_int64 = Bits0.unsafe_set_int64

module Expert = struct
  let unsafe_underlying_repr (t : t) = (t :> Bytes.t)
  let offset_for_data = 8
end

module Mutable = struct
  include Bits0

  let is_empty a = width a = 0
  let of_constant t = t
  let to_constant t = t
  let to_string t = Constant.to_binary_string t
  let to_int t = Constant.to_int t

  let copy ~src ~dst =
    let words = words src in
    for i = 0 to words - 1 do
      unsafe_set_int64 dst i (unsafe_get_int64 src i)
    done
  ;;

  let wire _ = empty
  let ( -- ) a _ = a
  let vdd = of_constant (Constant.of_int ~width:1 1)
  let gnd = of_constant (Constant.of_int ~width:1 0)

  let ( &: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      let x = Int64.( land ) (unsafe_get_int64 a i) (unsafe_get_int64 b i) in
      unsafe_set_int64 c i x
    done
  ;;

  let ( |: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      let x = Int64.( lor ) (unsafe_get_int64 a i) (unsafe_get_int64 b i) in
      unsafe_set_int64 c i x
    done
  ;;

  let ( ^: ) c a b =
    let words = words a in
    for i = 0 to words - 1 do
      let x = Int64.( lxor ) (unsafe_get_int64 a i) (unsafe_get_int64 b i) in
      unsafe_set_int64 c i x
    done
  ;;

  let ( ~: ) c a =
    (* Use [lxor] instead of [lnot] since the former is exposed as a primitive
       in base's mli. *)
    let words = words a in
    for i = 0 to words - 1 do
      unsafe_set_int64 c i (Int64.( lxor ) (-1L) (unsafe_get_int64 a i))
    done;
    mask c
  ;;

  external add : t -> t -> t -> unit = "hardcaml_bits_add" [@@noalloc]

  let ( +: ) dst a b =
    add dst a b;
    mask dst
  ;;

  external sub : t -> t -> t -> unit = "hardcaml_bits_sub" [@@noalloc]

  let ( -: ) dst a b =
    sub dst a b;
    mask dst
  ;;

  (* [eq], [neq], [lt] returns int rather than int64 to prevent allocation or any
     memory indirection.
  *)
  let rec eq words i a b =
    if i = words
    then 1
    else if Int64.( = ) (unsafe_get_int64 a i) (unsafe_get_int64 b i)
    then eq words (i + 1) a b
    else 0
  ;;

  let ( ==: ) c a b =
    let words = words a in
    unsafe_set_int64 c 0 (Int64.of_int (eq words 0 a b))
  ;;

  let rec neq words i a b =
    if i = words
    then 0L
    else if Int64.( = ) (unsafe_get_int64 a i) (unsafe_get_int64 b i)
    then neq words (i + 1) a b
    else 1L
  ;;

  let ( <>: ) c a b =
    let words = words a in
    unsafe_set_int64 c 0 (neq words 0 a b)
  ;;

  let rec lt i a b =
    if i < 0
    then 0 (* must be equal *)
    else (
      match Caml.Int64.unsigned_compare (unsafe_get_int64 a i) (unsafe_get_int64 b i) with
      | -1 -> 1
      | 0 -> lt (i - 1) a b
      | _ -> 0)
  ;;

  let ( <: ) c a b =
    let words = words a in
    (* We cannot special case <= 64, since OCaml does not have unsigned
       comparison primitives yet. *)
    if width a <= 63
    then (
      let result =
        (* Note: Caml.Obj.magic is the most efficient way to convert bool to int without
           incurring a branch. But it is a code smell. Benchmark measurements so
           negligible gain anyway. If it ends up in a blessed core library, then we'll use
           it. *)
        if Int64.( < ) (unsafe_get_int64 a 0) (unsafe_get_int64 b 0) then 1L else 0L
      in
      unsafe_set_int64 c 0 result)
    else unsafe_set_int64 c 0 (Int64.of_int (lt (words - 1) a b))
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

  let cat2 a_width a b =
    let b_width = width b in
    let a_words = words_of_width a_width in
    let b_words = words_of_width b_width in
    let a_bits = a_width land width_mask in
    if a_bits = 0
    then
      (* If the next bit to write to the is 64-bit-aligned, skip the unnecessary
         bit shifts.
      *)
      for i = 0 to b_words - 1 do
        unsafe_set_int64 a (a_words + i) (unsafe_get_int64 b i)
      done
    else (
      (* The general case where next bit to write is not 64-bit aligned follows. *)
      (* The below memory access is always safe, as (a_bits != 0 --> a_words > 0).
         This buffers the first word in [a], to be bitwise OR-ed with data from
         [b].
      *)
      let x = ref (unsafe_get_int64 a (a_words - 1)) in
      (* The following loop takes the bottom [64 - a_bits] and OR it with [a_bits]
         from either an earlier word or the existing data in [a]. [x] is then
         updated to contain the uppermost [a_bits] from this word in [b].
      *)
      for i = 0 to b_words - 1 do
        let y = unsafe_get_int64 b i in
        unsafe_set_int64 a (a_words - 1 + i) Int64.(!x lor (y lsl a_bits));
        x := Int64.O.(y lsr Int.(64 - a_bits))
      done;
      (* [x] contains residual data, that is either the a[words - 1] (when b_words
         = 0), or the upper [a_bits] of the last 64-bit word of [b]'s data.

         The following conditional checks if the residual word is within the bound
         of [b].
      *)
      let num_bits_in_last_word = b_width land 63 in
      let num_bits_in_last_word =
        if num_bits_in_last_word = 0 then 64 else num_bits_in_last_word
      in
      if num_bits_in_last_word > 64 - a_bits
      then unsafe_set_int64 a (a_words + b_words - 1) !x)
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
  let rec cat_iter_back width_ c l =
    match l with
    | [] -> ()
    | h :: t ->
      let width_ = width_ - width h in
      cat_iter_back width_ c t;
      cat2 width_ c h
  ;;

  let concat c l = cat_iter_back (width c) c l

  let concat_rev_array c l =
    let acc_width = ref 0 in
    for i = 0 to Array.length l - 1 do
      let h = Array.unsafe_get l i in
      let width = width h in
      cat2 !acc_width c h;
      acc_width := !acc_width + width
    done
  ;;

  let word w = w lsr log_bits_per_word

  let select dst src h l =
    let words = words dst in
    let s_bits = l land width_mask in
    let lo_word = word l in
    let hi_word = word h in
    if s_bits = 0
    then
      (* If first selected bit position is 64-bit aligned, use short-circuit
         that skip all the bit shifting
      *)
      for i = 0 to words - 1 do
        unsafe_set_int64 dst i (unsafe_get_int64 src (lo_word + i))
      done
    else (
      (* The following routine loops through [words] words in [src], can
         concatenate the upper [bits] of the src[low_word+i] with the bottom
         [64-bits] of [low_word+i+1]. src[low_word_i] is conveniently buffered
         in the [a] variable.
      *)
      let a = ref (unsafe_get_int64 src lo_word) in
      for i = 0 to words - 1 do
        let b =
          if lo_word + i >= hi_word then 0L else unsafe_get_int64 src (lo_word + i + 1)
        in
        let x = Int64.O.((b lsl Int.O.(64 - s_bits)) lor (!a lsr s_bits)) in
        unsafe_set_int64 dst i x;
        a := b
      done);
    mask dst
  ;;

  external umul : t -> t -> t -> unit = "hardcaml_bits_umul" [@@noalloc]
  external smul : t -> t -> t -> unit = "hardcaml_bits_smul" [@@noalloc]

  let ( *: ) dst a b =
    umul dst a b;
    mask dst
  ;;

  let ( *+ ) dst a b =
    smul dst a b;
    mask dst
  ;;

  let num_words = words

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

module _ = Pretty_printer.Register (struct
    type nonrec t = Bits0.t

    let module_name = "Hardcaml.Bits"
    let to_string = to_bstr
  end)
