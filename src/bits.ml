[@@@ocaml.flambda_o3]

open Base

type t = Bits0.t

let number_of_data_bytes (t : t) = Bits0.number_of_data_bytes t
let unsafe_get_int64 = Bits0.unsafe_get_int64
let unsafe_set_int64 = Bits0.unsafe_set_int64
let get_int64 = Bits0.get_int64
let set_int64 = Bits0.set_int64

module Expert = struct
  let unsafe_underlying_repr (t : t) = (t :> Bytes.t)
  let offset_for_data = 8
end

module Mutable = struct
  include Bits0

  let number_of_data_bytes (t : t) = Bits0.number_of_data_bytes t
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

  external add_stub : t -> t -> t -> unit = "hardcaml_bits_add" [@@noalloc]

  (* Note; across the recursive call we turn [carry] into an int. This avoids an
     allocation (same in the sub routine below) *)
  let[@inline always] rec add_iter words dst a_in b_in i carry =
    if i = words
    then ()
    else (
      let a = unsafe_get_int64 a_in i in
      let b = unsafe_get_int64 b_in i in
      let lo =
        Int64.((a land 0xFFFF_FFFFL) + (b land 0xFFFF_FFFFL) + Int64.of_int carry)
      in
      let carry = Int64.(lo lsr 32) in
      let lo = Int64.(lo land 0xFFFF_FFFFL) in
      let a = Int64.(a lsr 32) in
      let b = Int64.(b lsr 32) in
      let hi = Int64.(a + b + carry) in
      let carry = Int64.(hi lsr 32) in
      unsafe_set_int64 dst i Int64.(lo lor (hi lsl 32));
      add_iter words dst a_in b_in (i + 1) (Int64.to_int_trunc carry))
  ;;

  let add_ocaml dst a b =
    let words = words dst in
    add_iter words dst a b 0 0;
    mask dst
  ;;

  let ( +: ) =
    match Sys.backend_type with
    | Native | Bytecode -> add_stub
    | Other _ -> add_ocaml
  ;;

  external sub_stub : t -> t -> t -> unit = "hardcaml_bits_sub" [@@noalloc]

  let rec sub_iter words dst a_in b_in i borrow =
    if i = words
    then ()
    else (
      let a = unsafe_get_int64 a_in i in
      let b = unsafe_get_int64 b_in i in
      let lo =
        Int64.((a land 0xFFFF_FFFFL) - (b land 0xFFFF_FFFFL) - Int64.of_int borrow)
      in
      let borrow = Int64.((lo lsr 32) land 1L) in
      let lo = Int64.(lo land 0xFFFF_FFFFL) in
      let a = Int64.(a lsr 32) in
      let b = Int64.(b lsr 32) in
      let hi = Int64.(a - b - borrow) in
      let borrow = Int64.((hi lsr 32) land 1L) in
      unsafe_set_int64 dst i Int64.(lo lor (hi lsl 32));
      sub_iter words dst a_in b_in (i + 1) (Int64.to_int_trunc borrow))
  ;;

  let sub_ocaml dst a b =
    let words = words dst in
    sub_iter words dst a b 0 0;
    mask dst
  ;;

  let ( -: ) =
    match Sys.backend_type with
    | Native | Bytecode -> sub_stub
    | Other _ -> sub_ocaml
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
      match
        Stdlib.Int64.unsigned_compare (unsafe_get_int64 a i) (unsafe_get_int64 b i)
      with
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

  (* Internal logic for performing signed and unsigned multiplication. Both OCaml and C
     implementations are provided. *)
  module Multiplication = struct
    external umul : t -> t -> t -> unit = "hardcaml_bits_umul" [@@noalloc]
    external smul : t -> t -> t -> unit = "hardcaml_bits_smul" [@@noalloc]

    let[@inline always] unsafe_get32 d i =
      Int64.( land ) (Int64.of_int32 (unsafe_get_int32 d i)) 0xFFFF_FFFFL
    ;;

    let[@inline always] unsafe_set32 d i v = unsafe_set_int32 d i (Int32.of_int64_trunc v)

    (* C-implementation of the unsigned multiplier. *)
    let umul_stub dst a b =
      umul dst a b;
      mask dst
    ;;

    (** Array multiplication. *)
    let rec umul_inner_loop w u v m i j k =
      if i = m
      then k
      else (
        let u' = unsafe_get32 u i in
        let v' = unsafe_get32 v j in
        let w' = unsafe_get32 w (i + j) in
        let t = Int64.((u' * v') + w' + k) in
        unsafe_set32 w (i + j) t;
        umul_inner_loop w u v m (i + 1) j Int64.(t lsr 32))
    ;;

    let rec umul_outer_loop w u v m n j =
      if j = n
      then ()
      else (
        let k = umul_inner_loop w u v m 0 j 0L in
        unsafe_set32 w (j + m) k;
        umul_outer_loop w u v m n (j + 1))
    ;;

    let umul_core w u v m n = umul_outer_loop w u v m n 0

    (* The multiplier requires an output buffer which is not always the actual size of the
       destination buffer. Rather than allocating a new buffer every time, we keep one
       around and reallocate whenever a bigger multiplication happens. *)
    let mul_dst () =
      let dst = ref (create 64) in
      let setup_dst w =
        (* reallocating dst when the size changes is about 30ns (6%) quicker than
           allocating - and probably avoids some GC churn. *)
        if width !dst < w
        then dst := create w
        else
          for i = 0 to words_of_width w - 1 do
            unsafe_set_int64 !dst i 0L
          done
      in
      dst, setup_dst
    ;;

    (* Sign extend the (top) word *)
    let sign_extend width word =
      let width = width land width_mask in
      let top_bit_shift = width - 1 in
      if width = 0 || Int64.equal Int64.(word land (1L lsl top_bit_shift)) 0L
      then word
      else Int64.((-1L lsl width) lor word)
    ;;

    (* The arguments to the signed multiplier need to be sign extended. We should not
       mutate the inputs (I suppose we could if we put the zeros back at end). Similar to
       the output buffer we keep and internal buffer around which resizes according to the
       largest seen arguments. *)
    let smul_arg () =
      let arg = ref (create 64) in
      let setup_arg arg_in =
        let width_arg = width arg_in in
        if width !arg < width_arg then arg := create width_arg;
        let words = words_of_width width_arg in
        for i = 0 to words - 2 do
          unsafe_set_int64 !arg i (unsafe_get_int64 arg_in i)
        done;
        (* sign extend last word *)
        unsafe_set_int64
          !arg
          (words - 1)
          (sign_extend width_arg (unsafe_get_int64 arg_in (words - 1)))
      in
      arg, setup_arg
    ;;

    (* Setup the output buffer (clear it), perform the array multiplication, then copy to
       the output dst. *)
    let umul_ocaml =
      let dst', setup_dst = mul_dst () in
      fun dst a b ->
        let dst_bits =
          (words_of_width (width a) + words_of_width (width b)) * bits_per_word
        in
        setup_dst dst_bits;
        umul_core !dst' a b (words a lsl 1) (words b lsl 1);
        blit_data dst !dst';
        mask dst
    ;;

    (* Signed multiplication in C *)
    let smul_stub dst a b =
      smul dst a b;
      mask dst
    ;;

    (* A signed multiplication is done by correcting an unsigned result *)
    let rec smul_signed_correct w v n m j b =
      if j = n
      then ()
      else (
        let jm = j + m in
        let t = Int64.(unsafe_get32 w jm - unsafe_get32 v j - Int64.of_int_exn b) in
        unsafe_set32 w jm t;
        smul_signed_correct w v n m (j + 1) Int64.(t lsr 63 |> to_int_trunc))
    ;;

    let smul_core w u v m n =
      umul_core w u v m n;
      if Int64.equal (Int64.( land ) (unsafe_get32 u (m - 1)) 0x8000_0000L) 0x8000_0000L
      then smul_signed_correct w v n m 0 0;
      if Int64.equal (Int64.( land ) (unsafe_get32 v (n - 1)) 0x8000_0000L) 0x8000_0000L
      then smul_signed_correct w u m n 0 0
    ;;

    (* Setup the output buffer (clear it), sign extend arguments, perform the array
       multiplication, then copy to the output dst. *)
    let smul_ocaml =
      let dst', setup_dst = mul_dst () in
      let a', setup_a = smul_arg () in
      let b', setup_b = smul_arg () in
      fun dst a b ->
        let dst_bits =
          (words_of_width (width a) + words_of_width (width b)) * bits_per_word
        in
        setup_dst dst_bits;
        setup_a a;
        setup_b b;
        smul_core !dst' !a' !b' (words a lsl 1) (words b lsl 1);
        blit_data dst !dst';
        mask dst
    ;;
  end

  let ( *: ) =
    match Sys.backend_type with
    | Native | Bytecode -> Multiplication.umul_stub
    | Other _ -> Multiplication.umul_ocaml
  ;;

  let ( *+ ) =
    match Sys.backend_type with
    | Native | Bytecode -> Multiplication.smul_stub
    | Other _ -> Multiplication.smul_ocaml
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
let pp fmt t = Stdlib.Format.fprintf fmt "%s" (to_bstr t)

(* Install pretty printer. *)
module _ = Pretty_printer.Register (struct
  type nonrec t = Bits0.t

  let module_name = "Hardcaml.Bits"
  let to_string = to_bstr
end)
