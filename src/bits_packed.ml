[@@@ocaml.flambda_o3]

open Base

type t = Bytes.t

let set64 bytes address value = Bytes.unsafe_set_int64 bytes (address * 8) value
let get64 bytes address = Bytes.unsafe_get_int64 bytes (address * 8)
let num_words bits = (bits + 63) lsr 6

let masks =
  Array.init 64 ~f:(fun i -> if i = 0 then -1L else Int64.( lsr ) (-1L) (64 - i))
;;

let mask t ~dst_address ~width_in_bits =
  let mask = masks.(width_in_bits land 0x3f) in
  set64 t dst_address (Int64.( land ) (get64 t dst_address) mask)
;;

type op2_width =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit

type op2_size =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> size_in_words:int
  -> unit

type op2_mul =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit

external add
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  = "hardcaml_bits_packed_add"
  [@@noalloc]

external sub
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  = "hardcaml_bits_packed_sub"
  [@@noalloc]

external umul_packed
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit
  = "hardcaml_bits_packed_umul_bc" "hardcaml_bits_packed_umul"
  [@@noalloc]

let mulu t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b =
  let width_in_bits = width_in_bits_a + width_in_bits_b in
  let size_in_words = num_words width_in_bits in
  umul_packed
    t
    ~dst_address
    ~src_address_a
    ~src_address_b
    ~width_in_bits_a
    ~width_in_bits_b;
  mask t ~dst_address:(dst_address + size_in_words - 1) ~width_in_bits
;;

external smul_packed
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit
  = "hardcaml_bits_packed_smul_bc" "hardcaml_bits_packed_smul"
  [@@noalloc]

let muls t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b =
  let width_in_bits = width_in_bits_a + width_in_bits_b in
  let size_in_words = num_words width_in_bits in
  smul_packed
    t
    ~dst_address
    ~src_address_a
    ~src_address_b
    ~width_in_bits_a
    ~width_in_bits_b;
  mask t ~dst_address:(dst_address + size_in_words - 1) ~width_in_bits
;;

(* Those functions we implement in C for byte/native code, we implement by bouncing
   through the standard [Bits] implementation for javascript. This is more about the
   ability to compile than performance, though we dont care much about javascript
   performance today anyway. *)
module For_javascript = struct
  let alloc_mutable buffer ~address ~width_in_bits =
    let bits = Bits.Mutable.create width_in_bits in
    let size_in_words = num_words width_in_bits in
    for i = 0 to size_in_words - 1 do
      Bits.Mutable.unsafe_set_int64 bits i (get64 buffer (address + i))
    done;
    bits
  ;;

  let set_from_mutable bits t ~dst_address ~size_in_words =
    for i = 0 to size_in_words - 1 do
      set64 t (dst_address + i) (Bits.Mutable.unsafe_get_int64 bits i)
    done
  ;;

  let add t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
    let a = alloc_mutable t ~address:src_address_a ~width_in_bits in
    let b = alloc_mutable t ~address:src_address_b ~width_in_bits in
    let c = Bits.Mutable.create width_in_bits in
    Bits.Mutable.( +: ) c a b;
    set_from_mutable c t ~dst_address ~size_in_words:(num_words width_in_bits)
  ;;

  let sub t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
    let a = alloc_mutable t ~address:src_address_a ~width_in_bits in
    let b = alloc_mutable t ~address:src_address_b ~width_in_bits in
    let c = Bits.Mutable.create width_in_bits in
    Bits.Mutable.( -: ) c a b;
    set_from_mutable c t ~dst_address ~size_in_words:(num_words width_in_bits)
  ;;

  let mulu t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b =
    let a = alloc_mutable t ~address:src_address_a ~width_in_bits:width_in_bits_a in
    let b = alloc_mutable t ~address:src_address_b ~width_in_bits:width_in_bits_b in
    let c_width = width_in_bits_a + width_in_bits_b in
    let c = Bits.Mutable.create c_width in
    Bits.Mutable.( *: ) c a b;
    set_from_mutable c t ~dst_address ~size_in_words:(num_words c_width)
  ;;

  let muls t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b =
    let a = alloc_mutable t ~address:src_address_a ~width_in_bits:width_in_bits_a in
    let b = alloc_mutable t ~address:src_address_b ~width_in_bits:width_in_bits_b in
    let c_width = width_in_bits_a + width_in_bits_b in
    let c = Bits.Mutable.create c_width in
    Bits.Mutable.( *+ ) c a b;
    set_from_mutable c t ~dst_address ~size_in_words:(num_words c_width)
  ;;
end

let add =
  match Sys.backend_type with
  | Native | Bytecode -> add
  | Other _ -> For_javascript.add
;;

let sub =
  match Sys.backend_type with
  | Native | Bytecode -> sub
  | Other _ -> For_javascript.sub
;;

let mulu =
  match Sys.backend_type with
  | Native | Bytecode -> mulu
  | Other _ -> For_javascript.mulu
;;

let muls =
  match Sys.backend_type with
  | Native | Bytecode -> muls
  | Other _ -> For_javascript.muls
;;

let and_ t ~dst_address ~src_address_a ~src_address_b ~size_in_words =
  for i = 0 to size_in_words - 1 do
    set64
      t
      (dst_address + i)
      (Int64.( land ) (get64 t (src_address_a + i)) (get64 t (src_address_b + i)))
  done
;;

let or_ t ~dst_address ~src_address_a ~src_address_b ~size_in_words =
  for i = 0 to size_in_words - 1 do
    set64
      t
      (dst_address + i)
      (Int64.( lor ) (get64 t (src_address_a + i)) (get64 t (src_address_b + i)))
  done
;;

let xor t ~dst_address ~src_address_a ~src_address_b ~size_in_words =
  for i = 0 to size_in_words - 1 do
    set64
      t
      (dst_address + i)
      (Int64.( lxor ) (get64 t (src_address_a + i)) (get64 t (src_address_b + i)))
  done
;;

let eq t ~dst_address ~src_address_a ~src_address_b ~size_in_words =
  let rec eq i =
    if i = size_in_words
    then 1L
    else if Int64.( = ) (get64 t (src_address_a + i)) (get64 t (src_address_b + i))
    then eq (i + 1)
    else 0L
  in
  set64 t dst_address (eq 0)
;;

let rec lt_iter t i ~src_address_a ~src_address_b =
  if i < 0
  then 0L (* must be equal *)
  else (
    match
      Stdlib.Int64.unsigned_compare
        (get64 t (src_address_a + i))
        (get64 t (src_address_b + i))
    with
    | -1 -> 1L
    | 0 -> lt_iter t (i - 1) ~src_address_a ~src_address_b
    | _ -> 0L)
;;

let lt t ~dst_address ~src_address_a ~src_address_b ~size_in_words =
  set64 t dst_address (lt_iter t (size_in_words - 1) ~src_address_a ~src_address_b)
;;

let not' t ~dst_address ~src_address ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  for i = 0 to size_in_words - 1 do
    set64 t (dst_address + i) (Int64.(lnot) (get64 t (src_address + i)))
  done;
  mask t ~dst_address:(dst_address + size_in_words - 1) ~width_in_bits
;;

module Cat_src = struct
  type t =
    { address : int
    ; width : int
    }
end

let cat2 t a_width a_words a b_width b_words b =
  let a_bits = a_width land 0b11_1111 in
  if a_bits = 0
  then
    for i = 0 to b_words - 1 do
      set64 t (a + a_words + i) (get64 t (b + i))
    done
  else (
    let x = ref (get64 t (a + a_words - 1)) in
    for i = 0 to b_words - 1 do
      let y = get64 t (b + i) in
      set64 t (a + a_words - 1 + i) Int64.(!x lor (y lsl a_bits));
      x := Int64.O.(y lsr Int.(64 - a_bits))
    done;
    let num_bits_in_last_word = b_width land 63 in
    let num_bits_in_last_word =
      if num_bits_in_last_word = 0 then 64 else num_bits_in_last_word
    in
    if num_bits_in_last_word > 64 - a_bits then set64 t (a + a_words + b_words - 1) !x)
;;

let rec cat_iter_back t c_width c (l : Cat_src.t list) =
  match l with
  | [] -> ()
  | h :: tl ->
    let c_width = c_width - h.width in
    cat_iter_back t c_width c tl;
    let words_c = num_words c_width in
    let words_h = num_words h.width in
    cat2 t c_width words_c c h.width words_h h.address
;;

let cat t ~dst_address (srcs : Cat_src.t list) ~width_in_bits =
  cat_iter_back t width_in_bits dst_address srcs
;;

module Cat_src_array = struct
  type t =
    { src_address : int
    ; src_width : int
    ; src_words : int
    ; dst_width : int
    ; dst_words : int
    }

  let rec create ~acc_dst_width l =
    match l with
    | [] -> []
    | { Cat_src.address; width } :: t ->
      let x =
        let dst_width = acc_dst_width + width in
        { src_address = address
        ; src_width = width
        ; src_words = num_words width
        ; dst_width
        ; dst_words = num_words dst_width
        }
      in
      x :: create ~acc_dst_width:x.dst_width t
  ;;

  let create l = create ~acc_dst_width:0 (List.rev l) |> Array.of_list
end

let cat_array t ~dst_address (srcs : Cat_src_array.t array) =
  for i = 0 to Array.length srcs - 1 do
    let s = Array.unsafe_get srcs i in
    cat2 t s.dst_width s.dst_words dst_address s.src_width s.src_words s.src_address
  done
;;

let select t ~dst_address ~src_address ~high ~low =
  let word x = x lsr 6 in
  let width_in_bits = high - low + 1 in
  let size_in_words = num_words width_in_bits in
  let s_bits = low land 0b11_1111 in
  let lo_word = word low in
  let hi_word = word high in
  if s_bits = 0
  then
    for i = 0 to size_in_words - 1 do
      set64 t (dst_address + i) (get64 t (src_address + (lo_word + i)))
    done
  else (
    let a = ref (get64 t (src_address + lo_word)) in
    for i = 0 to size_in_words - 1 do
      let b =
        if lo_word + i >= hi_word then 0L else get64 t (src_address + (lo_word + i + 1))
      in
      let x = Int64.O.((b lsl Int.O.(64 - s_bits)) lor (!a lsr s_bits)) in
      set64 t (dst_address + i) x;
      a := b
    done);
  mask t ~dst_address:(dst_address + size_in_words - 1) ~width_in_bits
;;

let mux t ~dst_address ~select_address ~(cases : int array) ~size_in_words =
  let select = get64 t select_address |> Int64.to_int_exn in
  (* clip to number of cases. *)
  let select = min select (Array.length cases - 1) in
  let case_address = Array.unsafe_get cases select in
  for i = 0 to size_in_words - 1 do
    set64 t (dst_address + i) (get64 t (case_address + i))
  done
;;
