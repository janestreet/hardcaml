[@@@ocaml.flambda_o3]

open Base

let check_cache = true

type t =
  { mutable data : Bytes.t (* packed data *)
  ; mutable length : int (* number of elements in array *)
  ; mutable total_length : int
  ; width : int (* bit width *)
  ; rounded_width : int
      (* bit width rounded up to the nearest power of 2 if [width<64], or a multiple of 64. *)
  ; log2_rounded_width : int
  ; mutable cached_bits : Bits.t
  ; mutable cached_sub_word : int
  ; cached_multi_word : Bytes.t
  ; cached_temp_multi_word : Bytes.t
      (* this is needed to compare the underlying values. This is because we cannot do a
     [Bytes.compare] at arbitrary offsets within a buffer. *)
  ; mutable non_cache_hits : int
      (* ; set : Bytes.t -> int -> Bits.t -> unit [@compare.ignore] *)
  ; setter_index : int
  }
[@@deriving sexp_of, compare, fields ~getters, equal]

let get64u = Bytes.unsafe_get_int64
let set64u = Bytes.unsafe_set_int64
let raw_data t = t.data

let number_of_bytes_used width length =
  if width = 0 then raise_s [%message "Cannot have 0 bit data"];
  if width >= 64
  then 8 * length * ((width + 63) / 64)
  else (
    let rounded_width = 1 lsl Int.ceil_log2 width in
    if rounded_width <= 8
    then (length + (8 / rounded_width) - 1) / (8 / rounded_width)
    else rounded_width / 8 * length)
;;

let%expect_test "bytes used" =
  let open Stdio in
  let print widths =
    printf "    | ";
    List.iter widths ~f:(fun width -> printf "%3i " width);
    printf "\n";
    printf "    --";
    List.iter widths ~f:(fun _ -> printf "----");
    printf "\n";
    for length = 0 to 10 do
      printf "%3i | " length;
      List.iter widths ~f:(fun width -> printf "%3i " (number_of_bytes_used width length));
      printf "\n"
    done
  in
  print [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  [%expect
    {|
       |   1   2   3   4   5   6   7   8
       ----------------------------------
     0 |   0   0   0   0   0   0   0   0
     1 |   1   1   1   1   1   1   1   1
     2 |   1   1   1   1   2   2   2   2
     3 |   1   1   2   2   3   3   3   3
     4 |   1   1   2   2   4   4   4   4
     5 |   1   2   3   3   5   5   5   5
     6 |   1   2   3   3   6   6   6   6
     7 |   1   2   4   4   7   7   7   7
     8 |   1   2   4   4   8   8   8   8
     9 |   2   3   5   5   9   9   9   9
    10 |   2   3   5   5  10  10  10  10
    |}];
  print [ 9; 15; 16; 17; 31; 32; 33; 63; 64; 65 ];
  [%expect
    {|
       |   9  15  16  17  31  32  33  63  64  65
       ------------------------------------------
     0 |   0   0   0   0   0   0   0   0   0   0
     1 |   2   2   2   4   4   4   8   8   8  16
     2 |   4   4   4   8   8   8  16  16  16  32
     3 |   6   6   6  12  12  12  24  24  24  48
     4 |   8   8   8  16  16  16  32  32  32  64
     5 |  10  10  10  20  20  20  40  40  40  80
     6 |  12  12  12  24  24  24  48  48  48  96
     7 |  14  14  14  28  28  28  56  56  56 112
     8 |  16  16  16  32  32  32  64  64  64 128
     9 |  18  18  18  36  36  36  72  72  72 144
    10 |  20  20  20  40  40  40  80  80  80 160
    |}];
  print [ 127; 128; 129; 191; 192; 193 ];
  [%expect
    {|
       | 127 128 129 191 192 193
       --------------------------
     0 |   0   0   0   0   0   0
     1 |  16  16  24  24  24  32
     2 |  32  32  48  48  48  64
     3 |  48  48  72  72  72  96
     4 |  64  64  96  96  96 128
     5 |  80  80 120 120 120 160
     6 |  96  96 144 144 144 192
     7 | 112 112 168 168 168 224
     8 | 128 128 192 192 192 256
     9 | 144 144 216 216 216 288
    10 | 160 160 240 240 240 320
    |}]
;;

let used_raw_data_bytes t = number_of_bytes_used t.width t.length

let total_length length_in_bytes rounded_width =
  if rounded_width < 8
  then length_in_bytes * (8 / rounded_width)
  else length_in_bytes / (rounded_width / 8)
;;

let[@cold] raise_invalid_width actual_width expected_width =
  raise_s
    [%message
      "Unexpected [Bits.t] width in waveform" (actual_width : int) (expected_width : int)]
;;

(* Usable for >= 64 bits *)
let masks = Array.init 6 ~f:(fun i -> Int64.((1L lsl Int.(1 lsl i)) - 1L))
let offset_for_data = 8
let () = assert (offset_for_data = Constant.Expert.offset_for_data)

let[@cold] resize t =
  let length_in_bytes = Bytes.length t.data in
  let data = Bytes.make (length_in_bytes * 2) '\000' in
  Bytes.blit ~src:t.data ~src_pos:0 ~dst:data ~dst_pos:0 ~len:length_in_bytes;
  t.data <- data;
  t.total_length <- t.total_length * 2
;;

let number_of_data_bytes b = Bytes.length b - offset_for_data

let set_multi_word data index bits base_address =
  let bytes_per = number_of_data_bytes bits in
  Bytes.unsafe_blit
    ~src:bits
    ~src_pos:base_address
    ~dst:data
    ~dst_pos:(bytes_per * index)
    ~len:bytes_per
;;

let set log2_rounded_width data index bits base_address =
  let mask = Int64.((1L lsl Int.(1 lsl log2_rounded_width)) - 1L) in
  let shift = 6 - log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl log2_rounded_width in
  let new_bits = Bytes.unsafe_get_int64 bits base_address in
  let cur_bits = get64u data byte_offset in
  let bits =
    Int64.(cur_bits land lnot (mask lsl part_offset) lor (new_bits lsl part_offset))
  in
  set64u data byte_offset bits
;;

let set1 d i b = set 0 d i b
let set2 d i b = set 1 d i b
let set4 d i b = set 2 d i b

let[@inline always] set_unsafe log2_rounded_width data index bits base_address =
  let shift = 6 - log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl log2_rounded_width in
  let new_bits = Bytes.unsafe_get_int64 bits base_address in
  let cur_bits = get64u data byte_offset in
  let bits = Int64.(cur_bits lor (new_bits lsl part_offset)) in
  set64u data byte_offset bits
;;

let[@inline always] set1_unsafe d i b a = set_unsafe 0 d i b a
let[@inline always] set2_unsafe d i b a = set_unsafe 1 d i b a
let[@inline always] set4_unsafe d i b a = set_unsafe 2 d i b a

let[@inline always] set8 data index bits base_address =
  Bytes.unsafe_set data index (Bytes.unsafe_get bits base_address)
;;

let[@inline always] set16 data index bits base_address =
  Bytes.unsafe_set_int16 data (index lsl 1) (Bytes.unsafe_get_int16 bits base_address)
;;

let[@inline always] set32 data index bits base_address =
  Bytes.unsafe_set_int32 data (index lsl 2) (Bytes.unsafe_get_int32 bits base_address)
;;

let[@inline always] set64 data index bits base_address =
  let byte_offset = index lsl 3 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits base_address)
;;

let[@inline always] set128 data index bits base_address =
  let byte_offset = index lsl 4 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits base_address);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (base_address + 8))
;;

let[@inline always] set192 data index bits base_address =
  let byte_offset = (index lsl 4) + (index lsl 3) in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits base_address);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (base_address + 8));
  set64u data (byte_offset + 16) (Bytes.unsafe_get_int64 bits (base_address + 16))
;;

let[@inline always] set256 data index bits base_address =
  let byte_offset = index lsl 5 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits base_address);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (base_address + 8));
  set64u data (byte_offset + 16) (Bytes.unsafe_get_int64 bits (base_address + 16));
  set64u data (byte_offset + 24) (Bytes.unsafe_get_int64 bits (base_address + 24))
;;

let setters =
  [ 1, set1
  ; 2, set2
  ; 4, set4
  ; 8, set8
  ; 16, set16
  ; 32, set32
  ; 64, set64
  ; 128, set128
  ; 192, set192
  ; 256, set256
  ]
;;

let setters_unsafe =
  [ 1, set1_unsafe
  ; 2, set2_unsafe
  ; 4, set4_unsafe
  ; 8, set8
  ; 16, set16
  ; 32, set32
  ; 64, set64
  ; 128, set128
  ; 192, set192
  ; 256, set256
  ]
;;

let setter_index width =
  let rec f index = function
    | [] -> index
    | (max_width, _) :: tl -> if width <= max_width then index else f (index + 1) tl
  in
  f 0 setters
;;

let setter_fn = List.map setters ~f:snd @ [ set_multi_word ] |> Array.of_list

let setter_fn_unsafe =
  List.map setters_unsafe ~f:snd @ [ set_multi_word ] |> Array.of_list
;;

let rec set_mutable_unsafe t index (bits : Bits.Mutable.t) =
  if index >= t.total_length
  then (
    resize t;
    set_mutable_unsafe t index bits)
  else (
    setter_fn_unsafe.(t.setter_index) t.data index (bits :> Bytes.t) offset_for_data;
    t.length <- max t.length (index + 1))
;;

let set_from_bytes1 t index data byte_address =
  if index >= t.total_length then resize t;
  set1_unsafe t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes2 t index data byte_address =
  if index >= t.total_length then resize t;
  set2_unsafe t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes4 t index data byte_address =
  if index >= t.total_length then resize t;
  set4_unsafe t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes8 t index data byte_address =
  if index >= t.total_length then resize t;
  set8 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes16 t index data byte_address =
  if index >= t.total_length then resize t;
  set16 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes32 t index data byte_address =
  if index >= t.total_length then resize t;
  set32 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes64 t index data byte_address =
  if index >= t.total_length then resize t;
  set64 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes128 t index data byte_address =
  if index >= t.total_length then resize t;
  set128 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes192 t index data byte_address =
  if index >= t.total_length then resize t;
  set192 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let set_from_bytes256 t index data byte_address =
  if index >= t.total_length then resize t;
  set256 t.data index data byte_address;
  t.length <- max t.length (index + 1)
;;

let[@inline always] rec set_from_bytes_multi bytes_per_word t index bits byte_address =
  if index >= t.total_length
  then (
    resize t;
    set_from_bytes_multi bytes_per_word t index bits byte_address)
  else (
    Bytes.unsafe_blit
      ~src:bits
      ~src_pos:byte_address
      ~dst:t.data
      ~dst_pos:(bytes_per_word * index)
      ~len:bytes_per_word;
    t.length <- max t.length (index + 1))
;;

let set_from_bytes width =
  if width <= 1
  then set_from_bytes1
  else if width <= 2
  then set_from_bytes2
  else if width <= 4
  then set_from_bytes4
  else if width <= 8
  then set_from_bytes8
  else if width <= 16
  then set_from_bytes16
  else if width <= 32
  then set_from_bytes32
  else if width <= 64
  then set_from_bytes64
  else if width <= 128
  then set_from_bytes128
  else if width <= 192
  then set_from_bytes192
  else if width <= 256
  then set_from_bytes256
  else set_from_bytes_multi (8 * (Int.round_up ~to_multiple_of:64 width / 64))
;;

let rec set t index (bits : Bits.t) =
  if Bits.width bits <> t.width
  then raise_invalid_width (Bits.width bits : int) (t.width : int);
  if index >= t.total_length
  then (
    resize t;
    set t index bits)
  else (
    setter_fn.(t.setter_index) t.data index (bits :> Bytes.t) offset_for_data;
    t.length <- max t.length (index + 1))
;;

let create width =
  let rounded_width =
    if width < 64 then Int.ceil_pow2 width else Int.round_up ~to_multiple_of:64 width
  in
  (* at least 1 64 bit word, or as many words as needed for a single element. *)
  let length_in_bytes = max 8 (rounded_width / 8) in
  { data = Bytes.make length_in_bytes '\000'
  ; length = 0
  ; total_length = total_length length_in_bytes rounded_width
  ; width
  ; rounded_width
  ; log2_rounded_width = min 9 (Int.ceil_log2 rounded_width)
  ; cached_bits = Bits.zero width
  ; cached_sub_word = 0
  ; cached_multi_word = Bytes.make length_in_bytes '\000'
  ; cached_temp_multi_word = Bytes.make length_in_bytes '\000'
  ; non_cache_hits = 0
  ; setter_index = setter_index width
  }
;;

(* >= 64 bits *)
let get_multi_word t index =
  let bytes_per_word = t.rounded_width lsr 3 in
  Bytes.blit
    ~src:t.data
    ~src_pos:(bytes_per_word * index)
    ~dst:t.cached_temp_multi_word
    ~dst_pos:0
    ~len:bytes_per_word;
  if check_cache && Bytes.equal t.cached_temp_multi_word t.cached_multi_word
  then t.cached_bits
  else (
    let bits = Bits.zero t.width in
    let bits_underlying_repr = Bits.Expert.unsafe_underlying_repr bits in
    Bytes.blit
      ~src:t.cached_temp_multi_word
      ~src_pos:0
      ~dst:bits_underlying_repr
      ~dst_pos:Bits.Expert.offset_for_data
      ~len:bytes_per_word;
    Bytes.blit
      ~src:t.cached_temp_multi_word
      ~src_pos:0
      ~dst:t.cached_multi_word
      ~dst_pos:0
      ~len:bytes_per_word;
    t.non_cache_hits <- t.non_cache_hits + 1;
    t.cached_bits <- bits;
    bits)
;;

(* < 64 bits *)
let get_sub_word t index =
  (* extract the value *)
  let mask = masks.(t.log2_rounded_width) in
  let shift = 6 - t.log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl t.log2_rounded_width in
  let bits = get64u t.data byte_offset in
  let bits = Int64.((bits lsr part_offset) land mask) |> Int64.to_int_trunc in
  (* check if it is cached. *)
  if check_cache && bits = t.cached_sub_word
  then t.cached_bits
  else (
    t.non_cache_hits <- t.non_cache_hits + 1;
    t.cached_sub_word <- bits;
    t.cached_bits <- Bits.of_int ~width:t.width bits;
    t.cached_bits)
;;

let[@cold] raise_index_out_of_bounds index length =
  raise_s [%message "Index into waveform is out of bounds" (index : int) (length : int)]
;;

let get t index =
  if index >= t.length || index < 0
  then raise_index_out_of_bounds index t.length
  else if t.rounded_width < 64
  then get_sub_word t index
  else get_multi_word t index
;;

let init length ~width ~f =
  let t = create width in
  for index = 0 to length - 1 do
    set t index (f index)
  done;
  t
;;

let get_digestible_string t = raw_data t, used_raw_data_bytes t
