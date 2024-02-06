[@@@ocaml.flambda_o3]

open Base

module T = struct
  open Bin_prot.Std

  module For_sexp = struct
    type t =
      { width : int
      ; data : bytes
      }
    [@@deriving sexp]
  end

  type t = bytes [@@deriving compare, bin_io]

  let width t = Int64.to_int_trunc (Bytes.unsafe_get_int64 t 0)
  let offset_for_data = 8

  let sexp_of_t (t : t) =
    For_sexp.sexp_of_t
      { width = Int64.to_int_trunc (Bytes.unsafe_get_int64 t 0)
      ; data = Bytes.subo ~pos:offset_for_data t
      }
  ;;

  let t_of_sexp sexp =
    let for_sexp = For_sexp.t_of_sexp sexp in
    let t = Stdlib.Bytes.make (Bytes.length for_sexp.data) '\000' in
    Bytes.unsafe_set_int64 t 0 (Int64.of_int for_sexp.width);
    Bytes.blito ~src:for_sexp.data ~dst:t ~dst_pos:offset_for_data ();
    t
  ;;
end

include T
module Comparable = Comparable.Make (T)

let bits_per_word = 64
let log_bits_per_word = 6
let shift_bits_to_bytes = 3
let shift_bytes_to_words = 3
let shift_bytes_to_words32 = 2
let width_mask = 0b11_1111
let words_of_width width = (width + bits_per_word - 1) lsr log_bits_per_word
let bytes_of_width width = words_of_width width lsl shift_bytes_to_words
let words t = words_of_width (width t)
let number_of_data_bytes t = Bytes.length t - offset_for_data

let create width =
  let bytes = Bytes.make ((words_of_width width + 1) lsl shift_bytes_to_words) '\000' in
  Bytes.unsafe_set_int64 bytes 0 (Int64.of_int width);
  bytes
;;

let empty = create 0
let unsafe_get_byte (t : t) i = Bytes.unsafe_get t (offset_for_data + i)

let unsafe_get_int64 (t : t) i =
  Bytes.unsafe_get_int64 t ((i lsl shift_bytes_to_words) + offset_for_data)
;;

let unsafe_set_int64 (t : t) i x =
  Bytes.unsafe_set_int64 t ((i lsl shift_bytes_to_words) + offset_for_data) x
;;

let[@cold] raise_invalid_index width word_index =
  raise_s [%message "[Bits0] Invalid words index" (width : int) (word_index : int)]
;;

let get_int64 (t : t) index =
  let words = words t in
  if index < 0 || index >= words then raise_invalid_index words index;
  unsafe_get_int64 t index
;;

let set_int64 (t : t) index value =
  let words = words t in
  if index < 0 || index >= words then raise_invalid_index words index;
  unsafe_set_int64 t index value
;;

external unsafe_get_int32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external unsafe_set_int32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"

let unsafe_get_int32 (t : t) i =
  unsafe_get_int32 t ((i lsl shift_bytes_to_words32) + offset_for_data)
;;

let unsafe_set_int32 (t : t) i x =
  unsafe_set_int32 t ((i lsl shift_bytes_to_words32) + offset_for_data) x
;;

let mask (t : t) =
  let width = width t in
  let bits = width land width_mask in
  if bits <> 0
  then (
    let mask = Int64.( lsr ) (-1L) (64 - bits) in
    let word_pos = (width - 1) lsr log_bits_per_word in
    let x = Int64.( land ) (unsafe_get_int64 t word_pos) mask in
    unsafe_set_int64 t word_pos x)
;;

let init_byte ~width ~f =
  let t = create width in
  let num_bytes = words_of_width width lsl shift_bytes_to_words in
  for i = 0 to num_bytes - 1 do
    Bytes.unsafe_set t (i + offset_for_data) (f i)
  done;
  mask t;
  t
;;

let init_int64 ~width ~f =
  let t = create width in
  let num_words = words_of_width width in
  for i = 0 to num_words - 1 do
    unsafe_set_int64 t i (f i)
  done;
  mask t;
  t
;;

let blit_data dst src =
  let words = min (words dst) (words src) in
  Bytes.unsafe_blit
    ~src
    ~src_pos:offset_for_data
    ~dst
    ~dst_pos:offset_for_data
    ~len:(words lsl shift_bytes_to_words)
;;
