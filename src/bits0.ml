open! Import

module T = struct
  type t =
    { width : int
    ; data : Bytes.t
    }
  [@@deriving compare, sexp_of]
end

include T
module Comparable = Comparable.Make (T)

let bits_per_word = 64
let log_bits_per_word = 6
let shift_bits_to_bytes = 3
let shift_bytes_to_words = 3
let width_mask = 0b11_1111
let words_of_width width = (width + bits_per_word - 1) lsr log_bits_per_word
let words t = words_of_width t.width
let width t = t.width

let create_bytes width =
  Bytes.make (words_of_width width lsl shift_bytes_to_words) '\000'
;;

let create width = { width; data = create_bytes width }
let init ~width ~data = { width; data }
let empty = create 0
