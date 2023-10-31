open Base
include Bits0

let char_zero = Char.to_int '0'
let char_a = Char.to_int 'a'
let char_A = Char.to_int 'A'

module Raw = struct
  let of_bytes bytes ~width =
    init_byte ~width ~f:(fun i ->
      try Bytes.get bytes i with
      | _ -> '\000')
  ;;

  let of_string string ~width =
    init_byte ~width ~f:(fun i ->
      try string.[i] with
      | _ -> '\000')
  ;;

  let to_bytes (t : t) =
    let len = (width t + 7) / 8 in
    Bytes.subo ~pos:8 ~len (t :> bytes)
  ;;

  let to_string t =
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:(to_bytes t)
  ;;
end

let rec rsplit_into_n ~n string =
  let len = String.length string in
  if len <= n
  then [ string ]
  else
    String.drop_prefix string (len - n) :: rsplit_into_n ~n (String.drop_suffix string n)
;;

let int_of_hex_char c =
  match c with
  | '0' .. '9' -> Char.to_int c - char_zero
  | 'a' .. 'f' -> Char.to_int c - char_a + 10
  | 'A' .. 'F' -> Char.to_int c - char_A + 10
  | _ -> raise_s [%message "Invalid hex char" ~_:(c : char)]
;;

let hex_char_of_int i =
  if i >= 0 && i <= 9
  then Char.of_int_exn (i + char_zero)
  else if i >= 10 && i <= 15
  then Char.of_int_exn (i - 10 + char_a)
  else raise_s [%message "Cannot convert int to hex char" ~int_value:(i : int)]
;;

let to_hum x = rsplit_into_n ~n:4 x |> List.rev |> String.concat ~sep:"_"

let to_binary_string t =
  let width = width t in
  String.init width ~f:(fun j ->
    let j = width - j - 1 in
    let byte = j lsr 3 in
    let bit = j land 7 in
    let char = unsafe_get_byte t byte |> Char.to_int in
    let bit = (char land (1 lsl bit)) lsr bit in
    Char.of_int_exn (bit + char_zero))
;;

let to_binary_string_hum t = to_binary_string t |> to_hum
let to_int64 t = unsafe_get_int64 t 0
let to_int t = Int64.to_int_trunc (to_int64 t)
let to_int32 t = Int64.to_int32_trunc (to_int64 t)
let to_int64_array t = Array.init (words t) ~f:(fun i -> unsafe_get_int64 t i)

let to_z ~signedness t =
  let module Z = Zarith.Z in
  let words = words t in
  let rec f word z =
    if word < 0
    then z
    else (
      let w = unsafe_get_int64 t word in
      let a = Int64.(w lsr 32) in
      let z = Z.(logor (shift_left z 32) (of_int64 a)) in
      let a = Int64.(w land 0xFFFF_FFFFL) in
      let z = Z.(logor (shift_left z 32) (of_int64 a)) in
      f (word - 1) z)
  in
  let z = f (words - 1) Z.zero in
  match (signedness : Signedness.t) with
  | Unsigned -> z
  | Signed ->
    let width = width t in
    let is_unsigned = Z.(compare (z land shift_left one Int.(width - 1)) zero = 0) in
    if is_unsigned then z else Z.(z - shift_left (of_int 1) width)
;;

let to_hex_string ~signedness t =
  let rec of_binary ~(signedness : Signedness.t) s =
    let hex_of_bin s =
      match s with
      | "0000" -> "0"
      | "0001" -> "1"
      | "0010" -> "2"
      | "0011" -> "3"
      | "0100" -> "4"
      | "0101" -> "5"
      | "0110" -> "6"
      | "0111" -> "7"
      | "1000" -> "8"
      | "1001" -> "9"
      | "1010" -> "a"
      | "1011" -> "b"
      | "1100" -> "c"
      | "1101" -> "d"
      | "1110" -> "e"
      | "1111" -> "f"
      | _ -> raise_s [%message "Invalid string"]
    in
    let len = String.length s in
    match len with
    | 0 -> raise_s [%message "[Hex.of_binary] binary value is empty"]
    | 1 | 2 | 3 ->
      hex_of_bin
        ((match signedness with
          | Signed -> String.init (4 - len) ~f:(fun _ -> s.[0])
          | Unsigned -> String.init (4 - len) ~f:(fun _ -> '0'))
         ^ s)
    | 4 -> hex_of_bin s
    | _ ->
      of_binary ~signedness (String.sub s ~pos:0 ~len:(len - 4))
      ^ hex_of_bin (String.sub s ~pos:(len - 4) ~len:4)
  in
  to_binary_string t |> of_binary ~signedness
;;

let of_hum x = String.filter x ~f:(fun c -> not Char.(c = '_'))

let of_binary_string b =
  if String.length b = 0
  then raise_s [%message "[Constant.of_binary_string] input string is empty"]
  else (
    let width = String.length b in
    let rec to_int8 b acc offset count =
      if count = 0
      then Char.of_int_exn acc
      else (
        let char = b.[offset] in
        if Char.(char = '0' || char = '1')
        then (
          let bit = Char.to_int char - char_zero in
          (* 0 or 1 *)
          to_int8 b ((acc lsl 1) lor bit) (offset + 1) (count - 1))
        else
          raise_s
            [%message
              "[Constant.of_binary_string] input must only consist of '1' or '0'"
                ~got:(b : string)])
    in
    let rec convert b x index count =
      if count = 0
      then x
      else (
        let bits = min 8 count in
        Bytes.set x index (to_int8 b 0 (count - bits) bits);
        convert b x (index + 1) (count - bits))
    in
    let data = Stdlib.Bytes.make (words_of_width width lsl shift_bytes_to_words) '\000' in
    let data = convert b data 0 width in
    init_byte ~width ~f:(fun i -> Bytes.get data i))
;;

let of_binary_string_hum b = of_binary_string (of_hum b)

let of_int64 ~width i =
  let sign_extend_value = if Int64.(i < 0L) then -1L else 0L in
  init_int64 ~width ~f:(function
    | 0 -> i
    | _ -> sign_extend_value)
;;

let of_int ~width i = of_int64 ~width (Int64.of_int i)
let of_int32 ~width i = of_int64 ~width (Int64.of_int32 i)

let of_int64_array ~width a =
  let array_width = Array.length a lsl log_bits_per_word in
  if array_width > Int.round_up ~to_multiple_of:bits_per_word width
  then
    raise_s
      [%message
        "[of_int64_array] array to large for given [width]"
          (array_width : int)
          (width : int)];
  init_int64 ~width ~f:(fun i -> a.(i))
;;

let z32mask = Zarith.Z.of_int64 0xFFFF_FFFFL

let of_z ~width z =
  let module Z = Zarith.Z in
  let t = create width in
  let rec f i z width =
    if width <= 0 || Z.(equal z zero)
    then ()
    else (
      (* grab 2 lots of 32 bits at a time. *)
      let a = Z.(to_int64 (logand z z32mask)) in
      let z = Z.shift_right z 32 in
      (* _trunc? *)
      let b = Z.(to_int64 (logand z z32mask)) in
      let z = Z.shift_right z 32 in
      (* construct 64 bit value *)
      let a = Int64.(a lor (b lsl 32)) in
      unsafe_set_int64 t i a;
      f (i + 1) z (width - 64))
  in
  f 0 z width;
  mask t;
  t
;;

let of_pow2_string ~(signedness : Signedness.t) ~width ~pow2 h =
  let to_binary t =
    let len = String.length t in
    let len_pow = len * pow2 in
    let rec make_string = function
      | 0 -> ""
      | i ->
        let c = t.[i - 1] in
        let digit =
          match c with
          | '0' .. '9' -> Char.to_int c - char_zero
          | 'A' .. 'Z' -> Char.to_int c - char_A + 10
          | 'a' .. 'z' -> Char.to_int c - char_a + 10
          | _ -> raise_s [%message "Invalid numeric char" ~_:(c : char)]
        in
        if digit >= Int.O.(2 ** pow2)
        then raise_s [%message "Invalid numeric char" ~_:(c : char)];
        make_string (i - 1) ^ (of_int ~width:pow2 digit |> to_binary_string)
    in
    let result = make_string len in
    if width < len_pow
    then String.sub result ~pos:(len_pow - width) ~len:width
    else
      String.init (width - len_pow) ~f:(fun _ ->
        match signedness with
        | Signed -> result.[0]
        | Unsigned -> '0')
      ^ result
  in
  to_binary h |> of_binary_string
;;

let of_hex_string ~signedness ~width h = of_pow2_string ~signedness ~width ~pow2:4 h
let of_octal_string ~signedness ~width h = of_pow2_string ~signedness ~width ~pow2:3 h

module type Bit = sig
  type t

  val vdd : t
  val gnd : t
  val equal : t -> t -> bool
end

module Make_bit_list (Bit : Bit) = struct
  let to_constant bits =
    List.map bits ~f:(fun bit -> if Bit.equal bit Bit.vdd then '1' else '0')
    |> String.of_char_list
    |> of_binary_string
  ;;

  let of_constant t =
    to_binary_string t
    |> String.to_list
    |> List.map ~f:(function
         | '0' -> Bit.gnd
         | _ -> Bit.vdd)
  ;;
end

module Bits = Make_bit_list (struct
  type t = int

  let vdd = 1
  let gnd = 0
  let equal = Int.equal
end)

let of_bit_list = Bits.to_constant
let to_bit_list = Bits.of_constant

include Comparable

module Expert = struct
  let offset_for_data = Bits0.offset_for_data
end

(* Pretty printer *)
let pp fmt t = Stdlib.Format.fprintf fmt "%s" (to_binary_string_hum t)

module _ = Pretty_printer.Register (struct
  type nonrec t = t

  let module_name = "Hardcaml.Constant"
  let to_string t = to_binary_string_hum t
end)
