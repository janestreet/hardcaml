open! Core0
include Hardcaml_kernel.Wave_format

let rec to_string t =
  match t with
  | Binary | Bit ->
    (fun (bits : Bits.t) -> Constant.to_binary_string (bits :> Constant.t))
    |> Staged.stage
  | Bit_or t ->
    let to_string (bits : Bits.t) =
      if Bits.width bits = 1
      then Constant.to_binary_string (bits :> Constant.t)
      else Staged.unstage (to_string t) bits
    in
    Staged.stage to_string
  | Hex ->
    let to_string (bits : Bits.t) =
      Constant.to_hex_string ~capitalize:true ~signedness:Unsigned (bits :> Constant.t)
    in
    Staged.stage to_string
  | Unsigned_int ->
    let to_string (bits : Bits.t) =
      match Bits.to_unsigned_int bits |> Int.to_string with
      | exception _ ->
        Constant.to_bigint ~signedness:Unsigned (bits :> Constant.t) |> Bigint.to_string
      | bits -> bits
    in
    Staged.stage to_string
  | Int ->
    let to_string (bits : Bits.t) =
      match Bits.to_signed_int bits |> Int.to_string with
      | exception _ ->
        Constant.to_bigint ~signedness:Signed (bits :> Constant.t) |> Bigint.to_string
      | bits -> bits
    in
    Staged.stage to_string
  | Index strings ->
    let strings = Array.of_list strings in
    let to_string (bits : Bits.t) =
      match strings.(Bits.to_int_trunc bits) with
      | exception _ -> "-"
      | s -> s
    in
    Staged.stage to_string
  | Map map ->
    let map = Map.of_alist_exn (module Bits) map in
    let to_string (bits : Bits.t) =
      match Map.find map bits with
      | None -> "-"
      | Some s -> s
    in
    Staged.stage to_string
  | Custom f -> Staged.stage f
;;

let rec to_string_four_state t =
  let of_two_state to_string_two_state =
    let to_string_two_state = Staged.unstage to_string_two_state in
    Staged.stage (fun value ->
      match Four_state_logic.impurity value with
      | Pure -> to_string_two_state (Four_state_logic.bits value)
      | X -> "X"
      | Z -> "Z")
  in
  match t with
  | Binary | Bit -> Staged.stage Four_state_logic.to_string
  | Bit_or t ->
    let to_string_t = Staged.unstage (to_string_four_state t) in
    Staged.stage (fun value ->
      if Four_state_logic.width value = 1
      then Four_state_logic.to_string value
      else to_string_t value)
  | Hex ->
    let hex_string = Staged.unstage (to_string Hex) in
    (* Per hex digit: pure digits display normally; impure ones as X or Z. *)
    Staged.stage (fun value ->
      match Four_state_logic.impurity value with
      | Pure -> hex_string (Four_state_logic.bits value)
      | X | Z ->
        let width = Four_state_logic.width value in
        let num_digits = Int.round_up ~to_multiple_of:4 width / 4 in
        String.init num_digits ~f:(fun i ->
          (* Digit [i] counts from the msb; digit widths are uneven only for the first. *)
          let low = 4 * (num_digits - 1 - i) in
          let high = min (width - 1) (low + 3) in
          let digit = Four_state_logic.select value ~high ~low in
          match Four_state_logic.impurity digit with
          | Pure -> hex_string (Four_state_logic.bits digit) |> Char.of_string
          | X -> 'X'
          | Z -> 'Z'))
  | Unsigned_int | Int | Index _ | Custom _ | Map _ -> of_two_state (to_string t)
;;

let default = Bit_or Hex

let of_string_exn s =
  match String.lowercase s with
  | "b" | "bin" | "binary" -> Binary
  | "h" | "hex" -> Hex
  | "u" | "uint" | "unsigned" | "unsigned-int" -> Unsigned_int
  | "s" | "int" | "sint" | "signed" | "signed-int" -> Int
  | s -> raise_s [%message "Invalid wave_format" (s : string)]
;;
