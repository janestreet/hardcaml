open! Core0

type t =
  { used_length : int
  ; data : int array
  }

let bin_shape_t = Array.bin_shape_t Int.bin_shape_t

let validate_for_write t =
  if t.used_length < 0 || t.used_length > Array.length t.data
  then
    raise_s
      [%message
        "Invalid time slice length"
          (t.used_length : int)
          ~data_length:(Array.length t.data : int)]
;;

let bin_size_t t =
  validate_for_write t;
  let used_length = Bin_prot.Nat0.unsafe_of_int t.used_length in
  let size = ref (Bin_prot.Size.bin_size_nat0 used_length) in
  let previous = ref 0 in
  for index = 0 to (used_length :> int) - 1 do
    let time = t.data.(index) in
    size := !size + Int.bin_size_t (time - !previous);
    previous := time
  done;
  !size
;;

let bin_write_t buf ~pos t =
  validate_for_write t;
  let used_length = Bin_prot.Nat0.unsafe_of_int t.used_length in
  let pos = ref (Bin_prot.Write.bin_write_nat0 buf ~pos used_length) in
  let previous = ref 0 in
  for index = 0 to (used_length :> int) - 1 do
    let time = t.data.(index) in
    pos := Int.bin_write_t buf ~pos:!pos (time - !previous);
    previous := time
  done;
  !pos
;;

let bin_read_t buf ~pos_ref =
  let used_length = (Bin_prot.Read.bin_read_nat0 buf ~pos_ref :> int) in
  let data = Array.create ~len:used_length 0 in
  let previous = ref 0 in
  for index = 0 to used_length - 1 do
    let time = !previous + Int.bin_read_t buf ~pos_ref in
    data.(index) <- time;
    previous := time
  done;
  { used_length; data }
;;

let __bin_read_t__ _buf ~pos_ref _vint =
  Bin_prot.Common.raise_variant_wrong_type "Delta_encoded_times.t" !pos_ref
;;

let bin_writer_t : t Bin_prot.Type_class.writer =
  { size = bin_size_t; write = bin_write_t }
;;

let bin_reader_t : t Bin_prot.Type_class.reader =
  { read = bin_read_t; vtag_read = __bin_read_t__ }
;;

let bin_t : t Bin_prot.Type_class.t =
  { shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
;;
