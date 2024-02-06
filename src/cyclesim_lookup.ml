open Base

module T = struct
  type t =
    { byte_address : int
    ; data : Bytes.t
    ; width_in_bits : int
    ; size_in_words : int
    ; read_only : bool
    }
  [@@deriving fields ~getters]

  let unsafe_set64 t idx value =
    Bytes.unsafe_set_int64 t.data (t.byte_address + (idx * 8)) value
  ;;

  let unsafe_get64 t idx = Bytes.unsafe_get_int64 t.data (t.byte_address + (idx * 8))

  let bounds_check t idx =
    if idx < 0 || idx >= t.size_in_words
    then
      raise_s
        [%message
          "[Internal_node] index out of bounds" (t.size_in_words : int) (idx : int)]
  ;;

  let get64 t idx =
    bounds_check t idx;
    unsafe_get64 t idx
  ;;

  let set64 t idx value =
    bounds_check t idx;
    unsafe_set64 t idx value
  ;;

  let check_width t value_width =
    if value_width <> t.width_in_bits
    then
      raise_s
        [%message
          "[Internal_node] width mismatch" (value_width : int) (t.width_in_bits : int)]
  ;;

  let check_writable t =
    if t.read_only then raise_s [%message "Cannot set simulation node - read-only"]
  ;;

  let to_bits_mutable t value =
    check_width t (Bits.Mutable.width value);
    for i = 0 to t.size_in_words - 1 do
      Bits.Mutable.set_int64 value i (get64 t i)
    done
  ;;

  let of_bits_mutable t value =
    check_writable t;
    check_width t (Bits.Mutable.width value);
    for i = 0 to t.size_in_words - 1 do
      set64 t i (Bits.Mutable.get_int64 value i)
    done
  ;;

  let to_bits t =
    let value = Bits.zero t.width_in_bits in
    for i = 0 to t.size_in_words - 1 do
      Bits.set_int64 value i (get64 t i)
    done;
    value
  ;;

  let of_bits t value =
    check_writable t;
    check_width t (Bits.width value);
    for i = 0 to t.size_in_words - 1 do
      set64 t i (Bits.get_int64 value i)
    done
  ;;

  let to_int t = to_bits t |> Bits.to_int
  let of_int t i = of_bits t (Bits.of_int ~width:t.width_in_bits i)

  let create_from_bits_mutable (bits : Bits.Mutable.t) =
    { byte_address = Bits.Expert.offset_for_data
    ; data = (bits :> Bytes.t)
    ; size_in_words = Bits.Mutable.num_words bits
    ; width_in_bits = Bits.Mutable.width bits
    ; read_only = true
    }
  ;;

  let create_from_signal ~byte_address ~data signal =
    let width_in_bits = Signal.width signal in
    { byte_address
    ; data
    ; width_in_bits
    ; size_in_words = Int.round_up ~to_multiple_of:64 width_in_bits / 64
    ; read_only = true
    }
  ;;
end

module Node = T

module Reg = struct
  include T

  let writable t = { t with read_only = false }
  let create_from_bits_mutable bits = create_from_bits_mutable bits |> writable

  let create_from_signal ~byte_address ~data signal =
    create_from_signal ~byte_address ~data signal |> writable
  ;;

  let to_node t = t
  let read_only_of_node t = { t with read_only = true }
end

module Memory = struct
  type t =
    { unsafe_set64 : address:int -> int -> Int64.t -> unit
    ; unsafe_get64 : address:int -> int -> Int64.t
    ; size_in_words : int
    ; width_in_bits : int
    ; memory_size : int
    }
  [@@deriving fields ~getters]

  let bounds_check t ~address idx =
    if address < 0 || address >= t.memory_size
    then
      raise_s
        [%message
          "[Internal_mem] address is out of range" (t.size_in_words : int) (address : int)];
    if idx < 0 || idx >= t.size_in_words
    then
      raise_s
        [%message
          "[Internal_node] index out of bounds" (t.size_in_words : int) (idx : int)]
  ;;

  let unsafe_set64 t ~address idx value = t.unsafe_set64 ~address idx value
  let unsafe_get64 t ~address idx = t.unsafe_get64 ~address idx

  let get64 t ~address idx =
    bounds_check t ~address idx;
    unsafe_get64 t ~address idx
  ;;

  let set64 t ~address idx value =
    bounds_check t ~address idx;
    unsafe_set64 t ~address idx value
  ;;

  let check_width t value_width =
    if value_width <> t.width_in_bits
    then
      raise_s
        [%message
          "[Internal_node] width mismatch" (value_width : int) (t.width_in_bits : int)]
  ;;

  let of_bits_mutable t ~address value =
    check_width t (Bits.Mutable.width value);
    for i = 0 to t.size_in_words - 1 do
      set64 t ~address i (Bits.Mutable.get_int64 value i)
    done
  ;;

  let to_bits_mutable t ~address value =
    check_width t (Bits.Mutable.width value);
    for i = 0 to t.size_in_words - 1 do
      Bits.Mutable.set_int64 value i (get64 t ~address i)
    done
  ;;

  let to_bits t ~address =
    let value = Bits.zero t.width_in_bits in
    for i = 0 to t.size_in_words - 1 do
      Bits.set_int64 value i (get64 t ~address i)
    done;
    value
  ;;

  let of_bits t ~address value =
    check_width t (Bits.width value);
    for i = 0 to t.size_in_words - 1 do
      set64 t ~address i (Bits.get_int64 value i)
    done
  ;;

  let to_int t ~address = to_bits t ~address |> Bits.to_int
  let of_int t ~address i = of_bits t ~address (Bits.of_int ~width:t.width_in_bits i)
  let read_all t = Array.init t.memory_size ~f:(fun address -> to_bits t ~address)

  let create_from_bits_mutable_array (bits : Bits.Mutable.t array) =
    if Array.length bits = 0 then raise_s [%message "[Internal_memory] 0 size"];
    let memory_size = Array.length bits in
    let width_in_bits = Bits.Mutable.width bits.(0) in
    let size_in_words = Bits.Mutable.num_words bits.(0) in
    let unsafe_set64 ~address idx value =
      Bits.Mutable.unsafe_set_int64 bits.(address) idx value
    in
    let unsafe_get64 ~address idx = Bits.Mutable.unsafe_get_int64 bits.(address) idx in
    { unsafe_set64; unsafe_get64; size_in_words; width_in_bits; memory_size }
  ;;

  let create_from_read_only_bits_array (bits : Bits.t array) =
    if Array.length bits = 0 then raise_s [%message "[Internal_memory] 0 size"];
    let memory_size = Array.length bits in
    let width_in_bits = Bits.width bits.(0) in
    let size_in_words = Int.round_up ~to_multiple_of:64 width_in_bits / 64 in
    let unsafe_set64 ~address:_ _idx _value =
      raise_s [%message "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"]
    in
    let unsafe_get64 ~address idx = Bits.unsafe_get_int64 bits.(address) idx in
    { unsafe_set64; unsafe_get64; size_in_words; width_in_bits; memory_size }
  ;;

  let create_from_signal ~byte_address ~data signal =
    let width_in_bits = Signal.width signal in
    let size_in_words = Int.round_up ~to_multiple_of:64 width_in_bits / 64 in
    let memory_size =
      match signal with
      | Multiport_mem { size; _ } -> size
      | _ ->
        raise_s [%message "[Cyclesim_lookup.Memory.create_from_signal] not a memory node"]
    in
    let offset address index = byte_address + (8 * ((address * size_in_words) + index)) in
    { unsafe_get64 =
        (fun ~address index -> Bytes.unsafe_get_int64 data (offset address index))
    ; unsafe_set64 =
        (fun ~address index value ->
          Bytes.unsafe_set_int64 data (offset address index) value)
    ; width_in_bits
    ; size_in_words
    ; memory_size
    }
  ;;
end
