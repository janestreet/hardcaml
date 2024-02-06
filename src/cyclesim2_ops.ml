(* Optimisations are enabled for a few performance critical files in Hardcaml. This allows
   much better inlining and code gen, though we still lack cross module inlining. *)
[@@@ocaml.flambda_o3]

open Base

let run_bounds_checks = false

let bounds_check bytes address =
  if address < 0 || address * 8 >= Bytes.length bytes
  then raise_s [%message "Bad bounds" (address : int)]
;;

let[@inline always] set64 bytes address value =
  if run_bounds_checks then bounds_check bytes address;
  Bytes.unsafe_set_int64 bytes (address * 8) value
;;

let[@inline always] get64 bytes address =
  if run_bounds_checks then bounds_check bytes address;
  Bytes.unsafe_get_int64 bytes (address * 8)
;;

let[@inline always] get64i bytes address = get64 bytes address |> Int64.to_int_trunc
let[@inline always] int64_equal a b = if Int64.(a = b) then 1L else 0L
(* let[@inline always] int64_equal a b = Bool.select Int64.(a = b) 1L 0L *)

let[@inline always] int64_less_than a b =
  if Stdlib.Int64.unsigned_compare a b = -1 then 1L else 0L
;;

(* let[@inline always] int64_less_than a b =
 *   Bool.select (Stdlib.Int64.unsigned_compare a b = -1) 1L 0L
 * ;; *)

let dispatch_on_width width ~less_than_64 ~exactly_64 ~more_than_64 =
  if width < 64 then less_than_64 else if width = 64 then exactly_64 else more_than_64
;;

let masks =
  Array.init 64 ~f:(fun i -> if i = 0 then -1L else Int64.( lsr ) (-1L) (64 - i))
;;

let num_words w = (w + 63) / 64

let add t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let mask = masks.(width_in_bits land 0b11_1111) in
  let add_small () =
    set64 t dst_address Int64.((get64 t src_address_a + get64 t src_address_b) land mask)
  in
  let add_64 () =
    set64 t dst_address Int64.(get64 t src_address_a + get64 t src_address_b)
  in
  let add_large () =
    Bits_packed.add t ~dst_address ~src_address_a ~src_address_b ~width_in_bits
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:add_small
    ~exactly_64:add_64
    ~more_than_64:add_large
;;

let sub t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let mask = masks.(width_in_bits land 0b11_1111) in
  let sub_small () =
    set64 t dst_address Int64.((get64 t src_address_a - get64 t src_address_b) land mask)
  in
  let sub_64 () =
    set64 t dst_address Int64.(get64 t src_address_a - get64 t src_address_b)
  in
  let sub_large () =
    Bits_packed.sub t ~dst_address ~src_address_a ~src_address_b ~width_in_bits
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:sub_small
    ~exactly_64:sub_64
    ~more_than_64:sub_large
;;

let mulu t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b ()
  =
  Bits_packed.mulu
    t
    ~dst_address
    ~src_address_a
    ~src_address_b
    ~width_in_bits_a
    ~width_in_bits_b
;;

let muls t ~dst_address ~src_address_a ~src_address_b ~width_in_bits_a ~width_in_bits_b ()
  =
  Bits_packed.muls
    t
    ~dst_address
    ~src_address_a
    ~src_address_b
    ~width_in_bits_a
    ~width_in_bits_b
;;

let and_ t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  let and_small () =
    set64 t dst_address Int64.(get64 t src_address_a land get64 t src_address_b)
  in
  let and_large () =
    Bits_packed.and_ t ~dst_address ~src_address_a ~src_address_b ~size_in_words
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:and_small
    ~exactly_64:and_small
    ~more_than_64:and_large
;;

let or_ t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  let or_small () =
    set64 t dst_address Int64.(get64 t src_address_a lor get64 t src_address_b)
  in
  let or_large () =
    Bits_packed.or_ t ~dst_address ~src_address_a ~src_address_b ~size_in_words
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:or_small
    ~exactly_64:or_small
    ~more_than_64:or_large
;;

let xor t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  let xor_small () =
    set64 t dst_address Int64.(get64 t src_address_a lxor get64 t src_address_b)
  in
  let xor_large () =
    Bits_packed.xor t ~dst_address ~src_address_a ~src_address_b ~size_in_words
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:xor_small
    ~exactly_64:xor_small
    ~more_than_64:xor_large
;;

let eq t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  let eq_small () =
    set64 t dst_address (int64_equal (get64 t src_address_a) (get64 t src_address_b))
  in
  let eq_large () =
    Bits_packed.eq t ~dst_address ~src_address_a ~src_address_b ~size_in_words
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:eq_small
    ~exactly_64:eq_small
    ~more_than_64:eq_large
;;

let lt t ~dst_address ~src_address_a ~src_address_b ~width_in_bits =
  let size_in_words = num_words width_in_bits in
  let lt_small () =
    set64 t dst_address (int64_less_than (get64 t src_address_a) (get64 t src_address_b))
  in
  let lt_large () =
    Bits_packed.lt t ~dst_address ~src_address_a ~src_address_b ~size_in_words
  in
  dispatch_on_width
    width_in_bits
    ~less_than_64:lt_small
    ~exactly_64:lt_small
    ~more_than_64:lt_large
;;

let not_ t ~dst_address ~src_address ~width_in_bits =
  let mask = masks.(width_in_bits land 0b11_1111) in
  let not_small () = set64 t dst_address Int64.(get64 t src_address lxor mask) in
  let not_large () = Bits_packed.not' t ~dst_address ~src_address ~width_in_bits in
  dispatch_on_width
    width_in_bits
    ~less_than_64:not_small
    ~exactly_64:not_small
    ~more_than_64:not_large
;;

let mux t ~dst_address ~select_address ~select_width ~cases ~size_in_words =
  if size_in_words = 1
  then (
    let max = Array.length cases - 1 in
    if Array.length cases = 2 && select_width = 1
    then (
      let case0 = cases.(0) in
      let case1 = cases.(1) in
      let mux2 () =
        set64
          t
          dst_address
          (get64 t (Bool.select (get64i t select_address = 0) case0 case1))
      in
      mux2)
    else (
      let mux_small () =
        let select = get64i t select_address in
        let select = if select > max then max else select in
        (* let select = Bool.select (select > max) max select in *)
        set64 t dst_address (get64 t (Array.unsafe_get cases select))
      in
      mux_small))
  else (
    let mux_large () =
      Bits_packed.mux t ~dst_address ~select_address ~cases ~size_in_words
    in
    mux_large)
;;

let cat t ~dst_address (cat_src : Bits_packed.Cat_src.t list) ~width_in_bits =
  let cat_small =
    let cat_src = Array.of_list cat_src in
    let count = Array.length cat_src in
    let cat_small () =
      let acc = ref 0L in
      for i = 0 to count - 1 do
        let cat_src = Array.unsafe_get cat_src i in
        acc := Int64.(get64 t cat_src.address lor (!acc lsl cat_src.width))
      done;
      set64 t dst_address !acc
    in
    cat_small
  in
  (* let cat_large =
   *   let cat_src = Bits_packed.Cat_src_array.create cat_src in
   *   let cat_large () = Bits_packed.cat_array t ~dst_address cat_src in
   *   cat_large
   * in *)
  let cat_large () = Bits_packed.cat t ~dst_address cat_src ~width_in_bits in
  dispatch_on_width
    width_in_bits
    ~less_than_64:cat_small
    ~exactly_64:cat_small
    ~more_than_64:cat_large
;;

let select t ~dst_address ~src_address ~high ~low =
  let dst_width = high - low + 1 in
  let src_address, high, low =
    let rec shift_down src_address high low =
      if low < 64
      then src_address, high, low
      else shift_down (src_address + 1) (high - 64) (low - 64)
    in
    shift_down src_address high low
  in
  if high < 64
  then (
    let mask = masks.(dst_width land 0b11_1111) in
    let sel_small () =
      set64 t dst_address Int64.((get64 t src_address lsr low) land mask)
    in
    sel_small)
  else (
    let sel_large () = Bits_packed.select t ~dst_address ~src_address ~high ~low in
    sel_large)
;;

let mem_read t ~dst_address ~read_address ~memory_address ~memory_size ~size_in_words =
  if size_in_words = 1
  then (
    let memread1 () =
      let read_address = get64i t read_address in
      if read_address < memory_size
      then (
        let read_address = memory_address + read_address in
        set64 t dst_address (get64 t read_address))
    in
    memread1)
  else (
    let memreadn () =
      let read_address = get64i t read_address in
      if read_address < memory_size
      then
        for i = 0 to size_in_words - 1 do
          let read_address = memory_address + (read_address * size_in_words) in
          set64 t (dst_address + i) (get64 t (read_address + i))
        done
    in
    memreadn)
;;

type clear =
  { clear : int
  ; clear_value : int
  ; level : int
  }

let reg t ~clear ~enable ~dst_address ~src_address ~size_in_words =
  let copy =
    let copy1 () = set64 t dst_address (get64 t src_address) in
    let copyn () =
      for i = 0 to size_in_words - 1 do
        set64 t (dst_address + i) (get64 t (src_address + i))
      done
    in
    if size_in_words = 1 then copy1 else copyn
  in
  let copy_enabled enable =
    let copy_enabled1 () =
      if get64i t enable = 1 then set64 t dst_address (get64 t src_address)
    in
    let copy_enabledn () =
      if get64i t enable = 1
      then
        for i = 0 to size_in_words - 1 do
          set64 t (dst_address + i) (get64 t (src_address + i))
        done
    in
    if size_in_words = 1 then copy_enabled1 else copy_enabledn
  in
  let copy_cleared { clear; clear_value; level } =
    let copy_cleared1 () =
      if get64i t clear = level
      then set64 t dst_address (get64 t clear_value)
      else set64 t dst_address (get64 t src_address)
    in
    let copy_clearedn () =
      if get64i t clear = level
      then
        for i = 0 to size_in_words - 1 do
          set64 t (dst_address + i) (get64 t (clear_value + i))
        done
      else
        for i = 0 to size_in_words - 1 do
          set64 t (dst_address + i) (get64 t (src_address + i))
        done
    in
    if size_in_words = 1 then copy_cleared1 else copy_clearedn
  in
  let copy_cleared_and_enabled enable { clear; clear_value; level } =
    let copy_cleared_and_enabled1 () =
      if get64i t clear = level
      then set64 t dst_address (get64 t clear_value)
      else if get64i t enable = 1
      then set64 t dst_address (get64 t src_address)
    in
    let copy_cleared_and_enabledn () =
      if get64i t clear = level
      then
        for i = 0 to size_in_words - 1 do
          set64 t (dst_address + i) (get64 t (clear_value + i))
        done
      else if get64i t enable = 1
      then
        for i = 0 to size_in_words - 1 do
          set64 t (dst_address + i) (get64 t (src_address + i))
        done
    in
    if size_in_words = 1 then copy_cleared_and_enabled1 else copy_cleared_and_enabledn
  in
  match enable, clear with
  | None, None -> copy
  | Some enable, None -> copy_enabled enable
  | None, Some clear -> copy_cleared clear
  | Some enable, Some clear -> copy_cleared_and_enabled enable clear
;;

let mem_write_port
  t
  ~size
  ~memory_address
  ~write_enable
  ~write_address
  ~write_data
  ~size_in_words
  =
  if size_in_words = 1
  then (
    let memwrite1 () =
      if get64i t write_enable = 1
      then (
        let write_address = get64i t write_address in
        if write_address < size
        then (
          let memory_address = memory_address + write_address in
          set64 t memory_address (get64 t write_data)))
    in
    memwrite1)
  else (
    let memwriten () =
      if get64i t write_enable = 1
      then (
        let write_address = get64i t write_address in
        if write_address < size
        then (
          let memory_address = memory_address + (write_address * size_in_words) in
          for i = 0 to size_in_words - 1 do
            set64 t (memory_address + i) (get64 t (write_data + i))
          done))
    in
    memwriten)
;;
