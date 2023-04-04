open Core
open Hardcaml
open Hardcaml.Signal
open Quickcheck
open Quickcheck.Let_syntax

let clock = input "clock" 1
let reset_sig = input "reset" 1

let gen_bits_string width =
  let%map w = Generator.list_with_length width Generator.bool in
  List.map w ~f:(fun b -> if b then '1' else '0') |> String.of_char_list
;;

let gen_const width =
  let%map s = gen_bits_string width in
  of_bit_string s
;;

let gen_bits width =
  let%map s = gen_bits_string width in
  Bits.of_string s
;;

let max_width = 200

let gen_width =
  let%map w = Generator.of_list [ 1; 2; 3; 64; 100 ] in
  Int.min max_width w
;;

let rec gen_op2 width depth inputs =
  let%bind op = Generator.of_list [ ( +: ); ( -: ); ( &: ); ( |: ) ] in
  let%bind arg1 = gen_signal width (depth - 1) inputs in
  let%map arg2 = gen_signal width (depth - 1) inputs in
  op arg1 arg2

and gen_mul width depth inputs =
  if width = 1
  then gen_cat_or_bool 1 depth inputs
  else (
    let%bind op = Generator.of_list [ ( *: ); ( *+ ) ] in
    let%bind first_width = Int.gen_incl 1 (width - 1) in
    let second_width = width - first_width in
    let%bind arg1 = gen_signal first_width (depth - 1) inputs in
    let%map arg2 = gen_signal second_width (depth - 1) inputs in
    op arg1 arg2)

and gen_cat_or_bool width depth inputs =
  if width = 1
  then (
    let%bind op = Generator.of_list [ ( ==: ); ( <: ) ] in
    let%bind width = gen_width in
    let%bind arg1 = gen_signal width (depth - 1) inputs in
    let%map arg2 = gen_signal width (depth - 1) inputs in
    op arg1 arg2)
  else (
    let%bind a = Int.gen_incl 1 (width - 1) in
    let%bind arg1 = gen_signal a (depth - 1) inputs in
    let%map arg2 = gen_signal (width - a) (depth - 1) inputs in
    Signal.concat_msb [ arg1; arg2 ])

and gen_mux width depth inputs =
  let%bind sel_width = Generator.of_list [ 1; 2 ] in
  let%bind sel = gen_signal sel_width (depth - 1) inputs in
  let%map values =
    Generator.list_with_length (1 lsl sel_width) (gen_signal width (depth - 1) inputs)
  in
  Signal.mux sel values

and gen_select width depth inputs =
  let new_width = Int.min (width * 2) max_width in
  let%bind v = gen_signal new_width (depth - 1) inputs in
  let%map start = Int.gen_incl 0 (new_width - width) in
  Signal.select v (start + width - 1) start

and gen_register width depth inputs =
  let%bind enable = gen_signal 1 (depth - 1) inputs in
  let%bind want_clear = Generator.bool in
  let%bind clear =
    if want_clear then gen_signal 1 (depth - 1) inputs else return Signal.empty
  in
  let%bind clear_value =
    if want_clear then gen_signal width (depth - 1) inputs else return Signal.empty
  in
  (* cyclesim only supports global reset signal *)
  let%bind reset_value = gen_const width in
  let reg_spec =
    { Signal.reg_clock = clock
    ; reg_clock_edge = Rising
    ; reg_reset = reset_sig
    ; reg_reset_edge = Rising
    ; reg_reset_value = reset_value
    ; reg_clear = clear
    ; reg_clear_level = High
    ; reg_clear_value = clear_value
    ; reg_enable = enable
    }
  in
  (* reg_fb *)
  let d = wire width in
  let q = reg reg_spec ~enable d in
  let%map input = gen_signal width (depth - 1) (q :: inputs) in
  d <== input;
  q

and gen_memory width depth inputs =
  let%bind write_data = gen_signal width (depth - 1) inputs in
  let address_size = 2 in
  let%bind write_address = gen_signal address_size (depth - 1) inputs in
  let%bind read_address = gen_signal address_size (depth - 1) inputs in
  let%bind write_enable = gen_signal 1 (depth - 1) inputs in
  let arr =
    Signal.multiport_memory
      ~name:"mem"
      ~attributes:[ Hardcaml.Rtl_attribute.Vivado.Ram_style.distributed ]
      ~write_ports:[| { write_clock = clock; write_data; write_enable; write_address } |]
      ~read_addresses:[| read_address |]
      4
  in
  Generator.return arr.(0)

and gen_wire width depth inputs =
  let%map s = gen_signal width (depth - 1) inputs in
  Signal.wireof s

and gen_signal width depth inputs =
  let inputs_of_width =
    List.filter ~f:(fun signal -> Signal.width signal = width) inputs
  in
  if depth = 0
  then
    if List.is_empty inputs_of_width
    then gen_const width
    else Generator.union [ gen_const width; Generator.of_list inputs_of_width ]
  else (
    let%bind result =
      Generator.weighted_union
        [ 2.0, gen_op2 width depth inputs
        ; 0.5, gen_mul width depth inputs
        ; 2.0, gen_cat_or_bool width depth inputs
        ; 2.0, gen_register width depth inputs
        ; 2.0, gen_mux width depth inputs
        ; 2.0, gen_memory width depth inputs
        ; 2.0, gen_select width depth inputs
        ; 1.0, gen_wire width depth inputs
        ]
    in
    if%map Generator.bool then ~:result else result)
;;

let gen ~width ~depth ~inputs = gen_signal width depth inputs

let gen_circuit ~allow_inputs ~depth =
  let%bind width = gen_width in
  let%bind input_widths =
    if allow_inputs then Generator.list gen_width else Generator.return []
  in
  let inputs =
    List.mapi input_widths ~f:(fun i width -> Signal.input (sprintf "input%d" i) width)
  in
  let%map signal = gen_signal width depth inputs in
  Circuit.create_exn ~name:"generated" [ output "out" signal ]
;;

let rec gen_bits_list = function
  | width :: rest ->
    let%map b = gen_bits width
    and rest_b = gen_bits_list rest in
    b :: rest_b
  | [] -> Generator.return []
;;

let gen_input_data circuit =
  let inputs =
    Circuit.inputs circuit
    |> List.filter ~f:(fun signal ->
      match List.hd_exn (Signal.names signal) with
      | "reset" | "clock" -> false
      | _ -> true)
  in
  let names = List.map inputs ~f:(fun signal -> List.hd_exn (Signal.names signal)) in
  let widths = List.map inputs ~f:Signal.width in
  let%map bits_list = gen_bits_list widths in
  List.zip_exn names bits_list
;;
