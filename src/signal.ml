open Base
open! Signal_intf
module Type = Signal__type

type t = Type.t

module Uid = Type.Uid
open Type

let names = Type.names
let uid = Type.uid

let add_attribute signal attribute =
  match signal_id signal with
  | None ->
    raise_s
      [%message
        "attempt to add attribute to an empty signal" ~to_:(attribute : Rtl_attribute.t)]
  | Some _ ->
    Type.add_attribute signal attribute;
    signal
;;

let add_attributes signal attributes =
  List.iter attributes ~f:(fun x -> ignore (add_attribute signal x : t));
  signal
;;

let set_comment signal comment =
  match signal_id signal with
  | None ->
    raise_s [%message "attempt to add comment to an empty signal" ~to_:(comment : string)]
  | Some _ ->
    Type.set_comment signal comment;
    signal
;;

let unset_comment signal =
  match signal_id signal with
  | None -> raise_s [%message "attempt to remove comment from an empty signal"]
  | Some _ ->
    Type.unset_comment signal;
    signal
;;

let set_names s names = Type.set_names s names

let attributes s =
  match signal_id s with
  | None -> []
  | Some _ -> Type.get_attributes s
;;

let comment s =
  match signal_id s with
  | None -> raise_s [%message "cannot get [comment] from the empty signal"]
  | Some _ -> Type.get_comment s
;;

let ( --$ ) s w =
  Type.set_wave_format s w;
  s
;;

module Base = struct
  (* TODO: instantiations, memories *)

  type nonrec t = t

  let equal (t1 : t) t2 =
    match t1, t2 with
    | Empty, Empty -> true
    | Const { constant = c1; _ }, Const { constant = c2; _ } -> Bits.equal c1 c2
    | _ -> Uid.equal (uid t1) (uid t2)
  ;;

  let width = width
  let to_string = to_string
  let sexp_of_t = sexp_of_t
  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let of_bits constant = Const { signal_id = make_id (Bits.width constant); constant }
  let of_constant data = of_bits (Bits.of_constant data)

  let to_constant signal =
    if is_const signal
    then Bits.to_constant (const_value signal)
    else
      raise_s [%message "cannot use [to_constant] on non-constant signal" ~_:(signal : t)]
  ;;

  let ( -- ) signal name =
    match signal_id signal with
    | None ->
      raise_s
        [%message "attempt to set the name of the empty signal" ~to_:(name : string)]
    | Some _ ->
      Type.add_name signal name;
      signal
  ;;

  let op2 op len arg_a arg_b = Op2 { signal_id = make_id len; op; arg_a; arg_b }

  let concat_msb a =
    match a with
    | [ a ] -> a
    | _ ->
      let len = List.fold a ~init:0 ~f:(fun acc a -> acc + width a) in
      Cat { signal_id = make_id len; args = a }
  ;;

  let select arg high low =
    Select { signal_id = make_id (high - low + 1); arg; high; low }
  ;;

  let ( +: ) a b = op2 Signal_add (width a) a b
  let ( -: ) a b = op2 Signal_sub (width a) a b
  let ( *: ) a b = op2 Signal_mulu (width a + width b) a b
  let ( *+ ) a b = op2 Signal_muls (width a + width b) a b
  let ( &: ) a b = op2 Signal_and (width a) a b
  let ( |: ) a b = op2 Signal_or (width a) a b
  let ( ^: ) a b = op2 Signal_xor (width a) a b
  let ( ~: ) a = Not { signal_id = make_id (width a); arg = a }
  let ( ==: ) a b = op2 Signal_eq 1 a b
  let ( <: ) a b = op2 Signal_lt 1 a b

  let mux select cases =
    match cases with
    | first_case :: _ -> Mux { signal_id = make_id (width first_case); select; cases }
    | [] -> raise_s [%message "Mux with no cases"]
  ;;
end

module Comb_make = Comb.Make
module Unoptimized = Comb.Make (Base)

let ( <== ) a b =
  match a with
  | Wire { driver; _ } ->
    if not (is_empty !driver)
    then
      raise_s
        [%message
          "attempt to assign wire multiple times"
            ~already_assigned_wire:(a : t)
            ~expression:(b : t)];
    if width a <> width b
    then
      raise_s
        [%message
          "attempt to assign expression to wire of different width"
            ~wire_width:(width a : int)
            ~expression_width:(width b : int)
            ~wire:(a : t)
            ~expression:(b : t)];
    driver := b
  | _ ->
    raise_s
      [%message
        "attempt to assign non-wire" ~assignment_target:(a : t) ~expression:(b : t)]
;;

let[@cold] raise_wire_width_0 wire =
  (* print the wire as well - this is potentially helpful if [caller_id]s are enabled.  *)
  raise_s [%message "width of wire was specified as 0" (wire : t)]
;;

let assign = ( <== )

let wire w =
  let wire = Wire { signal_id = make_id w; driver = ref Empty } in
  if w = 0 then raise_wire_width_0 wire;
  wire
;;

let wireof s =
  let x = wire (width s) in
  x <== s;
  x
;;

let input name width = Unoptimized.( -- ) (wire width) name

let output name s =
  let w = Unoptimized.( -- ) (wire (width s)) name in
  w <== s;
  w
;;

module Const_prop = struct
  module Base = struct
    include Unoptimized

    let cv s = const_value s

    let eqs s n =
      let d = Bits.( ==: ) (cv s) (Bits.of_int ~width:(width s) n) in
      Bits.to_int d = 1
    ;;

    let cst b = of_constant (Bits.to_constant b)

    let ( +: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( +: ) (cv a) (cv b))
      | true, false when eqs a 0 -> b (* 0+b *)
      | false, true when eqs b 0 -> a (* a+0 *)
      | _ -> a +: b
    ;;

    let ( -: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( -: ) (cv a) (cv b))
      (* | true, false when eqs a 0 -> b *)
      | false, true when eqs b 0 -> a (* a-0 *)
      | _ -> a -: b
    ;;

    let ( *: ) a b =
      let w = width a + width b in
      let opt d c =
        if eqs c 0
        then zero w
        else if eqs c 1
        then zero (width c) @: d
        else (
          let c = cv c in
          if Bits.to_int @@ Bits.popcount c <> 1
          then a *: b
          else (
            let p = Bits.to_int @@ (Bits.floor_log2 c).value in
            if p = 0 then uresize d w else uresize (d @: zero p) w))
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a *: b
    ;;

    let ( *+ ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( *+ ) (cv a) (cv b))
      (* | we could do certain optimisations here *)
      | _ -> a *+ b
    ;;

    let ( &: ) a b =
      let opt d c =
        if eqs c 0 then zero (width a) else if eqs c (-1) then d else a &: b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( &: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a &: b
    ;;

    let ( |: ) a b =
      let opt d c =
        if eqs c 0 then d else if eqs c (-1) then ones (width a) else a |: b
      in
      match is_const a, is_const b with
      | true, true -> cst (Bits.( |: ) (cv a) (cv b))
      | true, false -> opt b a
      | false, true -> opt a b
      | _ -> a |: b
    ;;

    let ( ^: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( ^: ) (cv a) (cv b))
      | _ -> a ^: b
    ;;

    let ( ==: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( ==: ) (cv a) (cv b))
      | _ -> a ==: b
    ;;

    let ( ~: ) a =
      match is_const a with
      | true -> cst (Bits.( ~: ) (cv a))
      | _ -> ~:a
    ;;

    let ( <: ) a b =
      match is_const a, is_const b with
      | true, true -> cst (Bits.( <: ) (cv a) (cv b))
      | _ -> a <: b
    ;;

    let concat_msb l =
      let optimise_consts l =
        List.group l ~break:(fun a b -> not (Bool.equal (is_const a) (is_const b)))
        |> List.map ~f:(function
             | [] -> []
             | [ x ] -> [ x ]
             | h :: _ as l ->
               if is_const h
               then [ List.map l ~f:const_value |> Bits.concat_msb |> cst ]
               else l)
        |> List.concat
      in
      concat_msb (optimise_consts l)
    ;;

    (* {[
         let is_rom els =
           List.fold (fun b s -> b && is_const s) true els

         let opt_rom sel els =
           let len = List.length els in
           let len' = 1 lsl (width sel) in
           let els =
             if len' <> len
             then
               let e = List.nth els (len'-1) in
               els @ linit (len'-len) (fun _ -> e)
             else
               els
           in
           mux sel els
       ]} *)

    let mux sel els =
      let len = List.length els in
      (*let len' = 1 lsl (width sel) in*)
      if is_const sel
      then (
        let x = Bits.to_int (cv sel) in
        let x = min x (len - 1) in
        (* clip select *)
        List.nth_exn els x
        (* {[
           else if is_rom els && len <= len'
             then
               opt_rom sel els
           ]} *))
      else mux sel els
    ;;

    let select d h l =
      if is_const d
      then cst (Bits.select (cv d) h l)
      else if l = 0 && h = width d - 1
      then d
      else select d h l
    ;;
  end

  module Comb = struct
    include Comb_make (Base)

    let is_vdd = function
      | Const { constant; _ } when Bits.equal constant Bits.vdd -> true
      | _ -> false
    ;;

    let is_gnd = function
      | Const { constant; _ } when Bits.equal constant Bits.gnd -> true
      | _ -> false
    ;;
  end
end

include (Const_prop.Comb : module type of Const_prop.Comb with type t := t)

(* error checking *)
let assert_width signal w msg =
  if width signal <> w
  then
    raise_s
      [%message
        msg ~info:"signal has unexpected width" ~expected_width:(w : int) (signal : t)]
;;

let assert_width_or_empty signal w msg =
  if (not (is_empty signal)) && width signal <> w
  then
    raise_s
      [%message
        msg
          ~info:"signal should have expected width or be empty"
          ~expected_width:(w : int)
          (signal : t)]
;;

let form_spec spec enable d =
  assert_width spec.reg_clock 1 "clock is invalid";
  assert_width_or_empty spec.reg_reset 1 "reset is invalid";
  assert_width_or_empty spec.reg_reset_value (width d) "reset value is invalid";
  assert_width_or_empty spec.reg_clear 1 "clear signal is invalid";
  assert_width_or_empty spec.reg_clear_value (width d) "clear value is invalid";
  assert_width_or_empty spec.reg_enable 1 "enable is invalid";
  assert_width_or_empty enable 1 "enable is invalid";
  { spec with
    reg_reset_value =
      (if is_empty spec.reg_reset_value then zero (width d) else spec.reg_reset_value)
  ; reg_clear_value =
      (if is_empty spec.reg_clear_value then zero (width d) else spec.reg_clear_value)
  ; reg_enable =
      (let e0 = if is_empty spec.reg_enable then vdd else spec.reg_enable in
       let e1 = if is_empty enable then vdd else enable in
       if is_vdd e0 && is_vdd e1
       then vdd
       else if is_vdd e0
       then e1
       else if is_vdd e1
       then e0
       else e0 &: e1)
  }
;;

let reg spec ?(enable = vdd) d =
  let spec = form_spec spec enable d in
  Reg { signal_id = make_id (width d); register = spec; d }
;;

let reg_fb ?enable spec ~width ~f =
  let d = wire width in
  let q = reg spec ?enable d in
  d <== f q;
  q
;;

let rec pipeline ?(attributes = []) spec ~n ?enable d =
  let maybe_add_attributes s = List.fold attributes ~init:s ~f:add_attribute in
  if n = 0
  then d
  else
    maybe_add_attributes
      (reg spec ?enable (pipeline ~attributes ~n:(n - 1) spec ?enable d))
;;

let multiport_memory
  ?name
  ?(attributes = [])
  size
  ~(write_ports : _ Write_port.t array)
  ~read_addresses
  =
  (* size > 0 *)
  if size <= 0
  then
    raise_s
      [%message "[Signal.multiport_memory] size must be greater than 0" (size : int)];
  let write_expected =
    if Array.is_empty write_ports
    then raise_s [%message "[Signal.multiport_memory] requires at least one write port"]
    else (
      let expected = write_ports.(0) in
      (* cannot address all elements of the memory *)
      if address_bits_for size <> width expected.write_address
      then
        raise_s
          [%message
            "[Signal.multiport_memory] size does not match what can be addressed by \
             write port"
              (size : int)
              ~address_width:(width expected.write_address : int)];
      Array.iteri write_ports ~f:(fun port write_port ->
        (* clocks must be 1 bit *)
        if width write_port.write_clock <> 1
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of clock must be 1"
                (port : int)
                ~write_enable_width:(width write_port.write_enable : int)];
        (* all write enables must be 1 bit *)
        if width write_port.write_enable <> 1
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of write enable must be 1"
                (port : int)
                ~write_enable_width:(width write_port.write_enable : int)];
        (* all write addresses must be the same width *)
        if width write_port.write_address <> width expected.write_address
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of write address is inconsistent"
                (port : int)
                ~write_address_width:(width write_port.write_address : int)
                ~expected:(width expected.write_address : int)];
        if width write_port.write_data <> width expected.write_data
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of write data is inconsistent"
                (port : int)
                ~write_data_width:(width write_port.write_data : int)
                ~expected:(width expected.write_data : int)]);
      expected)
  in
  let read_expected =
    if Array.is_empty read_addresses
    then raise_s [%message "[Signal.multiport_memory] requires at least one read port"]
    else (
      let expected = read_addresses.(0) in
      if address_bits_for size <> width expected
      then
        raise_s
          [%message
            "[Signal.multiport_memory] size does not match what can be addressed by read \
             port"
              (size : int)
              ~address_width:(width expected : int)];
      Array.iteri read_addresses ~f:(fun port read_address ->
        if width read_address <> width expected
        then
          raise_s
            [%message
              "[Signal.multiport_memory] width of read address is inconsistent"
                (port : int)
                ~read_address_width:(width read_address : int)
                ~expected:(width expected : int)]);
      expected)
  in
  if width write_expected.write_address <> width read_expected
  then
    raise_s
      [%message
        "[Signal.multiport_memory] width of read and write addresses differ"
          ~write_address_width:(width write_expected.write_address : int)
          ~read_address_width:(width read_expected : int)];
  let data_width = width write_expected.write_data in
  let memory =
    add_attributes
      (Multiport_mem { signal_id = make_id data_width; size; write_ports })
      attributes
  in
  Option.iter name ~f:(fun name -> ignore (memory -- name : t));
  Array.map read_addresses ~f:(fun read_address ->
    Mem_read_port { signal_id = make_id data_width; memory; read_address })
;;

let memory size ~write_port ~read_address =
  (multiport_memory size ~write_ports:[| write_port |] ~read_addresses:[| read_address |]).(
  0)
;;

let ram_rbw ?name ?attributes ~write_port ~(read_port : _ Read_port.t) size =
  let spec = { Reg_spec.reg_empty with reg_clock = read_port.read_clock } in
  reg
    spec
    ~enable:read_port.read_enable
    (multiport_memory
       ?name
       ?attributes
       size
       ~write_ports:[| write_port |]
       ~read_addresses:[| read_port.read_address |]).(0)
;;

let ram_wbr ?name ?attributes ~write_port ~(read_port : _ Read_port.t) size =
  let spec = { Reg_spec.reg_empty with reg_clock = read_port.read_clock } in
  (multiport_memory
     size
     ?name
     ?attributes
     ~write_ports:[| write_port |]
     ~read_addresses:[| reg spec ~enable:read_port.read_enable read_port.read_address |]).(
  0)
;;

(* Pretty printer *)
let pp fmt t = Stdlib.Format.fprintf fmt "%s" ([%sexp (t : t)] |> Sexp.to_string_hum)

module _ = Pretty_printer.Register (struct
  type nonrec t = t

  let module_name = "Hardcaml.Signal"
  let to_string = to_bstr
end)
