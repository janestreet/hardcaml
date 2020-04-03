open! Import
open! Signal_intf

module Signal_op = struct
  type t =
    | Signal_add
    | Signal_sub
    | Signal_mulu
    | Signal_muls
    | Signal_and
    | Signal_or
    | Signal_xor
    | Signal_eq
    | Signal_lt
  [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
end

type signal_op = Signal_op.t =
  | Signal_add
  | Signal_sub
  | Signal_mulu
  | Signal_muls
  | Signal_and
  | Signal_or
  | Signal_xor
  | Signal_eq
  | Signal_lt
[@@deriving sexp_of, compare, hash]

module Uid = struct
  module T = struct
    type t = int64 [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let equal = [%compare.equal: t]
end

module Uid_map = Map.Make (Uid)

module Uid_set = struct
  type t = Set.M(Uid).t [@@deriving sexp_of]

  let empty = Set.empty (module Uid)
end

type signal_id =
  { s_id : Uid.t
  ; mutable s_names : string list
  ; s_width : int
  ; mutable s_attributes : Rtl_attribute.t list
  ; mutable s_deps : t list
  ; caller_id : Caller_id.t option
  }

and t =
  | Empty
  | Const of
      { signal_id : signal_id
      ; constant : Bits.t
      }
  | Op2 of
      { signal_id : signal_id
      ; op : signal_op
      ; arg_a : t
      ; arg_b : t
      }
  | Mux of
      { signal_id : signal_id
      ; select : t
      ; cases : t list
      }
  | Cat of
      { signal_id : signal_id
      ; args : t list
      }
  | Not of
      { signal_id : signal_id
      ; arg : t
      }
  | Wire of
      { signal_id : signal_id
      ; driver : t ref
      }
  | Select of
      { signal_id : signal_id
      ; arg : t
      ; high : int
      ; low : int
      }
  | Reg of
      { signal_id : signal_id
      ; register : register
      ; d : t
      }
  | Mem of
      { signal_id : signal_id
      ; extra_uid : Uid.t
      ; register : register
      ; memory : memory
      }
  | Multiport_mem of
      { signal_id : signal_id
      ; size : int
      ; write_ports : write_port array
      }
  | Mem_read_port of
      { signal_id : signal_id
      ; memory : t
      ; read_address : t
      }
  | Inst of
      { signal_id : signal_id
      ; extra_uid : Uid.t
      ; instantiation : instantiation
      }

and write_port =
  { write_clock : t
  ; write_address : t
  ; write_enable : t
  ; write_data : t
  }

and read_port =
  { read_clock : t
  ; read_address : t
  ; read_enable : t
  }

(* These types are used to define a particular type of register as per the following
   template, where each part is optional:

   {v
       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_value;
         else if (clear == clear_level) d <= clear_value;
         else if (enable) d <= ...;
     v} *)
and register =
  { reg_clock : t
  ; reg_clock_edge : Edge.t
  ; reg_reset : t
  ; reg_reset_edge : Edge.t
  ; reg_reset_value : t
  ; reg_clear : t
  ; reg_clear_level : Level.t
  ; reg_clear_value : t
  ; reg_enable : t
  }

and memory =
  { mem_size : int
  ; mem_read_address : t
  ; mem_write_address : t
  ; mem_write_data : t
  }

and instantiation =
  { inst_name : string (* name of circuit *)
  ; inst_instance : string (* instantiation label *)
  ; inst_generics : Parameter.t list
  (* [ Parameter.create ~name:"ram_type" ~value:(String "auto") ] *)
  ; inst_inputs : (string * t) list (* name and input signal *)
  ; inst_outputs : (string * (int * int)) list (* name, width and low index of output *)
  ; inst_lib : string
  ; inst_arch : string
  }

let is_empty = function
  | Empty -> true
  | _ -> false
;;

let signal_id s =
  match s with
  | Empty -> None
  | Const { signal_id; _ }
  | Select { signal_id; _ }
  | Reg { signal_id; _ }
  | Mem { signal_id; _ }
  | Multiport_mem { signal_id; _ }
  | Mem_read_port { signal_id; _ }
  | Wire { signal_id; _ }
  | Inst { signal_id; _ }
  | Op2 { signal_id; _ }
  | Mux { signal_id; _ }
  | Cat { signal_id; _ }
  | Not { signal_id; _ } -> Some signal_id
;;

let signal_id_exn s =
  match signal_id s with
  | None -> raise_s [%message "Cannot get [signal_id] from empty signal"]
  | Some s -> s
;;

let uid s =
  match signal_id s with
  | None -> 0L
  | Some s -> s.s_id
;;

let deps s =
  match s with
  | Empty | Const _ -> []
  | Wire { driver; _ } -> [ !driver ]
  | Select _
  | Reg _
  | Mem _
  | Multiport_mem _
  | Mem_read_port _
  | Inst _
  | Op2 _
  | Not _
  | Cat _
  | Mux _ -> (signal_id_exn s).s_deps
;;

let add_attribute signal attribute =
  match signal_id signal with
  | None ->
    raise_s
      [%message
        "attempt to add attribute to an empty signal" ~to_:(attribute : Rtl_attribute.t)]
  | Some s ->
    s.s_attributes <- attribute :: s.s_attributes;
    signal
;;

let add_attributes signal attributes =
  List.iter attributes ~f:(fun x -> ignore (add_attribute signal x : t));
  signal
;;

let names s =
  match signal_id s with
  | None -> raise_s [%message "cannot get [names] from the empty signal"]
  | Some s -> s.s_names
;;

let attributes s =
  match signal_id s with
  | None -> raise_s [%message "cannot get [tag] from the empty signal"]
  | Some s -> s.s_attributes
;;

let has_name t = not (List.is_empty (names t))

let width s =
  match signal_id s with
  | None -> 0
  | Some s -> s.s_width
;;

let caller_id s =
  match signal_id s with
  | None -> None
  | Some s -> s.caller_id
;;

let is_reg = function
  | Reg _ -> true
  | _ -> false
;;

let is_const = function
  | Const _ -> true
  | _ -> false
;;

let is_select = function
  | Select _ -> true
  | _ -> false
;;

let is_wire = function
  | Wire _ -> true
  | _ -> false
;;

let is_op2 op = function
  | Op2 { op = o; _ } -> Signal_op.equal o op
  | _ -> false
;;

let is_cat = function
  | Cat _ -> true
  | _ -> false
;;

let is_mux = function
  | Mux _ -> true
  | _ -> false
;;

let is_not = function
  | Not _ -> true
  | _ -> false
;;

let is_mem = function
  | Mem _ | Multiport_mem _ -> true
  | _ -> false
;;

let is_multiport_mem = function
  | Multiport_mem _ -> true
  | _ -> false
;;

let is_inst = function
  | Inst _ -> true
  | _ -> false
;;

let is_mem_read_port = function
  | Mem_read_port _ -> true
  | _ -> false
;;

let new_id, reset_id =
  let id = ref 1L in
  let new_id () =
    let x = !id in
    id := Int64.add !id 1L;
    x
  in
  let reset_id () = id := 1L in
  new_id, reset_id
;;

let make_id w deps =
  { s_id = new_id ()
  ; s_names = []
  ; s_attributes = []
  ; s_width = w
  ; s_deps = deps
  ; caller_id = Caller_id.get () ~skip:[]
  }
;;

let string_of_op = function
  | Signal_add -> "add"
  | Signal_sub -> "sub"
  | Signal_mulu -> "mulu"
  | Signal_muls -> "muls"
  | Signal_and -> "and"
  | Signal_or -> "or"
  | Signal_xor -> "xor"
  | Signal_eq -> "eq"
  | Signal_lt -> "lt"
;;

let to_string signal =
  let names s =
    List.fold (names s) ~init:"" ~f:(fun a s ->
      if String.is_empty a then s else a ^ "," ^ s)
  in
  let deps s =
    List.fold (deps s) ~init:"" ~f:(fun a s ->
      let s = Int64.to_string (uid s) in
      if String.is_empty a then s else a ^ "," ^ s)
  in
  let sid s =
    "id:"
    ^ Int64.to_string (uid s)
    ^ " bits:"
    ^ Int.to_string (width s)
    ^ " names:"
    ^ names s
    ^ " deps:"
    ^ deps s
    ^ ""
  in
  match signal with
  | Empty -> "Empty"
  | Const { constant; _ } -> "Const[" ^ sid signal ^ "] = " ^ Bits.to_bstr constant
  | Op2 { op = o; _ } -> "Op[" ^ sid signal ^ "] = " ^ string_of_op o
  | Not _ -> "Op[" ^ sid signal ^ "] = " ^ "not"
  | Cat _ -> "Op[" ^ sid signal ^ "] = " ^ "cat"
  | Mux _ -> "Op[" ^ sid signal ^ "] = " ^ "mux"
  | Wire { driver; _ } -> "Wire[" ^ sid signal ^ "] -> " ^ Int64.to_string (uid !driver)
  | Select { high; low; _ } ->
    "Select[" ^ sid signal ^ "] " ^ Int.to_string high ^ ".." ^ Int.to_string low
  | Reg _ -> "Reg[" ^ sid signal ^ "]"
  | Mem _ -> "Mem[" ^ sid signal ^ "]"
  | Multiport_mem _ -> "Multiport_mem[" ^ sid signal ^ "]"
  | Mem_read_port _ -> "Mem_read_port[" ^ sid signal ^ "]"
  | Inst _ -> "Inst" ^ sid signal ^ "]"
;;

let structural_compare
      ?(check_names = true)
      ?(check_deps = true)
      ?(initial_deps = Set.empty (module Uid))
      a
      b
  =
  let rec structural_compare set a b =
    if Set.mem set (uid a)
    then set, true
    else (
      let set = Set.add set (uid a) in
      (* check we have the same type of node *)
      let typ () =
        match a, b with
        | Empty, Empty -> true
        | Const { constant = a; _ }, Const { constant = b; _ } -> Bits.equal a b
        | Select { high = h0; low = l0; _ }, Select { high = h1; low = l1; _ } ->
          h0 = h1 && l0 = l1
        | Reg _, Reg _ -> true
        | Mem { memory = m0; _ }, Mem { memory = m1; _ } -> m0.mem_size = m1.mem_size
        | Multiport_mem { size = mem_size0; _ }, Multiport_mem { size = mem_size1; _ } ->
          mem_size0 = mem_size1
        | Mem_read_port _, Mem_read_port _ -> true
        (* XXX check if inputs have same names ? *)
        | Wire _, Wire _ -> true
        | Inst { instantiation = i0; _ }, Inst { instantiation = i1; _ } ->
          String.equal i0.inst_name i1.inst_name
          (*i0.inst_instance=i1.inst_instance &&*)
          && [%compare.equal: Parameter.t list] i0.inst_generics i1.inst_generics
          && [%compare.equal: (string * (int * int)) list]
               i0.inst_outputs
               i1.inst_outputs
        (* inst_inputs=??? *)
        | Op2 { op = o0; _ }, Op2 { op = o1; _ } -> Signal_op.equal o0 o1
        | Not _, Not _ | Mux _, Mux _ | Cat _, Cat _ -> true
        | _ -> false
      in
      let wid () = width a = width b in
      let names () =
        let names_or_empty = function
          | Empty -> []
          | s -> names s
        in
        if check_names
        then [%compare.equal: string list] (names_or_empty a) (names_or_empty b)
        else true
      in
      let deps () =
        if check_deps
        then (
          match
            List.fold2 (deps a) (deps b) ~init:(set, true) ~f:(fun (set, b) x y ->
              if b
              then (
                let set, b' = structural_compare set x y in
                set, b && b')
              else set, b)
          with
          | Ok b -> b
          | Unequal_lengths -> set, false)
        else set, false
      in
      if typ () && wid () && names () then deps () else set, false)
  in
  structural_compare initial_deps a b
;;

let rec sexp_of_instantiation_recursive ?show_uids ~depth inst =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  let name =
    if String.is_empty inst.inst_lib
    then inst.inst_name
    else inst.inst_lib ^ "." ^ inst.inst_name
  in
  let name =
    if String.is_empty inst.inst_arch then name else name ^ "(" ^ inst.inst_arch ^ ")"
  in
  let sexp_of_output_width (w, _) = [%sexp (w : int)] in
  [%message
    name
      ~parameters:(inst.inst_generics : Parameter.t list)
      ~inputs:(inst.inst_inputs : (string * next) list)
      ~outputs:(inst.inst_outputs : (string * output_width) list)]

and sexp_of_register_recursive ?show_uids ~depth reg =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  let sexp_of_opt g s =
    match g with
    | Empty -> None
    | _ -> Some (sexp_of_next s)
  in
  let sexp_of_edge g s =
    match g with
    | Empty -> None
    | _ -> Some (Edge.sexp_of_t s)
  in
  let sexp_of_level g s =
    match g with
    | Empty -> None
    | _ -> Some (Level.sexp_of_t s)
  in
  [%message
    ""
      ~clock:(sexp_of_next reg.reg_clock : Sexp.t)
      ~clock_edge:(reg.reg_clock_edge : Edge.t)
      ~reset:(sexp_of_opt reg.reg_reset reg.reg_reset : (Sexp.t option[@sexp.option]))
      ~reset_edge:
        (sexp_of_edge reg.reg_reset reg.reg_reset_edge : (Sexp.t option[@sexp.option]))
      ~reset_to:
        (sexp_of_opt reg.reg_reset reg.reg_reset_value : (Sexp.t option[@sexp.option]))
      ~clear:(sexp_of_opt reg.reg_clear reg.reg_clear : (Sexp.t option[@sexp.option]))
      ~clear_level:
        (sexp_of_level reg.reg_clear reg.reg_clear_level : (Sexp.t option[@sexp.option]))
      ~clear_to:
        (sexp_of_opt reg.reg_clear reg.reg_clear_value : (Sexp.t option[@sexp.option]))
      ~enable:(sexp_of_opt reg.reg_enable reg.reg_enable : (Sexp.t option[@sexp.option]))]

and sexp_of_memory_recursive
      ?show_uids
      ~depth
      (size, write_address, read_address, write_enable)
  =
  let sexp_of_signal s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  [%message
    ""
      (size : int)
      (write_address : signal)
      (read_address : signal)
      (write_enable : signal)]

and sexp_of_multiport_memory_recursive ?show_uids ~depth (size, write_ports) =
  let sexp_of_signal s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  let sexp_of_write_port (w : write_port) =
    [%message
      ""
        ~write_enable:(w.write_enable : signal)
        ~write_address:(w.write_address : signal)
        ~write_data:(w.write_data : signal)]
  in
  [%message "" (size : int) (write_ports : write_port array)]

and sexp_of_mem_read_port_recursive ?show_uids ~depth (memory, read_addresses) =
  let sexp_of_signal s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  [%message "" (memory : signal) (read_addresses : signal)]

and sexp_of_signal_recursive ?(show_uids = false) ~depth signal =
  let display_const c =
    if Bits.width c <= 8
    then "0b" ^ Bits.to_bstr c
    else "0x" ^ (Bits.to_constant c |> Constant.to_hex_string ~signedness:Unsigned)
  in
  let tag =
    match signal with
    | Empty -> "empty"
    | Const _ -> "const"
    | Op2 { op; _ } -> string_of_op op
    | Mux _ -> "mux"
    | Not _ -> "not"
    | Cat _ -> "cat"
    | Wire _ -> "wire"
    | Select _ -> "select"
    | Inst _ -> "instantiation"
    | Reg _ -> "register"
    | Mem _ -> "memory"
    | Multiport_mem _ -> "multiport_memory"
    | Mem_read_port _ -> "memory_read_port"
  in
  if depth = 0 || is_empty signal
  then (
    match signal with
    | Empty -> [%message "empty"]
    | Const { constant; _ } -> [%sexp (display_const constant : string)]
    | _ ->
      (match names signal with
       | [] -> if show_uids then [%sexp (uid signal : Uid.t)] else [%sexp (tag : string)]
       | [ name ] -> [%sexp (name : string)]
       | names -> [%sexp (names : string list)]))
  else (
    let sexp_of_next = sexp_of_signal_recursive ~show_uids ~depth:(depth - 1) in
    let sexp_of_instantiation = sexp_of_instantiation_recursive ~show_uids ~depth in
    let sexp_of_memory = sexp_of_memory_recursive ~show_uids ~depth in
    let sexp_of_multiport_memory =
      sexp_of_multiport_memory_recursive ~show_uids ~depth
    in
    let sexp_of_mem_read_port = sexp_of_mem_read_port_recursive ~show_uids ~depth in
    let sexp_of_register = sexp_of_register_recursive ~show_uids ~depth in
    let uid = if show_uids then Some (uid signal) else None in
    let names =
      match names signal with
      | [] -> None
      | names -> Some names
    in
    let width = width signal in
    let loc = caller_id signal in
    let create
          ?value
          ?arguments
          ?select
          ?data
          ?range
          ?instantiation
          ?register
          ?memory
          ?multiport_memory
          ?mem_read_port
          ?data_in
          constructor
      =
      [%message
        constructor
          (uid : (Uid.t option[@sexp.option]))
          (names : (string list option[@sexp.option]))
          (loc : (Caller_id.t option[@sexp.option]))
          (width : int)
          (value : (string option[@sexp.option]))
          (range : ((int * int) option[@sexp.option]))
          (select : (next option[@sexp.option]))
          (data : (next list option[@sexp.option]))
          ~_:(instantiation : (instantiation option[@sexp.option]))
          ~_:(register : (register option[@sexp.option]))
          ~_:(memory : (memory option[@sexp.option]))
          ~_:(multiport_memory : (multiport_memory option[@sexp.option]))
          ~_:(mem_read_port : (mem_read_port option[@sexp.option]))
          (arguments : (next list option[@sexp.option]))
          (data_in : (next option[@sexp.option]))]
    in
    match signal with
    | Empty -> create "empty"
    | Const { constant; _ } -> create tag ~value:(display_const constant)
    | Mux _ ->
      (match deps signal with
       | select :: data -> create tag ~select ~data
       | deps -> create "MUX IS BADLY FORMED" ~arguments:deps)
    | Cat _ -> create tag ~arguments:(deps signal)
    | Not _ -> create tag ~arguments:(deps signal)
    | Op2 _ -> create tag ~arguments:(deps signal)
    | Wire { driver; _ } -> create tag ~data_in:!driver
    | Select { high; low; _ } ->
      (match deps signal with
       | [ data_in ] -> create tag ~data_in ~range:(high, low)
       | deps -> create "SELECT IS BADLY FORMED" ~arguments:deps)
    | Inst { instantiation; _ } -> create tag ~instantiation
    | Reg { register; _ } ->
      (match deps signal with
       | data :: _ -> create tag ~register ~data_in:data
       | deps -> create "REGISTER IS BADLY FORMED" ~arguments:deps)
    | Mem { register; memory; _ } ->
      create
        tag
        ~register
        ~data_in:memory.mem_write_data
        ~memory:
          ( memory.mem_size
          , memory.mem_write_address
          , memory.mem_read_address
          , register.reg_enable )
    | Multiport_mem { size = mem_size; write_ports; _ } ->
      create tag ~multiport_memory:(mem_size, write_ports)
    | Mem_read_port { memory; read_address; _ } ->
      create tag ~mem_read_port:(memory, read_address))
;;

let sexp_of_t s = sexp_of_signal_recursive ~show_uids:false ~depth:1 s
let sexp_of_register register = sexp_of_register_recursive register ~depth:1

type signal = t [@@deriving sexp_of]

let const_value =
  let sexp_of_signal = sexp_of_signal_recursive ~depth:1 in
  function
  | Const { constant; _ } -> constant
  | signal ->
    raise_s
      [%message "cannot get the value of a non-constant signal" ~_:(signal : signal)]
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

  let of_bits constant = Const { signal_id = make_id (Bits.width constant) []; constant }
  let of_constant data = of_bits (Bits.of_constant data)

  let to_constant signal =
    if is_const signal
    then Bits.to_constant (const_value signal)
    else
      raise_s
        [%message "cannot use [to_constant] on non-constant signal" ~_:(signal : t)]
  ;;

  let ( -- ) signal name =
    match signal_id signal with
    | None ->
      raise_s
        [%message "attempt to set the name of the empty signal" ~to_:(name : string)]
    | Some s ->
      s.s_names <- name :: s.s_names;
      signal
  ;;

  let op2 op len arg_a arg_b =
    Op2 { signal_id = make_id len [ arg_a; arg_b ]; op; arg_a; arg_b }
  ;;

  let concat_msb a =
    (* automatically concatenate successive constants *)
    let rec optimise_consts l =
      match l with
      | [] -> []
      | [ a ] -> [ a ]
      | a :: b :: tl ->
        if is_const a && is_const b
        then optimise_consts (of_bits Bits.(const_value a @: const_value b) :: tl)
        else a :: optimise_consts (b :: tl)
    in
    let a = optimise_consts a in
    match a with
    | [ a ] -> a
    | _ ->
      let len = List.fold a ~init:0 ~f:(fun acc a -> acc + width a) in
      Cat { signal_id = make_id len a; args = a }
  ;;

  let select arg high low =
    Select { signal_id = make_id (high - low + 1) [ arg ]; arg; high; low }
  ;;

  let ( +: ) a b = op2 Signal_add (width a) a b
  let ( -: ) a b = op2 Signal_sub (width a) a b
  let ( *: ) a b = op2 Signal_mulu (width a + width b) a b
  let ( *+ ) a b = op2 Signal_muls (width a + width b) a b
  let ( &: ) a b = op2 Signal_and (width a) a b
  let ( |: ) a b = op2 Signal_or (width a) a b
  let ( ^: ) a b = op2 Signal_xor (width a) a b
  let ( ~: ) a = Not { signal_id = make_id (width a) [ a ]; arg = a }
  let ( ==: ) a b = op2 Signal_eq 1 a b
  let ( <: ) a b = op2 Signal_lt 1 a b

  let mux select cases =
    Mux
      { signal_id = make_id (width (List.hd_exn cases)) (select :: cases)
      ; select
      ; cases
      }
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
  let wire = Wire { signal_id = make_id w []; driver = ref Empty } in
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

    let cst b = of_string (Bits.to_string b)

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
      let rec f l nl =
        match l with
        | [] -> List.rev nl
        | h :: t when is_const h ->
          (match nl with
           | h' :: t' when is_const h' -> f t (cst (Bits.concat_msb [ cv h'; cv h ]) :: t')
           | _ -> f t (h :: nl))
        | h :: t -> f t (h :: nl)
      in
      concat_msb (f l [])
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

let reg_empty =
  { reg_clock = empty
  ; reg_clock_edge = Rising
  ; reg_reset = empty
  ; reg_reset_edge = Rising
  ; reg_reset_value = empty
  ; reg_clear = empty
  ; reg_clear_level = High
  ; reg_clear_value = empty
  ; reg_enable = empty
  }
;;

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

module Reg_spec_ = struct
  type t = register [@@deriving sexp_of]

  let override
        ?clock
        ?clock_edge
        ?reset
        ?reset_edge
        ?reset_to
        ?clear
        ?clear_level
        ?clear_to
        ?global_enable
        spec
    =
    { reg_clock = Option.value clock ~default:spec.reg_clock
    ; reg_clock_edge = Option.value clock_edge ~default:spec.reg_clock_edge
    ; reg_reset = Option.value reset ~default:spec.reg_reset
    ; reg_reset_edge = Option.value reset_edge ~default:spec.reg_reset_edge
    ; reg_reset_value = Option.value reset_to ~default:spec.reg_reset_value
    ; reg_clear = Option.value clear ~default:spec.reg_clear
    ; reg_clear_level = Option.value clear_level ~default:spec.reg_clear_level
    ; reg_clear_value = Option.value clear_to ~default:spec.reg_clear_value
    ; reg_enable = Option.value global_enable ~default:spec.reg_enable
    }
  ;;

  let create ?clear ?reset () ~clock =
    let spec =
      match clear, reset with
      | None, None -> reg_empty
      | None, Some reset -> { reg_empty with reg_reset = reset }
      | Some clear, None -> { reg_empty with reg_clear = clear }
      | Some clear, Some reset -> { reg_empty with reg_reset = reset; reg_clear = clear }
    in
    { spec with reg_clock = clock }
  ;;

  let clock spec = spec.reg_clock
  let clear spec = spec.reg_clear
  let reset spec = spec.reg_reset
end

let reg spec ~enable d =
  let spec = form_spec spec enable d in
  let deps =
    [ spec.reg_clock
    ; spec.reg_reset
    ; spec.reg_reset_value
    ; spec.reg_clear
    ; spec.reg_clear_value
    ; spec.reg_enable
    ]
  in
  Reg { signal_id = make_id (width d) (d :: deps); register = spec; d }
;;

let reg_fb spec ~enable ~w:width f =
  let d = wire width in
  let q = reg spec ~enable d in
  d <== f q;
  q
;;

let rec pipeline spec ~n ~enable d =
  if n = 0 then d else reg spec ~enable (pipeline ~n:(n - 1) spec ~enable d)
;;

let memory size ~write_port ~read_address =
  let we = write_port.write_enable in
  let wa = write_port.write_address in
  let d = write_port.write_data in
  if width write_port.write_enable <> 1
  then
    raise_s
      [%message
        "[Signal.memory] width of write enable must be 1"
          ~write_enable_width:(width we : int)];
  if size <= 0
  then raise_s [%message "[Signal.memory] size must be greater than 0" (size : int)];
  if width read_address <> width wa
  then
    raise_s
      [%message
        "[Signal.memory] width of read and write addresses differ"
          ~write_address_width:(width wa : int)
          ~read_address_width:(width read_address : int)];
  if size > 1 lsl width write_port.write_address
  then
    raise_s
      [%message
        "[Signal.memory] size greater than what can be addressed"
          (size : int)
          ~address_width:(width wa : int)];
  let spec =
    form_spec
      { reg_empty with reg_clock = write_port.write_clock }
      write_port.write_enable
      write_port.write_data
  in
  let deps =
    [ d
    ; write_port.write_address
    ; read_address
    ; write_port.write_enable
    ; spec.reg_clock
    ; spec.reg_reset
    ; spec.reg_reset_value
    ; spec.reg_clear
    ; spec.reg_clear_value
    ; spec.reg_enable
    ]
  in
  Mem
    { signal_id = make_id (width d) deps
    ; extra_uid = new_id ()
    ; register = spec
    ; memory =
        { mem_size = size
        ; mem_write_address = wa
        ; mem_read_address = read_address
        ; mem_write_data = d
        }
    }
;;

let multiport_memory ?name ?(attributes = []) size ~write_ports ~read_addresses =
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
      if size > 1 lsl width expected.write_address
      then
        raise_s
          [%message
            "[Signal.multiport_memory] size is greater than what can be addressed by \
             write port"
              (size : int)
              ~address_width:(width expected.write_address : int)];
      Array.iteri write_ports ~f:(fun port (write_port : write_port) ->
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
      if size > 1 lsl width expected
      then
        raise_s
          [%message
            "[Signal.multiport_memory] size is greater than what can be addressed by \
             read port"
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
  let deps =
    Array.to_list read_addresses
    :: List.map (Array.to_list write_ports) ~f:(fun (w : write_port) ->
      let { write_clock; write_address; write_data; write_enable } = w in
      [ write_clock; write_address; write_data; write_enable ])
    |> List.concat
  in
  let mem =
    add_attributes
      (Multiport_mem { signal_id = make_id data_width deps; size; write_ports })
      attributes
  in
  Option.iter name ~f:(fun name -> ignore (mem -- name : t));
  Array.map read_addresses ~f:(fun r ->
    Mem_read_port
      { signal_id = make_id data_width [ r; mem ]; memory = mem; read_address = r })
;;

let ram_rbw ?attributes ~write_port ~read_port size =
  let spec = { reg_empty with reg_clock = read_port.read_clock } in
  reg
    spec
    ~enable:read_port.read_enable
    (multiport_memory
       ?attributes
       size
       ~write_ports:[| write_port |]
       ~read_addresses:[| read_port.read_address |]).(0)
;;

let ram_wbr ?attributes ~write_port ~read_port size =
  let spec = { reg_empty with reg_clock = read_port.read_clock } in
  (multiport_memory
     size
     ?attributes
     ~write_ports:[| write_port |]
     ~read_addresses:[| reg spec ~enable:read_port.read_enable read_port.read_address |]).(
    0)
;;

(* Pretty printer *)
let pp fmt t = Caml.Format.fprintf fmt "%s" ([%sexp (t : t)] |> Sexp.to_string_hum)

module PP = Pretty_printer.Register (struct
    type nonrec t = t

    let module_name = "Hardcaml.Signal"
    let to_string = to_bstr
  end)
