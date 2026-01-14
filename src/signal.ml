open! Core0
module Type = Signal__type

type t = Type.t

module type S = Signal_intf.S

open Type

let names = Type.names
let names_and_locs = Type.names_and_locs
let%template uid = (Type.uid [@mode m]) [@@mode m = (local, global)]
let to_rep t = t, ()
let from_rep t () = t
let update_rep t ~info:() = t

let add_attribute signal attribute =
  match info signal with
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
  match info signal with
  | None ->
    raise_s [%message "attempt to add comment to an empty signal" ~to_:(comment : string)]
  | Some _ ->
    Type.set_comment signal comment;
    signal
;;

let unset_comment signal =
  match info signal with
  | None -> raise_s [%message "attempt to remove comment from an empty signal"]
  | Some _ ->
    Type.unset_comment signal;
    signal
;;

let set_names s names = Type.set_names s names

let attributes s =
  match info s with
  | None -> []
  | Some _ -> Type.get_attributes s
;;

let comment s =
  match info s with
  | None -> raise_s [%message "cannot get [comment] from the empty signal"]
  | Some _ -> Type.get_comment s
;;

let raise_invalid_waiver t ~expected_kind =
  let msg =
    [%string
      {|Trying to add a %{expected_kind} waiver to a signal that isn't a %{expected_kind}|}]
  in
  raise_s [%message msg (t : t)]
;;

let add_mux_waiver_exn t waiver =
  match t with
  | Mux _ ->
    Type.update_coverage_metadata t ~f:(fun m ->
      Coverage_metadata.add_mux_waiver_exn m waiver);
    t
  | _ -> raise_invalid_waiver t ~expected_kind:"mux"
;;

let add_cases_waiver_exn t waiver =
  match t with
  | Cases _ ->
    Type.update_coverage_metadata t ~f:(fun m ->
      Coverage_metadata.add_cases_waiver_exn m waiver);
    t
  | _ -> raise_invalid_waiver t ~expected_kind:"cases"
;;

let is_always_state_register t =
  match Type.coverage_metadata t with
  | Some { kind = Some (Variable (State_machine_state _)); _ } -> true
  | _ -> false
;;

let add_register_waiver_exn t waiver =
  match t with
  | Reg _ ->
    if is_always_state_register t
    then
      raise_s
        [%message
          "Trying to add a toggle waiver to an always state register. Use \
           [add_always_state_waiver_exn]"];
    Type.update_coverage_metadata t ~f:(fun m ->
      Coverage_metadata.add_register_waiver_exn m waiver);
    t
  | _ -> raise_invalid_waiver t ~expected_kind:"register"
;;

let add_always_state_waiver_exn t waiver =
  match t with
  | Reg _ ->
    if not (is_always_state_register t)
    then
      raise_s
        [%message
          "Trying to add an always state waiver to a plain register. Use \
           [add_register_waiver_exn]"];
    Type.update_coverage_metadata t ~f:(fun m ->
      Coverage_metadata.add_always_state_waiver_exn m waiver);
    t
  | _ -> raise_invalid_waiver t ~expected_kind:"register"
;;

let add_always_state_transition_waiver_exn t waiver =
  match t with
  | Reg _ ->
    if not (is_always_state_register t)
    then
      raise_s
        [%message
          "Trying to add a always state waiver to plain register. Use \
           [add_register_waiver_exn]"];
    Type.update_coverage_metadata t ~f:(fun m ->
      Coverage_metadata.add_always_state_transition_waiver_exn m waiver);
    t
  | _ -> raise_invalid_waiver t ~expected_kind:"register"
;;

let set_wave_format = Type.set_wave_format

let ( --$ ) s w =
  Type.set_wave_format s w;
  s
;;

module Base = struct
  (* TODO: instantiations, memories *)

  type nonrec t = t

  let%template[@mode local] equal (t1 @ local) (t2 @ local) =
    match t1, t2 with
    | Empty, Empty -> true
    | Const { constant = c1; _ }, Const { constant = c2; _ } ->
      (Bits.equal [@mode local]) c1 c2
    | _ ->
      (Uid.equal [@mode local])
        ((uid [@mode local]) t1)
        ((uid [@mode local]) t2) [@nontail]
  ;;

  let%template equal = [%eta2 equal [@mode local]]
  let width = width
  let to_string = to_string
  let sexp_of_t = sexp_of_t
  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let of_bits = Type.of_bits
  let of_constant data = of_bits (Bits.of_constant data)

  let to_constant signal =
    if is_const signal
    then Bits.to_constant (const_value signal)
    else
      raise_s [%message "cannot use [to_constant] on non-constant signal" ~_:(signal : t)]
  ;;

  let ( -- ) ~(loc : [%call_pos]) signal name =
    match info signal with
    | None ->
      raise_s
        [%message "attempt to set the name of the empty signal" ~to_:(name : string)]
    | Some _ ->
      Type.add_name signal { name; loc };
      signal
  ;;

  let vdd = of_constant (Constant.of_int ~width:1 1) -- "vdd"
  let gnd = of_constant (Constant.of_int ~width:1 0) -- "gnd"
  let op2 op len arg_a arg_b = Op2 { info = make_id len; op; arg_a; arg_b }

  let concat_msb a =
    match a with
    | [ a ] -> a
    | _ ->
      let len = List.fold a ~init:0 ~f:(fun acc a -> acc + width a) in
      Cat { info = make_id len; args = a }
  ;;

  let select arg ~high ~low = Select { info = make_id (high - low + 1); arg; high; low }
  let ( +: ) a b = op2 Add (width a) a b
  let ( -: ) a b = op2 Sub (width a) a b
  let ( *: ) a b = op2 Mulu (width a + width b) a b
  let ( *+ ) a b = op2 Muls (width a + width b) a b
  let ( &: ) a b = op2 And (width a) a b
  let ( |: ) a b = op2 Or (width a) a b
  let ( ^: ) a b = op2 Xor (width a) a b
  let ( ~: ) a = Not { info = make_id (width a); arg = a }
  let ( ==: ) a b = op2 Eq 1 a b
  let ( <: ) a b = op2 Lt 1 a b

  let mux select cases =
    (* We are a bit more lax about this in [Comb], but RTL generation requires 2 cases so
       ensure it here. *)
    if List.length cases < 2
    then raise_s [%message "[Signal.mux] requires a minimum of 2 cases"];
    match cases with
    | first_case :: _ -> Mux { info = make_id (width first_case); select; cases }
    | [] -> raise_s [%message "Mux with no cases"]
  ;;

  let cases ~default select cases =
    List.iter cases ~f:(fun (match_value, _) ->
      if not (is_const match_value)
      then raise_s [%message "[cases] the match value must be a constant."]);
    match cases with
    | (_, first_case) :: _ ->
      Cases { info = make_id (width first_case); select; cases; default }
    | [] -> raise_s [%message "[cases] no cases specified"]
  ;;
end

module Unoptimized = Comb.Make (Base)

let assign a b =
  match a with
  | Wire { driver = Some _; _ } ->
    raise_s
      [%message
        "attempt to assign wire multiple times"
          ~already_assigned_wire:(a : t)
          ~expression:(b : t)]
  | Wire ({ driver = None; _ } as w) ->
    if width a <> width b
    then
      raise_s
        [%message
          "attempt to assign expression to wire of different width"
            ~wire_width:(width a : int)
            ~expression_width:(width b : int)
            ~wire:(a : t)
            ~expression:(b : t)];
    w.driver <- Some b
  | _ ->
    raise_s
      [%message
        "attempt to assign non-wire" ~assignment_target:(a : t) ~expression:(b : t)]
;;

let ( <-- ) = assign

let wire w =
  let wire = Wire { info = make_id w; driver = None } in
  wire
;;

let wireof s =
  let x = wire (width s) in
  x <-- s;
  x
;;

let input name width = Unoptimized.( -- ) (wire width) name

let output name s =
  let w = Unoptimized.( -- ) (wire (width s)) name in
  w <-- s;
  w
;;

module Optimized : Comb.S with type t = t = Signal_builders.Const_prop (struct
    include Unoptimized

    let is_const = Type.is_const
    let const_value = Type.const_value
  end)

include (Optimized : Comb.S with type t := t)
include Signal_builders.Conversion_functions (Optimized)

let reset_names () =
  set_names vdd [ { name = "vdd"; loc = [%here] } ];
  set_names gnd [ { name = "gnd"; loc = [%here] } ]
;;

module Reg_spec_ = Reg_spec.Make (struct
    type nonrec t = t [@@deriving sexp_of]

    let is_empty = Signal__type.is_empty
  end)

let to_signal_type_reg_spec (spec : Reg_spec_.t) =
  { Signal__type.clock = Reg_spec_.clock spec
  ; clock_edge = Reg_spec_.clock_edge spec
  ; reset = Reg_spec_.reset spec
  ; reset_edge = Reg_spec_.reset_edge spec
  ; clear = Reg_spec_.clear spec
  }
;;

module Reg_spec = struct
  include Reg_spec_

  let sexp_of_t t = Signal__type.sexp_of_reg_spec (to_signal_type_reg_spec t)
end

type 'a with_register_spec =
  ?enable:t
  -> ?initialize_to:Bits.t
  -> ?reset_to:Bits.t
  -> ?clear:t
  -> ?clear_to:t
  -> Reg_spec.t
  -> 'a

let reg__with_signal_reset ?enable ?initialize_to ?reset_to ?clear ?clear_to spec d =
  (* if width d = 0 then raise_s [%message "[Signal.reg] width of data input is 0"]; *)
  let spec =
    Type.Register.of_reg_spec
      (to_signal_type_reg_spec spec)
      ~enable
      ~initialize_to
      ~reset_to
      ~clear_to
      ~clear
      d
  in
  Reg { info = make_id (width d); register = spec; d }
;;

include Signal_builders.Registers (struct
    type info = unit

    include Optimized
    module Reg_spec = Reg_spec

    let add_attribute = add_attribute
    let reg__with_signal_reset = reg__with_signal_reset
    let wire = wire
    let assign = assign
    let update_rep = update_rep
    let to_rep = to_rep
  end)

module Memory_prim = struct
  let multiport_memory_prim
    ?name
    ?(attributes = [])
    ?initialize_to
    size
    ~remove_unused_write_ports
    ~data_width
    ~(write_ports : _ Write_port.t array)
    ~read_addresses
    =
    let write_ports =
      if remove_unused_write_ports
      then
        Array.filter write_ports ~f:(fun { write_enable; _ } -> Type.is_gnd write_enable)
      else write_ports
    in
    let memory =
      add_attributes
        (Multiport_mem
           { info = make_id data_width
           ; size
           ; write_ports
           ; initialize_to = Option.map initialize_to ~f:Array.copy
           })
        attributes
    in
    Option.iter name ~f:(fun name -> ignore (memory -- name : t));
    Array.map read_addresses ~f:(fun read_address ->
      Mem_read_port { info = make_id data_width; memory; read_address })
  ;;
end

include Signal_builders.Memories (struct
    include Optimized
    include Memory_prim
    module Reg_spec = Reg_spec

    let reg = reg
  end)

include Signal__type.Make_default_info (struct
    type nonrec t = t
  end)

let __ppx_auto_name = ( -- )

(* Pretty printer *)
let pp fmt t = Stdlib.Format.fprintf fmt "%s" ([%sexp (t : t)] |> Sexp.to_string_hum)

module Expert = struct
  include Memory_prim

  let reg__with_signal_reset = reg__with_signal_reset
end

module (* Install pretty printer in top level *) _ = Pretty_printer.Register (struct
    type nonrec t = t

    let module_name = "Hardcaml.Signal"
    let to_string = to_bstr
  end)

(* For tests - we have a mode in ppx_hardcaml0 which is supposed to work without linking
   Hardcaml. We can use this value in tests to tell one way of the other - it will become
   true if hardcaml is linked. *)
let () = Ppx_hardcaml_runtime0.hardcaml_is_linked := true
