open Base

module type Uid = Signal__type_intf.Uid
module type Type = Signal__type_intf.Type
module type Uid_set = Signal__type_intf.Uid_set
module type Uid_map = Signal__type_intf.Uid_map

module Uid = struct
  module I = Int

  module T = struct
    type t = I.t [@@deriving compare, sexp_of]

    (* We need a hash function compatible with native code and javascript. Currently the
       only type which allows this is [Int64]. So we perform a conversion to int64 here,
       and reach out directly to the (unboxed) hash_fold function in base to implement it.
       This allows zero alloc operation even in fast-build mode. *)

    external fold_int64
      :  Base.Hash.state
      -> (int64[@unboxed])
      -> Base.Hash.state
      = "Base_internalhash_fold_int64" "Base_internalhash_fold_int64_unboxed"
    [@@noalloc]

    let hash_fold_t state t = fold_int64 state (Int64.of_int_exn t)
    let hash t = Hash.get_hash_value (hash_fold_t (Hash.alloc ()) t)
  end

  include T
  include Comparator.Make (T)

  let zero = I.of_int 0
  let one = I.of_int 1
  let equal = [%compare.equal: t]
  let to_int t = I.to_int_exn t
  let to_string t = I.to_string t

  let generator () =
    let id = ref one in
    let new_id () =
      let x = !id in
      (id := I.(!id + one));
      x
    in
    let reset_id () = id := one in
    `New new_id, `Reset reset_id
  ;;
end

type signal_op =
  | Signal_add
  | Signal_sub
  | Signal_mulu
  | Signal_muls
  | Signal_and
  | Signal_or
  | Signal_xor
  | Signal_eq
  | Signal_lt
[@@deriving sexp_of, compare, equal, hash]

type signal_metadata =
  { mutable names : string list
  ; mutable attributes : Rtl_attribute.t list
  ; mutable comment : string option
  ; mutable caller_id : Caller_id.t option
  ; mutable wave_format : Wave_format.t
  }

type signal_id =
  { s_id : Uid.t
  ; s_width : int
  ; mutable s_metadata : signal_metadata option
  }

type t =
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
      ; mutable driver : t option
      }
  | Select of
      { signal_id : signal_id
      ; arg : t
      ; high : int
      ; low : int
      }
  | Reg of
      { signal_id : signal_id
      ; register : t register
      ; d : t
      }
  | Multiport_mem of
      { signal_id : signal_id
      ; size : int
      ; write_ports : t Write_port.t array
      ; initialize_to : Bits.t array option
      }
  | Mem_read_port of
      { signal_id : signal_id
      ; memory : t
      ; read_address : t
      }
  | Inst of
      { signal_id : signal_id
      ; instantiation : instantiation
      }

(* These types are used to define a particular type of register as per the following
   template, where each part is optional:

   {v
       reg [7:0] d = initialize_to;

       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_to;
         else if (clear) d <= clear_to;
         else if (enable) d <= ...;
     v} *)
and reg_spec =
  { clock : t
  ; clock_edge : Edge.t
  ; reset : t option
  ; reset_edge : Edge.t
  ; clear : t option
  }

and 'a clock_spec =
  { clock : 'a
  ; clock_edge : Edge.t
  }

and 'a reset_spec =
  { reset : 'a
  ; reset_edge : Edge.t
  ; reset_to : 'a
  }

and 'a clear_spec =
  { clear : 'a
  ; clear_to : 'a
  }

and 'a register =
  { clock : 'a clock_spec
  ; reset : 'a reset_spec option
  ; clear : 'a clear_spec option
  ; enable : 'a option
  ; initialize_to : 'a option
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

module Uid_map = Map.M (Uid)

module Uid_set = struct
  type t = Set.M(Uid).t [@@deriving sexp_of]

  let empty = Set.empty (module Uid)
end

module type Printable =
  Signal__type_intf.Printable
  with type t := t
   and type register := t register
   and type reg_spec := reg_spec

module type Is_a = Signal__type_intf.Is_a with type t := t and type signal_op := signal_op
module type Deps = Signal__type_intf.Deps with type t := t

module Make_deps (Fold : sig
    val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
  end) =
struct
  open Fold

  let fold = fold
  let iter t ~f = fold t ~init:() ~f:(fun () s -> f s)
  let rev_map t ~f = fold t ~init:[] ~f:(fun lst s -> f s :: lst)
  let map t ~f = List.rev (rev_map t ~f)
  let to_list t = map t ~f:Fn.id
end

module Deps = Make_deps (struct
    let fold t ~init ~f =
      match t with
      | Empty | Const _ -> init
      | Wire { driver; _ } -> Option.value_map driver ~default:init ~f:(f init)
      | Select { arg; _ } -> f init arg
      | Reg
          { register =
              { clock = { clock; clock_edge = _ }; reset; clear; enable; initialize_to }
          ; d
          ; _
          } ->
        let arg = f init d in
        let arg = f arg clock in
        let arg =
          Option.value_map
            ~default:arg
            reset
            ~f:(fun { reset; reset_edge = _; reset_to } ->
              let arg = f arg reset in
              f arg reset_to)
        in
        let arg =
          Option.value_map ~default:arg clear ~f:(fun { clear; clear_to } ->
            let arg = f arg clear in
            f arg clear_to)
        in
        let arg = Option.value_map ~default:arg initialize_to ~f:(f arg) in
        let arg = Option.value_map ~default:arg enable ~f:(f arg) in
        arg
      | Multiport_mem { write_ports; _ } ->
        Array.fold
          write_ports
          ~init
          ~f:(fun arg { write_clock; write_address; write_data; write_enable } ->
            let arg = f arg write_clock in
            let arg = f arg write_address in
            let arg = f arg write_data in
            let arg = f arg write_enable in
            arg)
      | Mem_read_port { memory; read_address; _ } ->
        let arg = f init read_address in
        let arg = f arg memory in
        arg
      | Inst { instantiation = { inst_inputs; _ }; _ } ->
        List.fold ~init inst_inputs ~f:(fun arg (_, s) -> f arg s)
      | Op2 { arg_a; arg_b; _ } ->
        let arg = f init arg_a in
        let arg = f arg arg_b in
        arg
      | Not { arg; _ } -> f init arg
      | Cat { args; _ } -> List.fold ~init args ~f
      | Mux { select; cases; _ } ->
        let arg = f init select in
        let arg = List.fold ~init:arg cases ~f in
        arg
    ;;
  end)

let signal_id s =
  match s with
  | Empty -> None
  | Const { signal_id; _ }
  | Select { signal_id; _ }
  | Reg { signal_id; _ }
  | Multiport_mem { signal_id; _ }
  | Mem_read_port { signal_id; _ }
  | Wire { signal_id; _ }
  | Inst { signal_id; _ }
  | Op2 { signal_id; _ }
  | Mux { signal_id; _ }
  | Cat { signal_id; _ }
  | Not { signal_id; _ } -> Some signal_id
;;

let uid s =
  match s with
  | Empty -> Uid.zero
  | Const { signal_id; _ }
  | Select { signal_id; _ }
  | Reg { signal_id; _ }
  | Multiport_mem { signal_id; _ }
  | Mem_read_port { signal_id; _ }
  | Wire { signal_id; _ }
  | Inst { signal_id; _ }
  | Op2 { signal_id; _ }
  | Mux { signal_id; _ }
  | Cat { signal_id; _ }
  | Not { signal_id; _ } -> signal_id.s_id
;;

let width s =
  match signal_id s with
  | None -> 0
  | Some s -> s.s_width
;;

let caller_id s =
  match signal_id s with
  | None -> None
  | Some s ->
    (match s.s_metadata with
     | None -> None
     | Some id -> id.caller_id)
;;

let names s =
  match signal_id s with
  | None -> raise_s [%message "cannot get [names] from the empty signal"]
  | Some s ->
    Option.value_map ~default:[] s.s_metadata ~f:(fun metadata -> metadata.names)
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
        | Multiport_mem { size = mem_size0; _ }, Multiport_mem { size = mem_size1; _ } ->
          mem_size0 = mem_size1
        | Mem_read_port _, Mem_read_port _ -> true
        | Wire _, Wire _ -> true
        | Inst { instantiation = i0; _ }, Inst { instantiation = i1; _ } ->
          String.equal i0.inst_name i1.inst_name
          (*i0.inst_instance=i1.inst_instance &&*)
          && [%equal: Parameter.t list] i0.inst_generics i1.inst_generics
          && [%equal: (string * (int * int)) list] i0.inst_outputs i1.inst_outputs
        (* inst_inputs=??? *)
        | Op2 { op = o0; _ }, Op2 { op = o1; _ } -> equal_signal_op o0 o1
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
            List.fold2
              (Deps.to_list a)
              (Deps.to_list b)
              ~init:(set, true)
              ~f:(fun (set, b) x y ->
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

let is_empty = function
  | Empty -> true
  | _ -> false
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
  | Op2 { op = o; _ } -> equal_signal_op o op
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
  let name = name ^ "{" ^ inst.inst_instance ^ "}" in
  let sexp_of_output_width (w, _) = [%sexp (w : int)] in
  [%message
    name
      ~parameters:(inst.inst_generics : Parameter.t list)
      ~inputs:(inst.inst_inputs : (string * next) list)
      ~outputs:(inst.inst_outputs : (string * output_width) list)]

and sexp_of_register_recursive ?show_uids ~depth (reg : t register) =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  let sexp_of_opt s ~f = Option.map s ~f:(fun s -> sexp_of_next (f s)) in
  [%message
    ""
      ~clock:(sexp_of_next reg.clock.clock : Sexp.t)
      ~clock_edge:(reg.clock.clock_edge : Edge.t)
      ~initialize_to:
        (sexp_of_opt reg.initialize_to ~f:Fn.id : (Sexp.t option[@sexp.option]))
      ~reset:
        (sexp_of_opt reg.reset ~f:(fun { reset; _ } -> reset)
         : (Sexp.t option[@sexp.option]))
      ~reset_edge:
        (Option.map reg.reset ~f:(fun { reset_edge; _ } -> reset_edge)
         : (Edge.t option[@sexp.option]))
      ~reset_to:
        (sexp_of_opt reg.reset ~f:(fun { reset_to; _ } -> reset_to)
         : (Sexp.t option[@sexp.option]))
      ~clear:
        (sexp_of_opt reg.clear ~f:(fun { clear; _ } -> clear)
         : (Sexp.t option[@sexp.option]))
      ~clear_to:
        (sexp_of_opt reg.clear ~f:(fun { clear_to; _ } -> clear_to)
         : (Sexp.t option[@sexp.option]))
      ~enable:(sexp_of_opt reg.enable ~f:Fn.id : (Sexp.t option[@sexp.option]))]

and sexp_of_reg_spec_recursive ?show_uids ~depth spec =
  let sexp_of_next s = sexp_of_signal_recursive ?show_uids ~depth:(depth - 1) s in
  let sexp_of_opt s = Option.map s ~f:(fun s -> sexp_of_next s) in
  let sexp_of_edge g s = Option.map g ~f:(fun _ -> Edge.sexp_of_t s) in
  [%message
    ""
      ~clock:(sexp_of_next spec.clock : Sexp.t)
      ~clock_edge:(spec.clock_edge : Edge.t)
      ~reset:(sexp_of_opt spec.reset : (Sexp.t option[@sexp.option]))
      ~reset_edge:
        (sexp_of_edge spec.reset spec.reset_edge : (Sexp.t option[@sexp.option]))
      ~clear:(sexp_of_opt spec.clear : (Sexp.t option[@sexp.option]))]

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
  let sexp_of_write_port (w : _ Write_port.t) =
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
    let sexp_of_multiport_memory = sexp_of_multiport_memory_recursive ~show_uids ~depth in
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
    | Mux { select; cases; _ } -> create tag ~select ~data:cases
    | Cat { args; _ } -> create tag ~arguments:args
    | Not { arg; _ } -> create tag ~arguments:[ arg ]
    | Op2 { arg_a; arg_b; _ } -> create tag ~arguments:[ arg_a; arg_b ]
    | Wire { driver; _ } -> create tag ?data_in:driver
    | Select { arg; high; low; _ } -> create tag ~data_in:arg ~range:(high, low)
    | Inst { instantiation; _ } -> create tag ~instantiation
    | Reg { register; d; _ } -> create tag ~register ~data_in:d
    | Multiport_mem { size = mem_size; write_ports; _ } ->
      create tag ~multiport_memory:(mem_size, write_ports)
    | Mem_read_port { memory; read_address; _ } ->
      create tag ~mem_read_port:(memory, read_address))
;;

let sexp_of_t s = sexp_of_signal_recursive ~show_uids:false ~depth:1 s
let sexp_of_register register = sexp_of_register_recursive register ~depth:1
let sexp_of_reg_spec register = sexp_of_reg_spec_recursive register ~depth:1

let to_string signal =
  let names s =
    List.fold (names s) ~init:"" ~f:(fun a s ->
      if String.is_empty a then s else a ^ "," ^ s)
  in
  let deps s =
    Deps.fold s ~init:"" ~f:(fun a s ->
      let s = Uid.to_string (uid s) in
      if String.is_empty a then s else a ^ "," ^ s)
  in
  let sid s =
    "id:"
    ^ Uid.to_string (uid s)
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
  | Wire { driver; _ } ->
    "Wire["
    ^ sid signal
    ^ "] -> "
    ^ Option.value_map driver ~default:"()" ~f:(fun driver -> Uid.to_string (uid driver))
  | Select { high; low; _ } ->
    "Select[" ^ sid signal ^ "] " ^ Int.to_string high ^ ".." ^ Int.to_string low
  | Reg _ -> "Reg[" ^ sid signal ^ "]"
  | Multiport_mem _ -> "Multiport_mem[" ^ sid signal ^ "]"
  | Mem_read_port _ -> "Mem_read_port[" ^ sid signal ^ "]"
  | Inst _ -> "Inst" ^ sid signal ^ "]"
;;

let const_value =
  let sexp_of_signal = sexp_of_signal_recursive ~depth:1 in
  function
  | Const { constant; _ } -> constant
  | signal ->
    raise_s
      [%message "cannot get the value of a non-constant signal" ~_:(signal : signal)]
;;

let wire_driver = function
  | Wire { driver; _ } -> driver
  | _ -> None
;;

let has_name t = not (List.is_empty (names t))
let `New new_id, `Reset reset_id = Uid.generator ()
let default_wave_format = Wave_format.Bit_or Binary

let make_id width =
  if width <= 0 then raise_s [%message "Width of signals must be >= 0" (width : int)];
  { s_id = new_id ()
  ; s_width = width
  ; s_metadata =
      Option.map (Caller_id.get () ~skip:[]) ~f:(fun caller_id ->
        { names = []
        ; attributes = []
        ; comment = None
        ; caller_id = Some caller_id
        ; wave_format = default_wave_format
        })
  }
;;

let get_metadata t =
  match signal_id t with
  | None -> None
  | Some signal_id -> signal_id.s_metadata
;;

let get_or_alloc_metadata t =
  match t.s_metadata with
  | None ->
    let metadata =
      { names = []
      ; attributes = []
      ; comment = None
      ; caller_id = None
      ; wave_format = default_wave_format
      }
    in
    t.s_metadata <- Some metadata;
    metadata
  | Some metadata -> metadata
;;

let change_metadata t ~f =
  Option.iter (signal_id t) ~f:(fun t ->
    let metadata = get_or_alloc_metadata t in
    f metadata)
;;

let check_attribute_applies_to t = function
  | Rtl_attribute.Applies_to.Non_wires when not (is_wire t) -> true
  | Regs when is_reg t -> true
  | Memories when is_mem t -> true
  | Instantiations when is_inst t -> true
  | Non_wires | Regs | Memories | Instantiations -> false
;;

let check_attribute t attr =
  let applies_to = Rtl_attribute.applies_to attr in
  match applies_to with
  | [] -> ()
  | applies_to ->
    if List.fold applies_to ~init:false ~f:(fun acc app ->
         acc || check_attribute_applies_to t app)
    then ()
    else
      raise_s
        [%message
          "Rtl_attribute is applied to wrong type of signal"
            (t : t)
            (attr : Rtl_attribute.t)]
;;

let add_attribute t attr =
  change_metadata t ~f:(fun metadata ->
    check_attribute t attr;
    metadata.attributes <- attr :: metadata.attributes)
;;

let get_attributes t =
  Option.value_map ~default:[] (get_metadata t) ~f:(fun metadata -> metadata.attributes)
;;

let set_attributes t attributes =
  change_metadata t ~f:(fun metadata ->
    List.iter attributes ~f:(check_attribute t);
    metadata.attributes <- attributes)
;;

let set_comment t comment =
  change_metadata t ~f:(fun metadata -> metadata.comment <- Some comment)
;;

let get_comment t =
  let%bind.Option metadata = get_metadata t in
  metadata.comment
;;

let unset_comment t =
  match get_metadata t with
  | None -> ()
  | Some metadata -> metadata.comment <- None
;;

let add_name t name =
  change_metadata t ~f:(fun metadata -> metadata.names <- name :: metadata.names)
;;

let set_names t names = change_metadata t ~f:(fun metadata -> metadata.names <- names)

let set_wave_format t wave_format =
  change_metadata t ~f:(fun metadata -> metadata.wave_format <- wave_format)
;;

let get_wave_format t =
  match get_metadata t with
  | Some metadata -> metadata.wave_format
  | None -> default_wave_format
;;

let is_vdd = function
  | Const { constant; _ } when Bits.equal constant Bits.vdd -> true
  | _ -> false
;;

let is_gnd = function
  | Const { constant; _ } when Bits.equal constant Bits.gnd -> true
  | _ -> false
;;

let of_bits constant = Const { signal_id = make_id (Bits.width constant); constant }

let has_initializer = function
  | Reg { register = { initialize_to = Some _; _ }; _ } -> true
  | Multiport_mem { initialize_to = Some _; _ } -> true
  | _ -> false
;;

module Register = struct
  type 'a t = 'a register

  let map { clock = { clock; clock_edge }; reset; clear; initialize_to; enable } ~f =
    let clock = f clock in
    let reset =
      Option.map reset ~f:(fun { reset; reset_edge; reset_to } ->
        let reset = f reset in
        let reset_to = f reset_to in
        { reset; reset_edge; reset_to })
    in
    let clear =
      Option.map clear ~f:(fun { clear; clear_to } ->
        let clear = f clear in
        let clear_to = f clear_to in
        { clear; clear_to })
    in
    let initialize_to = Option.map initialize_to ~f in
    let enable = Option.map enable ~f in
    { clock = { clock; clock_edge }; reset; clear; initialize_to; enable }
  ;;

  (* error checking *)
  let assert_width signal w msg =
    if width signal <> w
    then
      raise_s
        [%message
          msg ~info:"signal has unexpected width" ~expected_width:(w : int) (signal : t)]
  ;;

  let assert_width_or_none signal w msg =
    match signal with
    | None -> ()
    | Some signal ->
      if width signal <> w
      then
        raise_s
          [%message
            msg
              ~info:"signal should have expected width or be empty"
              ~expected_width:(w : int)
              (signal : t)]
  ;;

  let zero w = of_bits (Bits.zero w)

  let of_reg_spec (spec : reg_spec) ~enable ~initialize_to ~reset_to ~clear_to ~clear d =
    if width d = 0 then raise_s [%message "Zero width registers are not allowed"];
    assert_width spec.clock 1 "clock is invalid";
    assert_width_or_none spec.reset 1 "reset is invalid";
    let reset =
      Option.map spec.reset ~f:(fun reset ->
        (* If there is a reset, ensure there is a reset_to of the correct width (default
           to zero) *)
        let reset_to =
          match reset_to with
          | None -> zero (width d)
          | Some reset_to ->
            assert_width reset_to (width d) "reset_to is invalid";
            (* if not (is_const reset_to)
             * then raise_s [%message "Register reset_to is not constant" (reset_to : t)]; *)
            reset_to
        in
        { reset; reset_edge = spec.reset_edge; reset_to })
    in
    (* override the clear if required. *)
    let clear =
      match clear with
      | Some clear -> Some clear
      | None -> spec.clear
    in
    assert_width_or_none clear 1 "clear signal is invalid";
    let clear =
      (* If there is a clear, ensure there is a clear_to of the correct width (default to
       zero) *)
      Option.map clear ~f:(fun clear ->
        let clear_to =
          match clear_to with
          | None -> zero (width d)
          | Some clear_to ->
            assert_width clear_to (width d) "clear_to is invalid";
            clear_to
        in
        { clear; clear_to })
    in
    let enable =
      match enable with
      | None -> None
      | Some enable ->
        assert_width enable 1 "enable is invalid";
        if is_vdd enable then None else Some enable
    in
    Option.iter initialize_to ~f:(fun initialize_to ->
      assert_width initialize_to (width d) "initial value is invalid";
      if not (is_const initialize_to)
      then raise_s [%message "Register initializer is not constant" (initialize_to : t)]);
    { clock = { clock = spec.clock; clock_edge = spec.clock_edge }
    ; reset
    ; clear
    ; initialize_to
    ; enable
    }
  ;;
end
