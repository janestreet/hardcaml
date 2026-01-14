open! Core0

module type Uid = Signal__type_intf.Uid
module type Type = Signal__type_intf.Type
module type Uid_set = Signal__type_intf.Uid_set
module type Uid_map = Signal__type_intf.Uid_map
module type With_info = Signal__type_intf.With_info

module Uid = Uid_builder.Make ()

module Op = struct
  type t =
    | Add
    | Sub
    | Mulu
    | Muls
    | And
    | Or
    | Xor
    | Eq
    | Lt
  [@@deriving bin_io, sexp_of, compare ~localize, equal ~localize, hash]
end

module Metadata = struct
  type t =
    { mutable names_and_locs : Name_and_loc.t list
    ; mutable attributes : Rtl_attribute.t list
    ; mutable comment : string option
    ; caller_id : Caller_id.t option
    ; mutable wave_format : Wave_format.t
    ; mutable coverage : Coverage_metadata.t option
    }
  [@@deriving bin_io, sexp_of]
end

module Info = struct
  type t =
    { uid : Uid.t
    ; width : int
    ; mutable metadata : Metadata.t option
    }
  [@@deriving bin_io, sexp_of]
end

module Const = struct
  type t =
    { info : Info.t
    ; constant : Bits.t
    }
  [@@deriving bin_io, sexp_of]
end

module Op2 = struct
  type 'signal t =
    { info : Info.t
    ; op : Op.t
    ; arg_a : 'signal
    ; arg_b : 'signal
    }
  [@@deriving bin_io, sexp_of]

  let map { info; op; arg_a; arg_b } ~f =
    let arg_a = f arg_a in
    let arg_b = f arg_b in
    { info : Info.t; op : Op.t; arg_a; arg_b }
  ;;
end

module Mux = struct
  type 'signal t =
    { info : Info.t
    ; select : 'signal
    ; cases : 'signal list
    }
  [@@deriving bin_io, sexp_of]

  let map { info; select; cases } ~f =
    let select = f select in
    let cases = List.map cases ~f in
    { info : Info.t; select; cases }
  ;;
end

module Cases = struct
  type 'signal t =
    { info : Info.t
    ; select : 'signal
    ; cases : ('signal * 'signal) list
    ; default : 'signal
    }
  [@@deriving bin_io, sexp_of]

  let map { info; select; cases; default } ~f =
    let select = f select in
    let cases = List.map cases ~f:(fun (match_value, data) -> f match_value, f data) in
    let default = f default in
    { info : Info.t; select; cases; default }
  ;;
end

module Cat = struct
  type 'signal t =
    { info : Info.t
    ; args : 'signal list
    }
  [@@deriving bin_io, sexp_of]

  let map { info; args } ~f =
    let args = List.map args ~f in
    { info : Info.t; args }
  ;;
end

module Not = struct
  type 'signal t =
    { info : Info.t
    ; arg : 'signal
    }
  [@@deriving bin_io, sexp_of]

  let map { info; arg } ~f =
    let arg = f arg in
    { info : Info.t; arg }
  ;;
end

module Wire = struct
  type 'signal t =
    { info : Info.t
    ; mutable driver : 'signal option
    }
  [@@deriving bin_io, sexp_of]

  let map { info; driver } ~f =
    let driver = Option.map driver ~f in
    { info : Info.t; driver }
  ;;
end

module Select = struct
  type 'signal t =
    { info : Info.t
    ; arg : 'signal
    ; high : int
    ; low : int
    }
  [@@deriving bin_io, sexp_of]

  let map { info; arg; high; low } ~f =
    let arg = f arg in
    { info : Info.t; arg; high : int; low : int }
  ;;
end

module Reg = struct
  module Clock_spec = struct
    type 'signal t =
      { clock : 'signal
      ; clock_edge : Edge.t
      }
    [@@deriving bin_io, sexp_of]

    let map { clock; clock_edge } ~f =
      let clock = f clock in
      { clock; clock_edge : Edge.t }
    ;;
  end

  module Reset_spec = struct
    type 'signal t =
      { reset : 'signal
      ; reset_edge : Edge.t
      ; reset_to : 'signal
      }
    [@@deriving bin_io, sexp_of]

    let map { reset; reset_edge; reset_to } ~f =
      let reset = f reset in
      let reset_to = f reset_to in
      { reset; reset_edge : Edge.t; reset_to }
    ;;
  end

  module Clear_spec = struct
    type 'signal t =
      { clear : 'signal
      ; clear_to : 'signal
      }
    [@@deriving bin_io, sexp_of]

    let map { clear; clear_to } ~f =
      let clear = f clear in
      let clear_to = f clear_to in
      { clear; clear_to }
    ;;
  end

  module Register = struct
    type 'signal t =
      { clock : 'signal Clock_spec.t
      ; reset : 'signal Reset_spec.t option
      ; clear : 'signal Clear_spec.t option
      ; enable : 'signal option
      ; initialize_to : Bits.t option
      }
    [@@deriving bin_io, sexp_of]

    let map { clock; reset; clear; enable; initialize_to } ~f =
      let clock = Clock_spec.map clock ~f in
      let reset = Option.map reset ~f:(Reset_spec.map ~f) in
      let clear = Option.map clear ~f:(Clear_spec.map ~f) in
      let enable = Option.map enable ~f in
      { clock; reset; clear; enable; initialize_to }
    ;;
  end

  type 'signal t =
    { info : Info.t
    ; register : 'signal Register.t
    ; d : 'signal
    }
  [@@deriving bin_io, sexp_of]

  let map { info; register; d } ~f =
    let d = f d in
    let register = Register.map register ~f in
    { info : Info.t; register; d }
  ;;
end

module Multiport_mem = struct
  type 'signal t =
    { info : Info.t
    ; size : int
    ; write_ports : 'signal Write_port.t array
    ; initialize_to : Bits.t array option
    }
  [@@deriving bin_io, sexp_of]

  let map { info; size; write_ports; initialize_to } ~f =
    let write_ports = Array.map write_ports ~f:(Write_port.map ~f) in
    { info : Info.t; size : int; write_ports; initialize_to : Bits.t array option }
  ;;
end

module Mem_read_port = struct
  type 'signal t =
    { info : Info.t
    ; memory : 'signal
    ; read_address : 'signal
    }
  [@@deriving bin_io, sexp_of]

  let map { info; memory; read_address } ~f =
    let read_address = f read_address in
    let memory = f memory in
    { info : Info.t; memory; read_address }
  ;;
end

module Inst = struct
  module Input = struct
    type 'signal t =
      { name : string
      ; input_signal : 'signal
      }
    [@@deriving bin_io, sexp_of]

    let map { name; input_signal } ~f = { name; input_signal = f input_signal }
  end

  module Output = struct
    type t =
      { name : string
      ; output_width : int
      ; output_low_index : int
      }
    [@@deriving bin_io, sexp_of, equal]
  end

  module Vhdl_instance = struct
    type t =
      { library_name : string
      ; architecture_name : string
      }
    [@@deriving bin_io, sexp_of]
  end

  module Instantiation = struct
    type 'signal t =
      { circuit_name : string
      ; instance_label : string
      ; parameters : Parameter.t list
      ; inputs : 'signal Input.t list
      ; outputs : Output.t list
      ; vhdl_instance : Vhdl_instance.t
      }
    [@@deriving bin_io, sexp_of]

    let map
      { circuit_name; instance_label; parameters; inputs; outputs; vhdl_instance }
      ~f
      =
      { circuit_name
      ; instance_label
      ; parameters
      ; inputs = List.map inputs ~f:(Input.map ~f)
      ; outputs
      ; vhdl_instance
      }
    ;;
  end

  type 'signal t =
    { info : Info.t
    ; instantiation : 'signal Instantiation.t
    }
  [@@deriving bin_io, sexp_of]

  let map { info; instantiation } ~f =
    { info : Info.t; instantiation = Instantiation.map instantiation ~f }
  ;;
end

type t =
  | Empty
  | Const of Const.t
  | Op2 of t Op2.t
  | Mux of t Mux.t
  | Cases of t Cases.t
  | Cat of t Cat.t
  | Not of t Not.t
  | Wire of t Wire.t
  | Select of t Select.t
  | Reg of t Reg.t
  | Multiport_mem of t Multiport_mem.t
  | Mem_read_port of t Mem_read_port.t
  | Inst of t Inst.t

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

module Uid_map = Map.M (Uid)

module Uid_set = struct
  type t = Set.M(Uid).t [@@deriving sexp_of]

  let empty = Set.empty (module Uid)
end

module type Printable =
  Signal__type_intf.Printable
  with type t := t
   and type register := t Reg.Register.t
   and type reg_spec := reg_spec

module type Is_a = Signal__type_intf.Is_a with type t := t and type signal_op := Op.t
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
              { clock = { clock; clock_edge = _ }
              ; reset
              ; clear
              ; enable
              ; initialize_to = _
              }
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
      | Inst { instantiation = { inputs; _ }; _ } ->
        List.fold ~init inputs ~f:(fun arg { name = _; input_signal } ->
          f arg input_signal)
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
      | Cases { select; cases; default; _ } ->
        let arg = f init select in
        let arg =
          List.fold ~init:arg cases ~f:(fun arg (match_with, value) ->
            let arg = f arg match_with in
            f arg value)
        in
        let arg = f arg default in
        arg
    ;;
  end)

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let info s =
  match s with
  | Empty -> None
  | Const { info; _ }
  | Select { info; _ }
  | Reg { info; _ }
  | Multiport_mem { info; _ }
  | Mem_read_port { info; _ }
  | Wire { info; _ }
  | Inst { info; _ }
  | Op2 { info; _ }
  | Mux { info; _ }
  | Cases { info; _ }
  | Cat { info; _ }
  | Not { info; _ } -> Some info [@exclave_if_stack a]
;;

let uid (s @ m) =
  match[@exclave_if_stack a] (info [@alloc a]) s with
  | None -> Uid.zero
  | Some s -> s.uid
;;]

let%template[@mode local] uid = (uid [@alloc stack])

let width s =
  match info s with
  | None -> 0
  | Some s -> s.width
;;

let caller_id s =
  let%bind.Option s = info s in
  let%bind.Option m = s.metadata in
  m.caller_id
;;

let coverage_metadata s =
  let%bind.Option s = info s in
  let%bind.Option m = s.metadata in
  m.coverage
;;

let names_and_locs s =
  match info s with
  | None -> raise_s [%message "cannot get [names] from the empty signal"]
  | Some s ->
    Option.value_map ~default:[] s.metadata ~f:(fun metadata -> metadata.names_and_locs)
;;

let names s : string list = List.map (names_and_locs s) ~f:(fun s -> s.name)

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
      let kinds_are_the_same () =
        match a, b with
        | Const { constant = a; _ }, Const { constant = b; _ } -> Bits.equal a b
        | Select { high = h0; low = l0; _ }, Select { high = h1; low = l1; _ } ->
          h0 = h1 && l0 = l1
        | Multiport_mem { size = mem_size0; _ }, Multiport_mem { size = mem_size1; _ } ->
          mem_size0 = mem_size1
        | Inst { instantiation = i0; _ }, Inst { instantiation = i1; _ } ->
          String.equal i0.circuit_name i1.circuit_name
          (* i0.inst_instance=i1.inst_instance && *)
          (* inst_inputs=??? *)
          && [%equal: Parameter.t list] i0.parameters i1.parameters
          && [%equal: Inst.Output.t list] i0.outputs i1.outputs
        | Op2 { op = o0; _ }, Op2 { op = o1; _ } -> Op.equal o0 o1
        | Empty, Empty
        | Reg _, Reg _
        | Mem_read_port _, Mem_read_port _
        | Wire _, Wire _
        | Not _, Not _
        | Mux _, Mux _
        | Cases _, Cases _
        | Cat _, Cat _ -> true
        | ( ( Empty
            | Const _
            | Select _
            | Op2 _
            | Multiport_mem _
            | Inst _
            | Reg _
            | Mem_read_port _
            | Wire _
            | Not _
            | Mux _
            | Cases _
            | Cat _ )
          , _ ) -> false
      in
      let widths_are_the_same () = width a = width b in
      let names_are_the_same () =
        let names_or_empty = function
          | Empty -> []
          | s -> names s
        in
        if check_names
        then [%compare.equal: string list] (names_or_empty a) (names_or_empty b)
        else true
      in
      let deps_are_the_same () =
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
      let attributes_are_the_same () =
        let attributes_set a =
          let%bind.Option info = info a in
          let%map.Option metadata = info.metadata in
          let attributes = metadata.attributes in
          Set.of_list (module Rtl_attribute) attributes
        in
        let a = attributes_set a in
        let b = attributes_set b in
        [%equal: Set.M(Rtl_attribute).t option] a b
      in
      if widths_are_the_same ()
         && kinds_are_the_same ()
         && attributes_are_the_same ()
         && names_are_the_same ()
      then deps_are_the_same ()
      else set, false)
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
  | Op2 { op = o; _ } -> Op.equal o op
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

let is_cases = function
  | Cases _ -> true
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
  | Op.Add -> "add"
  | Sub -> "sub"
  | Mulu -> "mulu"
  | Muls -> "muls"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Eq -> "eq"
  | Lt -> "lt"
;;

let rec sexp_of_instantiation_recursive
  ?show_uids
  ?show_locs
  ~depth
  (inst : t Inst.Instantiation.t)
  =
  let sexp_of_next s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
  let name =
    if String.is_empty inst.vhdl_instance.library_name
    then inst.circuit_name
    else inst.vhdl_instance.library_name ^ "." ^ inst.circuit_name
  in
  let name =
    if String.is_empty inst.vhdl_instance.architecture_name
    then name
    else name ^ "(" ^ inst.vhdl_instance.architecture_name ^ ")"
  in
  let name = name ^ "{" ^ inst.instance_label ^ "}" in
  let sexp_of_inst_input ({ name; input_signal } : t Inst.Input.t) =
    [%sexp ((name, input_signal) : string * next)]
  in
  let sexp_of_inst_output ({ name; output_width; output_low_index = _ } : Inst.Output.t) =
    [%sexp ((name, output_width) : string * int)]
  in
  [%message
    name
      ~parameters:(inst.parameters : Parameter.t list)
      ~inputs:(inst.inputs : inst_input list)
      ~outputs:(inst.outputs : inst_output list)]

and sexp_of_bits_opt bits = Option.map bits ~f:Bits.sexp_of_t

and sexp_of_register_recursive ?show_uids ?show_locs ~depth (reg : t Reg.Register.t) =
  let sexp_of_next s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
  let sexp_of_opt s ~f = Option.map s ~f:(fun s -> sexp_of_next (f s)) in
  [%message
    ""
      ~clock:(sexp_of_next reg.clock.clock : Sexp.t)
      ~clock_edge:(reg.clock.clock_edge : Edge.t)
      ~initialize_to:(sexp_of_bits_opt reg.initialize_to : (Sexp.t option[@sexp.option]))
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

and sexp_of_reg_spec_recursive ?show_uids ?show_locs ~depth spec =
  let sexp_of_next s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
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
  ?show_locs
  ~depth
  (size, write_address, read_address, write_enable)
  =
  let sexp_of_signal s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
  [%message
    ""
      (size : int)
      (write_address : signal)
      (read_address : signal)
      (write_enable : signal)]

and sexp_of_multiport_memory_recursive ?show_uids ?show_locs ~depth (size, write_ports) =
  let sexp_of_signal s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
  let sexp_of_write_port (w : _ Write_port.t) =
    [%message
      ""
        ~write_enable:(w.write_enable : signal)
        ~write_address:(w.write_address : signal)
        ~write_data:(w.write_data : signal)]
  in
  [%message "" (size : int) (write_ports : write_port array)]

and sexp_of_mem_read_port_recursive ?show_uids ?show_locs ~depth (memory, read_addresses) =
  let sexp_of_signal s =
    sexp_of_signal_recursive ?show_uids ?show_locs ~depth:(depth - 1) s
  in
  [%message "" (memory : signal) (read_addresses : signal)]

and sexp_of_signal_recursive ?(show_uids = false) ?(show_locs = false) ~depth signal =
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
    | Cases _ -> "cases"
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
    let names, names_and_locs =
      if show_locs
      then (
        match names_and_locs signal with
        | [] -> None, None
        | names_and_locs -> None, Some names_and_locs)
      else (
        match names signal with
        | [] -> None, None
        | names -> Some names, None)
    in
    let width = width signal in
    let loc = caller_id signal in
    let create
      ?value
      ?arguments
      ?select
      ?data
      ?cases
      ?default
      ?range
      ?instantiation
      ?register
      ?memory
      ?multiport_memory
      ?mem_read_port
      ?data_in
      constructor
      =
      [%message.omit_nil
        constructor
          (uid : Uid.t option)
          (names : string list option)
          (names_and_locs : Name_and_loc.t list option)
          (loc : Caller_id.t option)
          (width : int)
          (value : string option)
          (range : (int * int) option)
          (select : next option)
          (cases : (next * next) list option)
          (data : next list option)
          (default : next option)
          ~_:(instantiation : instantiation option)
          ~_:(register : register option)
          ~_:(memory : memory option)
          ~_:(multiport_memory : multiport_memory option)
          ~_:(mem_read_port : mem_read_port option)
          (arguments : next list option)
          (data_in : next option)]
    in
    match signal with
    | Empty -> create "empty"
    | Const { constant; _ } -> create tag ~value:(display_const constant)
    | Mux { select; cases; _ } -> create tag ~select ~data:cases
    | Cases { select; cases; default; _ } -> create tag ~select ~cases ~default
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
  | Cases _ -> "Op[" ^ sid signal ^ "] = " ^ "cases"
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
let `New new_id, `Reset _ = Uid.generator ()
let default_wave_format = Wave_format.Bit_or Hex

let make_id_allow_zero_width width : Info.t =
  { uid = new_id ()
  ; width
  ; metadata =
      Option.map (Caller_id.get () ~skip:[]) ~f:(fun caller_id ->
        { Metadata.names_and_locs = []
        ; attributes = []
        ; comment = None
        ; caller_id = Some caller_id
        ; wave_format = default_wave_format
        ; coverage = None
        })
  }
;;

let make_id width =
  if width <= 0 then raise_s [%message "Width of signals must be >= 0" (width : int)];
  make_id_allow_zero_width width
;;

let get_metadata t =
  let%bind.Option s = info t in
  s.metadata
;;

let get_or_alloc_metadata (t : Info.t) =
  match t.metadata with
  | None ->
    let metadata =
      { Metadata.names_and_locs = []
      ; attributes = []
      ; comment = None
      ; caller_id = None
      ; wave_format = default_wave_format
      ; coverage = None
      }
    in
    t.metadata <- Some metadata;
    metadata
  | Some metadata -> metadata
;;

let change_metadata t ~f =
  Option.iter (info t) ~f:(fun t ->
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
  | _ ->
    if not (List.exists applies_to ~f:(check_attribute_applies_to t))
    then
      raise_s
        [%message
          "Rtl_attribute is applied to wrong type of signal"
            (t : t)
            (attr : Rtl_attribute.t)]
;;

let add_attribute t attr =
  change_metadata t ~f:(fun metadata ->
    check_attribute t attr;
    List.iter metadata.attributes ~f:(fun existing_attr ->
      if String.equal (Rtl_attribute.name existing_attr) (Rtl_attribute.name attr)
      then
        raise_s
          [%message
            "Tried to assign the same attribute to a signal twice"
              (t : t)
              ~old:(existing_attr : Rtl_attribute.t)
              ~new_:(attr : Rtl_attribute.t)]);
    metadata.attributes <- attr :: metadata.attributes)
;;

let get_attributes t =
  Option.value_map ~default:[] (get_metadata t) ~f:(fun metadata -> metadata.attributes)
;;

let set_attributes t attributes =
  change_metadata t ~f:(fun metadata ->
    metadata.attributes <- [];
    (* Add the attributes one by one to check for duplicated names *)
    List.iter attributes ~f:(add_attribute t))
;;

let set_comment t comment =
  change_metadata t ~f:(fun metadata -> metadata.comment <- Some comment)
;;

let get_comment t =
  let%bind.Option metadata = get_metadata t in
  metadata.comment
;;

let unset_comment t = change_metadata t ~f:(fun metadata -> metadata.comment <- None)

let add_name t name_and_loc =
  change_metadata t ~f:(fun metadata ->
    metadata.names_and_locs <- name_and_loc :: metadata.names_and_locs)
;;

let set_names t names =
  change_metadata t ~f:(fun metadata -> metadata.names_and_locs <- names)
;;

let set_wave_format t wave_format =
  change_metadata t ~f:(fun metadata -> metadata.wave_format <- wave_format)
;;

let get_wave_format t =
  Option.value_map (get_metadata t) ~default:default_wave_format ~f:(fun m ->
    m.wave_format)
;;

let update_coverage_metadata t ~f =
  change_metadata t ~f:(fun metadata -> metadata.coverage <- Some (f metadata.coverage))
;;

let is_vdd = function
  | Const { constant; _ } when Bits.equal constant Bits.vdd -> true
  | _ -> false
;;

let is_gnd = function
  | Const { constant; _ } when Bits.equal constant Bits.gnd -> true
  | _ -> false
;;

let of_bits constant = Const { info = make_id (Bits.width constant); constant }

let has_initializer = function
  | Reg { register = { initialize_to = Some _; _ }; _ } -> true
  | Multiport_mem { initialize_to = Some _; _ } -> true
  | _ -> false
;;

module Register = struct
  type 'a t = 'a Reg.Register.t

  let map = Reg.Register.map

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
    | Some signal when width signal <> w ->
      raise_s
        [%message
          msg
            ~info:"signal should have expected width or be empty"
            ~expected_width:(w : int)
            (signal : t)]
    | None | Some _ -> ()
  ;;

  let assert_width_bits (bits : Bits.t) w msg =
    if Bits.width bits <> w
    then
      raise_s
        [%message
          msg ~info:"bits has unexpected width" ~expected_width:(w : int) (bits : Bits.t)]
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
            reset_to
        in
        { Reg.Reset_spec.reset; reset_edge = spec.reset_edge; reset_to })
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
        { Reg.Clear_spec.clear; clear_to })
    in
    let enable =
      match enable with
      | None -> None
      | Some enable ->
        assert_width enable 1 "enable is invalid";
        if is_vdd enable then None else Some enable
    in
    Option.iter initialize_to ~f:(fun initialize_to ->
      assert_width_bits initialize_to (width d) "initial value is invalid");
    { Reg.Register.clock = { clock = spec.clock; clock_edge = spec.clock_edge }
    ; reset
    ; clear
    ; initialize_to
    ; enable
    }
  ;;
end

let map_info t ~f =
  match t with
  | Empty -> Empty
  | Const { info; constant } -> Const { info = f info; constant }
  | Op2 { info; op; arg_a; arg_b } -> Op2 { info = f info; op; arg_a; arg_b }
  | Mux { info; select; cases } -> Mux { info = f info; select; cases }
  | Cases { info; select; cases; default } ->
    Cases { info = f info; select; cases; default }
  | Cat { info; args } -> Cat { info = f info; args }
  | Not { info; arg } -> Not { info = f info; arg }
  | Select { info; arg; high; low } -> Select { info = f info; arg; high; low }
  | Reg { info; register; d } -> Reg { info = f info; register; d }
  | Multiport_mem { info; size; write_ports; initialize_to } ->
    Multiport_mem { info = f info; size; write_ports; initialize_to }
  | Mem_read_port { info; memory; read_address } ->
    Mem_read_port { info = f info; memory; read_address }
  | Inst { info; instantiation } -> Inst { info = f info; instantiation }
  | Wire { info; driver } -> Wire { info = f info; driver }
;;

let map_dependant t ~f =
  match t with
  | Empty -> Empty
  | Const { constant; info } -> Const { info; constant : Bits.t }
  | Op2 op2 -> Op2 (Op2.map op2 ~f)
  | Mux mux -> Mux (Mux.map mux ~f)
  | Cases cases -> Cases (Cases.map cases ~f)
  | Cat cat -> Cat (Cat.map cat ~f)
  | Not not_ -> Not (Not.map not_ ~f)
  | Select select -> Select (Select.map select ~f)
  | Reg reg -> Reg (Reg.map reg ~f)
  | Multiport_mem mem -> Multiport_mem (Multiport_mem.map mem ~f)
  | Mem_read_port mem_read -> Mem_read_port (Mem_read_port.map mem_read ~f)
  | Inst inst -> Inst (Inst.map inst ~f)
  | Wire wire -> Wire (Wire.map wire ~f)
;;

module Make_default_info (S : sig
    type t
  end) : With_info with type t := S.t and type info = unit = struct
  type info = unit

  let info _ = ()
  let set_info t ~info:() = t
end
