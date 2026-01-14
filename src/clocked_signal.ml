open! Core0

type info = Clock_domain.Runtime.t [@@deriving sexp_of, equal ~localize]

type t =
  { base : Signal.t
  ; dom : info
  }
[@@deriving sexp_of, equal ~localize, fields ~getters]

let to_rep { base; dom } = base, dom
let from_rep (base : Signal.t) (info : info) : t = { base; dom = info }
let update_rep ({ base; dom = _ } : t) ~info : t = { base; dom = info }
let get_domain { base = _; dom } = dom
let to_clocked base ~dom = { base; dom }
let map { base; dom } ~f = { base = f base; dom }

(* Projection modules *)
let[@inline] pr_app (type a) (f : Signal.t -> a) { base; _ } = f base

let[@inline] pr_transform (f : Signal.t -> Signal.t) { base; dom } =
  { base = f base; dom }
;;

let[@inline] pr_transform_extra
  (type a)
  (f : Signal.t -> a -> Signal.t)
  { base; dom }
  extra
  =
  { base = f base extra; dom }
;;

let const_value t = to_rep t |> fst |> Signal.Type.const_value

let ensure_domain ~(loc : [%call_pos]) ?name t ~(required_domain : Clock_domain.Runtime.t)
  =
  let actual_domain = get_domain t in
  match actual_domain, required_domain with
  | _, Unknown ->
    raise_s
      [%message
        "[ensure_domain] is called with required_domain = Unknown. This is not allowed"]
  | Unknown, _ ->
    raise_s
      [%message
        "[ensure_domain] is called with actual_domain = Unknown. This is not allowed. A \
         value with an unknown clock domain is not comaptible with any runtime clock \
         domain including Unknown itself."]
  | Constant, _ ->
    (* When the actual domain is [Constant], it is compatible with any required domain. *)
    t
  | Exact _, Constant ->
    raise_s
      [%message
        "required domain is [Constant], but the value's provided domain is an exact \
         domain."
          (actual_domain : Clock_domain.Runtime.t)]
  | Exact actual, Exact expected ->
    if not (Clock_domain.Exact.equal actual expected)
    then
      raise_s
        [%message.omit_nil
          "[ensure_domain] Non-const Signal has unexpected clock domain"
            (loc : Source_code_position.t)
            ~name:(name : string option)
            ~signal:(t : t)
            ~expected:(expected : Clock_domain.Exact.t)
            ~actual:(actual : Clock_domain.Exact.t)]
    else t
;;

let unwrap_signal ~(loc : [%call_pos]) t ~dom =
  let _ : t = ensure_domain ~loc ~required_domain:dom t in
  t.base
;;

type common_runtime_domain_denominator_error =
  | Contains_unknown of string list
  | Mismatching_clock_domains of (string * info) list
[@@deriving sexp_of]

let find_common_denominator_runtime_clock_domain (alist : (string * info) list)
  : (Clock_domain.Runtime.t, common_runtime_domain_denominator_error) Result.t
  =
  let alist_without_constant_domains =
    List.filter_map alist ~f:(fun (n, dom) ->
      match dom with
      | Constant -> None
      | Unknown | Exact _ -> Some (n, dom))
  in
  if List.is_empty alist_without_constant_domains
  then
    (* Given a list of signals all with [Constant] domain, it is correct that the output
       signal also has an [Constant] domain.
    *)
    Ok Clock_domain.Runtime.Constant
  else (
    let name_of_signals_with_unknown_clock_domains =
      List.filter_map alist_without_constant_domains ~f:(fun (name, dom) ->
        match dom with
        | Unknown -> Some name
        | Constant | Exact _ -> None)
    in
    if not (List.is_empty name_of_signals_with_unknown_clock_domains)
    then Error (Contains_unknown name_of_signals_with_unknown_clock_domains)
    else (
      let common_clock_domain =
        List.map alist_without_constant_domains ~f:(fun (_, s) ->
          (* Calling [exact_exn] here is safe as we filter out Any in the beginning, and
             handle the existence of any [Empty] earlier. *)
          Clock_domain.Runtime.exact_exn s)
        |> List.all_equal ~equal:Clock_domain.Exact.equal
      in
      match common_clock_domain with
      | Some dom -> Ok (Clock_domain.Runtime.Exact dom)
      | None -> Error (Mismatching_clock_domains alist)))
;;

let find_common_denominator_runtime_clock_domain_on_signals l =
  find_common_denominator_runtime_clock_domain
    (List.map l ~f:(fun (n, s) -> n, get_domain s))
;;

let validate_signals_are_consistent ~op_name alist =
  match find_common_denominator_runtime_clock_domain_on_signals alist with
  | Ok dom -> dom
  | Error (Contains_unknown operands) ->
    let message =
      [%string
        {|[%{op_name}] does not allow any of the the operands to have an unknown clock domain. Operands with unknown clock domains:|}]
    in
    raise_s [%message message ~_:(operands : string list)]
  | Error (Mismatching_clock_domains alist) ->
    let message =
      [%string
        {|[%{op_name}] expects the clock domain of the operands to be compatible, but they're not:|}]
    in
    raise_s [%message message ~_:(alist : (string * info) list)]
;;

(* Signal.Names *)
let set_names = pr_app Signal.set_names
let names = pr_app Signal.names
let names_and_locs = pr_app Signal.names_and_locs
let reset_names = Signal.reset_names

(* Signal.Attributes *)
let add_attribute = pr_transform_extra Signal.add_attribute
let attributes = pr_app Signal.attributes
let set_wave_format = pr_app Signal.set_wave_format
let ( --$ ) = pr_transform_extra Signal.( --$ )

(* Signal.Comments *)
let set_comment = pr_transform_extra Signal.set_comment
let unset_comment = pr_transform Signal.unset_comment
let comment = pr_app Signal.comment

(* Signal.Coverage *)
let add_mux_waiver_exn = pr_transform_extra Signal.add_mux_waiver_exn
let add_cases_waiver_exn = pr_transform_extra Signal.add_cases_waiver_exn
let add_register_waiver_exn = pr_transform_extra Signal.add_register_waiver_exn
let add_always_state_waiver_exn = pr_transform_extra Signal.add_always_state_waiver_exn

let add_always_state_transition_waiver_exn =
  pr_transform_extra Signal.add_always_state_transition_waiver_exn
;;

(* Partial Signal.Wires *)
let wireof = pr_transform Signal.wireof

let ( <-- ) s1 s2 =
  (match
     find_common_denominator_runtime_clock_domain_on_signals [ "dst", s1; "src", s2 ]
   with
   | Ok (_ : Clock_domain.Runtime.t) -> ()
   | Error (Contains_unknown operands_with_unknown_clock_domains) ->
     raise_s
       [%message
         "[assign] does not allow any of the operands to have an unknown clock domain. \
          The following operands to [assign] has an unknown clock domain:"
           ~_:(operands_with_unknown_clock_domains : string list)]
   | Error (Mismatching_clock_domains alist) ->
     raise_s
       [%message
         "[assign] expects the clock domain of the two signals to be compatible"
           ~_:(alist : (string * info) list)]);
  Signal.(s1.base <-- s2.base)
;;

let assign s1 s2 = s1 <-- s2
let to_constant_clocked base = { base; dom = Constant }
let to_unknown_clocked base = { base; dom = Unknown }

(* Construction methods. These generate new Clocked signals that don't have any clock
   domain associated with them which is problematic. To use in a context where domains
   matter, this needs to use explicitly use the domain from an existing signal.
*)
let wire width = Signal.wire width |> to_unknown_clocked
let input name width = Signal.input name width |> to_unknown_clocked
let output name { base; dom } = { base = Signal.output name base; dom }
let pp fmt t = Signal.pp fmt t.base

let binop_transform op_name op t1 t2 =
  let dom =
    match
      find_common_denominator_runtime_clock_domain_on_signals [ "lhs", t1; "rhs", t2 ]
    with
    | Ok dom -> dom
    | Error (Contains_unknown operands_with_unknown_clock_domains) ->
      let message =
        Printf.sprintf
          !"%s does not allow any of the the operands to have an unknown clock domain."
          op_name
      in
      raise_s [%message message (operands_with_unknown_clock_domains : string list)]
    | Error (Mismatching_clock_domains operands_with_mismatching_clock_domains) ->
      let message =
        Printf.sprintf
          !"%s expects the clock domain of the two operands to be compatible."
          op_name
      in
      raise_s
        [%message
          message (operands_with_mismatching_clock_domains : (string * info) list)]
  in
  { base = op t1.base t2.base; dom }
;;

module Primitives : Comb.Primitives with type t = t = struct
  type nonrec t = t [@@deriving sexp_of, equal ~localize]

  let empty = Signal.empty |> to_constant_clocked
  let is_empty = pr_app Signal.is_empty
  let width = pr_app Signal.width
  let to_constant = pr_app Signal.to_constant

  let concat_msb signals =
    let named_signals = List.mapi signals ~f:(fun i s -> "concat" ^ Int.to_string i, s) in
    let dom = validate_signals_are_consistent ~op_name:"concat" named_signals in
    let unwraped = List.map signals ~f:(fun { base; _ } -> base) in
    { base = Signal.concat_msb unwraped; dom }
  ;;

  let select { base; dom } ~high ~low = { base = Signal.select base ~high ~low; dom }

  let ( -- ) ~(loc : [%call_pos]) { base; dom } name =
    { base = Signal.( -- ) ~loc base name; dom }
  ;;

  let ( &: ) = binop_transform "&:" Signal.( &: )
  let ( |: ) = binop_transform "|:" Signal.( |: )
  let ( ^: ) = binop_transform "^:" Signal.( ^: )
  let ( ~: ) { base; dom } = { base = Signal.( ~: ) base; dom }
  let to_string = pr_app Signal.to_string
  let of_constant c = Signal.of_constant c |> to_constant_clocked
  let ( +: ) = binop_transform "+:" Signal.( +: )
  let ( -: ) = binop_transform "-:" Signal.( -: )
  let ( *: ) = binop_transform "*:" Signal.( *: )
  let ( *+ ) = binop_transform "*+" Signal.( *+ )
  let ( ==: ) = binop_transform "==:" Signal.( ==: )
  let ( <: ) = binop_transform "<:" Signal.( <: )
  let vdd = Signal.vdd |> to_constant_clocked
  let gnd = Signal.gnd |> to_constant_clocked

  let mux (sel : t) (cases : t list) =
    let dom =
      validate_signals_are_consistent
        ~op_name:"mux"
        (("sel", sel) :: List.mapi cases ~f:(fun i case -> [%string "case%{i#Int}"], case))
    in
    let cases_unwrapped = List.map cases ~f:(fun s -> unwrap_signal ~dom s) in
    { base = Signal.mux sel.base cases_unwrapped; dom }
  ;;

  let cases ~default sel cases =
    let flat_cases =
      List.concat_mapi cases ~f:(fun i (m, v) ->
        [ [%string "match%{i#Int}"], m; [%string "val%{i#Int}"], v ])
    in
    let dom =
      validate_signals_are_consistent
        ~op_name:"cases"
        ([ "default", default; "sel", sel ] @ flat_cases)
    in
    let cases_unwrapped =
      List.map cases ~f:(fun (m, v) -> unwrap_signal ~dom m, unwrap_signal ~dom v)
    in
    { base = Signal.cases ~default:(unwrap_signal ~dom default) sel.base cases_unwrapped
    ; dom
    }
  ;;
end

module Unoptimized = Comb.Make (Primitives)

module Optimized : Comb.S with type t = t = Signal_builders.Const_prop (struct
    include Unoptimized

    let is_const t = to_rep t |> fst |> Signal.Type.is_const
    let const_value = const_value
  end)

module Reg_spec = Reg_spec.Make (struct
    type nonrec t = t [@@deriving sexp_of]

    let is_empty t = Signal.is_empty t.base
  end)

module Reg = struct
  type 'a with_register_spec =
    ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> 'a

  let get_opt = function
    | Some { base; _ } -> Some base
    | None -> None
  ;;

  let spec_to_base (spec : Reg_spec.t) =
    Signal.Reg_spec.create
      ~clock_edge:(Reg_spec.clock_edge spec)
      ?reset:(Reg_spec.reset spec |> get_opt)
      ~reset_edge:(Reg_spec.reset_edge spec)
      ?clear:(Reg_spec.clear spec |> get_opt)
      ()
      ~clock:(Reg_spec.clock spec |> base)
  ;;

  let reg__with_signal_reset
    ?enable
    ?initialize_to
    ?reset_to
    ?clear
    ?clear_to
    (spec : Reg_spec.t)
    t
    =
    let clock = Reg_spec.clock spec in
    let dom =
      [ "clock", Some clock
      ; "in", Some t
      ; "enable", enable
      ; ( "clear"
        , match clear with
          | Some _ -> clear
          | _ -> Reg_spec.clear spec )
      ; "clear_to", clear_to
      ]
      |> List.filter_map ~f:(fun (name, s) ->
        match s with
        | None -> None
        | Some s -> Some (name, s))
      |> validate_signals_are_consistent ~op_name:"reg"
    in
    let base_spec = spec_to_base spec in
    { base =
        Signal.Expert.reg__with_signal_reset
          ?enable:(get_opt enable)
          ?initialize_to
          ?reset_to:(get_opt reset_to)
          ?clear:(get_opt clear)
          ?clear_to:(get_opt clear_to)
          base_spec
          t.base
    ; dom
    }
  ;;

  include Signal_builders.Registers (struct
      type nonrec info = info

      include Optimized
      module Reg_spec = Reg_spec

      let add_attribute = add_attribute
      let reg__with_signal_reset = reg__with_signal_reset
      let wire = wire
      let assign = assign
      let update_rep = update_rep
      let to_rep = to_rep
    end)
end

module Memories = struct
  let multiport_memory_prim
    ?name
    ?(attributes : Rtl_attribute.t list = [])
    ?initialize_to
    size
    ~remove_unused_write_ports
    ~data_width
    ~(write_ports : t Write_port.t array)
    ~(read_addresses : t array)
    =
    let unwrap_write_port
      ({ write_clock; write_address; write_enable; write_data } : t Write_port.t)
      =
      ignore
        (validate_signals_are_consistent
           ~op_name:"multiport_memory"
           [ "write_clock", write_clock
           ; "write_address", write_address
           ; "write_enable", write_enable
           ; "write_data", write_data
           ]
         : Clock_domain.Runtime.t);
      { Write_port.write_clock = base write_clock
      ; write_address = base write_address
      ; write_enable = base write_enable
      ; write_data = base write_data
      }
    in
    let unwrapped_write_ports = Array.map write_ports ~f:unwrap_write_port in
    let read_ports =
      Signal.Expert.multiport_memory_prim
        ?name
        ~attributes
        ?initialize_to
        size
        ~remove_unused_write_ports
        ~data_width
        ~write_ports:unwrapped_write_ports
        ~read_addresses:(Array.map read_addresses ~f:base)
    in
    Array.map2_exn write_ports read_ports ~f:(fun wp base ->
      { base; dom = get_domain wp.write_clock })
  ;;

  include Signal_builders.Memories (struct
      include Optimized
      module Reg_spec = Reg_spec

      type nonrec t = t

      let reg = Reg.reg
      let multiport_memory_prim = multiport_memory_prim
    end)
end

include (Optimized : Comb.S with type t := t)
include Signal_builders.Conversion_functions (Optimized)
include Reg
include Memories

let info (t : t) : info = dom t
let set_info = update_rep
let __ppx_auto_name = ( -- )

module Unsafe = struct
  let set_domain t ~dom = { t with dom }

  let sync_crossing ~(loc : [%call_pos]) ~from_dom ~to_dom t =
    let _ : t = ensure_domain ~loc t ~required_domain:from_dom in
    { t with dom = to_dom }
  ;;

  let reg_spec_to_unclocked t =
    let dom = get_domain (Reg_spec.clock t) in
    let unwrap s = unwrap_signal s ~dom in
    let maybe_unwrap = Option.map ~f:unwrap in
    Signal.Reg_spec.create
      ~clock:(Reg_spec.clock t |> unwrap)
      ~clock_edge:(Reg_spec.clock_edge t)
      ?reset:(Reg_spec.reset t |> maybe_unwrap)
      ~reset_edge:(Reg_spec.reset_edge t)
      ?clear:(Reg_spec.clear t |> maybe_unwrap)
      ()
  ;;
end

module Overrides = struct
  let set_domain = Unsafe.set_domain
  let wire width ~dom = wire width |> set_domain ~dom
  let input name width ~dom = input name width |> set_domain ~dom
end

module Expert = struct
  let multiport_memory_prim = Memories.multiport_memory_prim

  let validate_assign_in_always ~dst ~src =
    ignore
      (validate_signals_are_consistent ~op_name:"Always.(<--)" [ "dst", dst; "src", src ]
       : Clock_domain.Runtime.t)
  ;;

  let reg__with_signal_reset = reg__with_signal_reset
end
