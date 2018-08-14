open! Import
open Circuit

include Cyclesim_intf

module Port_list = struct
  type t = (string * Bits.t ref) list [@@deriving sexp_of]
end

exception Failure of string
let failwith str = raise (Failure str)

let uid = Signal.uid
let names = Signal.names

let find_elements circuit =
  Signal_graph.depth_first_search (Circuit.signal_graph circuit)
    ~init:([],[],[],[],[])
    ~f_before:(fun (regs, mems, consts, inputs, remaining) signal ->
      if Signal.is_empty signal
      then (regs, mems, consts, inputs , remaining)
      else if Signal.is_reg signal
      then (signal :: regs, mems, consts, inputs , remaining)
      else if Signal.is_const signal
      then (regs, mems, signal :: consts, inputs , remaining)
      else if Circuit.is_input circuit signal
      then (regs, mems, consts, signal :: inputs , remaining)
      else if Signal.is_mem signal
      then (regs, signal :: mems, consts, inputs, remaining)
      else (regs, mems, consts, inputs , signal :: remaining))

(* Mangle all names - creating a map of signal uid's to names.  First, add all inputs,
   outputs and reserved words - they must not be mangled or things get very confusing.
   Then the rest of the signals. *)
let mangle_names reserved prefix circuit =
  let inputs = inputs circuit in
  let outputs = outputs circuit in
  let ios = inputs @ outputs in
  let ioset = Set.of_list (module Signal.Uid) (List.map ios ~f:uid) in
  let module IntPair = struct
    module T = struct type t = Signal.Uid.t * int [@@deriving compare, sexp_of] end
    include T
    include Comparator.Make(T)
  end in
  (* initialise the mangler with all reserved words and the inputs+outputs *)
  let mangler = Mangler.create ~case_sensitive:true in
  Mangler.add_identifiers_exn mangler
    ((List.map ios ~f:(fun x -> List.hd_exn (names x))) @ reserved);
  (* initialize the name map with inputs and outputs *)
  let name_map =
    List.fold ios ~init:(Map.empty (module IntPair)) ~f:(fun map signal ->
      Map.set map ~key:(uid signal, 0) ~data:(List.hd_exn (names signal))) in
  (* add name to mangler and name map *)
  let generated_name map uid =
    let name = Mangler.mangle mangler (prefix ^ Int64.to_string uid) in
    Map.set map ~key:(uid, 0) ~data:name
  in
  let add_name map signal =
    let is_io signal = Set.mem ioset (uid signal) in
    if is_io signal || Signal.is_empty signal
    then
      (* IO signal names are handled seperately (they are not mangled) *)
      map
    else if List.is_empty (names signal)
    then
      (* generated name *)
      generated_name map (uid signal)
    else
      (* mangle signal names *)
      fst (
        List.fold (names signal) ~init:(map, 0) ~f:(fun (map, i) name ->
          let name = Mangler.mangle mangler name in
          Map.set map ~key:(uid signal, i) ~data:name, i+1))
  in
  (* add special case for memories and instantiations *)
  let add_name map (signal : Signal.t) =
    match signal with
    | Mem (_, u, _, _) -> add_name (generated_name map u) signal
    | Inst (_, u, _) -> add_name (generated_name map u) signal
    | _ -> add_name map signal
  in
  let name_map =
    Signal_graph.depth_first_search
      (Signal_graph.create outputs)
      ~init:name_map
      ~f_before:add_name
  in
  let lookup name index = Map.find_exn name_map (name, index) in
  lookup

(* internally traced nodes *)
let get_internal_ports circuit ~is_internal_port =
  (* create name mangler *)
  let name = mangle_names [] "_" circuit in
  let i =
    match is_internal_port with
    | None -> []
    | Some f ->
      Signal_graph.filter (Circuit.signal_graph circuit) ~f:(fun s ->
        not (Circuit.is_input circuit s)
        && not (Circuit.is_output circuit s)
        && not (Signal.is_empty s)
        && f s)
  in
  (* Create a wire for each node, and give it the mangled names.  NOTE: these wire are
     required to make registers 'look' like they are updating correctly in simulation,
     however, they are not specifically needed for combinatorial nodes.  It does make the
     name mangling scheme a wee bit easier though. *)
  let dangler_uid = ref Int64.(shift_right_logical max_value 1) in
  let dangler s_width =
    let open Signal in
    Int64.incr dangler_uid;
    Wire ({ s_id = !dangler_uid
          ; s_names = []
          ; s_width
          ; s_deps = [] },
          ref Empty)
  in
  List.map i ~f:(fun s ->
    let w = dangler (Signal.width s) in
    Signal.(<==) w s;
    List.iteri (names s) ~f:(fun i _ -> ignore (Signal.(--) w (name (uid s) i)));
    w)

type signal_bundles =
  { schedule       : Signal.t list
  ; internal_ports : Signal.t list
  ; regs           : Signal.t list
  ; mems           : Signal.t list
  ; consts         : Signal.t list
  ; inputs         : Signal.t list
  ; remaining      : Signal.t list
  ; ready          : Signal.t list }

let scheduling_deps (s : Signal.t) =
  match s with
  | Mem (_, _, _, m) -> [ m.mem_read_address ]
  | Reg _ -> [ ]
  | Empty | Const _ | Op _ | Wire _ | Select _ | Inst _ -> Signal.deps s

let get_schedule circuit internal_ports =
  let regs, mems, consts, inputs, remaining = find_elements circuit in
  let ready = regs @ inputs @ consts in
  let outputs = Circuit.outputs circuit @ internal_ports in
  let schedule =
    if List.is_empty outputs
    then []
    else Signal_graph.topological_sort ~deps:scheduling_deps (Signal_graph.create outputs)
  in
  let schedule_set =
    List.concat [ internal_ports; mems; remaining ]
    |> List.map ~f:Signal.uid
    |> Set.of_list (module Signal.Uid)
  in
  let schedule =
    List.filter schedule ~f:(fun signal -> Set.mem schedule_set (Signal.uid signal)) in
  { schedule; internal_ports; regs; mems; consts; inputs; remaining; ready }

let get_maps ~ref ~const ~zero ~bundle =
  let data_add map signal =
    let value = match (signal : Signal.t) with
      | Const (_, v) -> const v
      | _ -> zero (Signal.width signal)
    in
    Map.set map ~key:(uid signal) ~data:(ref value)
  in
  let data_map = Signal.Uid_map.empty in
  let data_map = List.fold bundle.ready          ~init:data_map ~f:data_add in
  let data_map = List.fold bundle.internal_ports ~init:data_map ~f:data_add in
  let data_map = List.fold bundle.mems           ~init:data_map ~f:data_add in
  let data_map = List.fold bundle.remaining      ~init:data_map ~f:data_add in
  let reg_add map signal =
    Map.set map ~key:(uid signal) ~data:(ref (zero (Signal.width signal)))
  in
  let reg_map = List.fold bundle.regs ~init:Signal.Uid_map.empty ~f:reg_add in
  let mem_add map (signal : Signal.t) =
    match signal with
    | Mem (_, _, _, m) ->
      let mem =
        Array.init m.mem_size ~f:(fun _ -> zero (Signal.width signal))
      in
      Map.set map ~key:(uid signal) ~data:mem
    | _ -> failwith "Expecting memory"
  in
  let mem_map = List.fold bundle.mems ~init:Signal.Uid_map.empty ~f:mem_add in
  data_map, reg_map, mem_map

let filter_none l =
  let l = List.filter l ~f:Option.is_some in
  List.map l ~f:(function Some x -> x | _ -> failwith "error")

module Io_ports = struct
  type 'a t =
    { in_ports                    : 'a
    ; out_ports_before_clock_edge : 'a
    ; out_ports_after_clock_edge  : 'a
    ; internal_ports              : 'a }
end

let io_ports ~copy circuit data_map internal_ports : _ Io_ports.t =
  (* list of input ports *)
  let in_ports =
    List.map (Circuit.inputs circuit) ~f:(fun signal ->
      (List.hd_exn (names signal)), Map.find_exn data_map (uid signal))
  in
  (* list of output ports *)
  let out_ports_after_clock_edge =
    List.map (Circuit.outputs circuit) ~f:(fun signal ->
      (List.hd_exn (names signal)), (Map.find_exn data_map (uid signal)))
  in
  let out_ports_before_clock_edge = List.map out_ports_after_clock_edge ~f:copy in
  let internal_ports =
    List.concat @@
    List.map internal_ports ~f:(fun signal ->
      (List.map (names signal) ~f:(fun name ->
         name, Map.find_exn data_map (uid signal))))
  in
  { in_ports; out_ports_after_clock_edge; out_ports_before_clock_edge; internal_ports }

type task = unit -> unit

type ('i, 'o) t =
  { in_ports                    : Port_list.t
  ; out_ports_before_clock_edge : Port_list.t
  ; out_ports_after_clock_edge  : Port_list.t
  ; internal_ports              : Port_list.t
  ; inputs                      : 'i
  ; outputs_after_clock_edge    : 'o
  ; outputs_before_clock_edge   : 'o
  ; reset                       : task
  ; cycle_check                 : task
  ; cycle_comb0                 : task
  ; cycle_seq                   : task
  ; cycle_comb1                 : task
  ; lookup_signal               : Signal.Uid.t -> Bits.t ref
  ; lookup_reg                  : Signal.Uid.t -> Bits.t ref }
[@@deriving fields]

let sexp_of_t sexp_of_i sexp_of_o t =
  [%message
    ""
      ~inputs:(t.inputs : i)
      ~outputs_before_clock_edge:(t.outputs_before_clock_edge : o)
      ~outputs_after_clock_edge:(t.outputs_after_clock_edge : o)]

type t_port_list = (Port_list.t, Port_list.t) t

module Private = struct

  type nonrec task = task

  let create
        ~in_ports
        ~out_ports_before_clock_edge
        ~out_ports_after_clock_edge
        ~internal_ports
        ~reset
        ~cycle_check
        ~cycle_comb0
        ~cycle_seq
        ~cycle_comb1
        ~lookup_signal
        ~lookup_reg =
    { in_ports
    ; out_ports_before_clock_edge
    ; out_ports_after_clock_edge
    ; internal_ports
    ; inputs                      = in_ports
    ; outputs_before_clock_edge   = out_ports_before_clock_edge
    ; outputs_after_clock_edge    = out_ports_after_clock_edge
    ; reset
    ; cycle_check
    ; cycle_comb0
    ; cycle_seq
    ; cycle_comb1
    ; lookup_signal
    ; lookup_reg }

  module Step = struct
    type t = Reset | Check | Comb0 | Seq | Comb1
    [@@deriving sexp_of]
  end

  let modify t l =
    List.fold l ~init:t ~f:(fun t ((side : Side.t), (step : Step.t), f) ->
      let apply current =
        match side with
        | Before -> fun () -> f (); current ()
        | After  -> fun () -> current (); f ()
      in
      match step with
      | Reset -> { t with reset       = apply t.reset }
      | Check -> { t with cycle_check = apply t.cycle_check }
      | Comb0 -> { t with cycle_comb0 = apply t.cycle_comb0 }
      | Seq   -> { t with cycle_seq   = apply t.cycle_seq }
      | Comb1 -> { t with cycle_comb1 = apply t.cycle_comb1 })

  let coerce sim ~to_input ~to_output =
    { sim with inputs                    = to_input sim.in_ports
             ; outputs_after_clock_edge  = to_output sim.out_ports_after_clock_edge
             ; outputs_before_clock_edge = to_output sim.out_ports_before_clock_edge }
end

let cycle_check sim = sim.cycle_check ()
let cycle_comb0 sim = sim.cycle_comb0 ()
let cycle_seq   sim = sim.cycle_seq ()
let cycle_comb1 sim = sim.cycle_comb1 ()
let reset       sim = sim.reset ()
let cycle       sim =
  cycle_check  sim;
  cycle_comb0  sim;
  cycle_seq    sim;
  cycle_comb1  sim

let in_port sim name =
  try List.Assoc.find_exn sim.in_ports name ~equal:String.equal
  with _ -> failwith ("couldn't find input port " ^ name)
let out_port_after_clock_edge sim name =
  try List.Assoc.find_exn sim.out_ports_after_clock_edge name ~equal:String.equal
  with _ -> failwith ("cound't find output port " ^ name)
let out_port_before_clock_edge sim name =
  try List.Assoc.find_exn sim.out_ports_before_clock_edge name ~equal:String.equal
  with _ -> failwith ("cound't find output port " ^ name)

let out_port ?(clock_edge = Side.After) t name =
  match clock_edge with
  | Before -> out_port_before_clock_edge t name
  | After  -> out_port_after_clock_edge t name

let out_ports ?(clock_edge = Side.After) t =
  match clock_edge with
  | Before -> t.out_ports_before_clock_edge
  | After  -> t.out_ports_after_clock_edge

let outputs ?(clock_edge = Side.After) t  =
  match clock_edge with
  | Before -> t.outputs_before_clock_edge
  | After  -> t.outputs_after_clock_edge

let empty_ops_database = Combinational_ops_database.create ()

module Kind = struct
  type t = Immutable | Mutable [@@deriving sexp_of]
end

type 'a with_create_options
  =  ?is_internal_port : (Signal.t -> bool)
  -> ?combinational_ops_database : Combinational_ops_database.t
  -> kind : Kind.t
  -> 'a

let create_immutable
      ~is_internal_port
      ~combinational_ops_database
      ~circuit =
  (* add internally traced nodes *)
  let internal_ports = get_internal_ports circuit ~is_internal_port in
  let bundle = get_schedule circuit internal_ports in
  (* build maps *)
  let data_map, reg_map, mem_map =
    get_maps ~ref ~const:Bits.const ~zero:Bits.zero ~bundle in
  (* compilation *)
  let compile signal =
    let tgt = Map.find_exn data_map (uid signal) in
    let deps =
      List.map (Signal.deps signal) ~f:(fun signal ->
        match Map.find data_map (uid signal) with
        | Some x -> x
        | None -> ref Bits.empty)
    in
    match signal with
    | Empty -> failwith "cant compile empty signal"
    | Const _ -> None
    | Op (_, op) ->
      let op2 op =
        let a = List.nth_exn deps 0 in
        let b = List.nth_exn deps 1 in
        Some (fun () -> tgt := op !a !b;)
      in
      (match op with
       | Signal_add -> op2 Bits.(+:)
       | Signal_sub -> op2 Bits.(-:)
       | Signal_mulu -> op2 Bits.( *: )
       | Signal_muls -> op2 Bits.( *+ )
       | Signal_and -> op2 Bits.(&:)
       | Signal_or -> op2 Bits.(|:)
       | Signal_xor -> op2 Bits.(^:)
       | Signal_eq -> op2 Bits.(==:)
       | Signal_not -> Some (fun () -> tgt := Bits.(~:) !(List.hd_exn deps))
       | Signal_lt -> op2 Bits.(<:)
       | Signal_cat -> Some (fun () -> tgt := Bits.concat (List.map deps ~f:(!)))
       | Signal_mux ->
         (* tgt := Bits.mux !(List.hd_exn deps) (List.map (!) (List.tl_exn deps)) *)
         (* this optimisation makes a large performance difference *)
         let sel = List.hd_exn deps in
         let els = Array.of_list (List.tl_exn deps) in
         let max = Array.length els - 1 in
         Some (fun () ->
           let sel = Bits.to_int (!sel) in
           let sel = if sel > max then max else sel in
           tgt := !(els.(sel))))
    | Wire _ ->
      let src = List.hd_exn deps in
      Some (fun () -> tgt := !src)
    | Select (_, h, l) ->
      Some (fun () -> tgt := Bits.select !(List.hd_exn deps) h l)
    | Reg (_, r) ->
      let tgt = Map.find_exn reg_map (uid signal) in
      let src = List.hd_exn deps in
      let clr =
        if not (Signal.is_empty r.reg_clear)
        then Some (Map.find_exn data_map (uid r.reg_clear),
                   Map.find_exn data_map (uid r.reg_clear_value),
                   Bits.to_int !(Map.find_exn data_map (uid r.reg_clear_level)))
        else None
      in
      let ena =
        if not (Signal.is_empty r.reg_enable)
        then Some (Map.find_exn data_map (uid r.reg_enable))
        else None
      in
      (match clr, ena with
       | None, None -> Some (fun () -> tgt := !src)
       | Some (c, v, l), None ->
         Some (fun () ->
           if Bits.to_int !c = l
           then tgt := !v
           else tgt := !src)
       | None, Some e -> Some (fun () -> if Bits.to_int !e  = 1 then tgt := !src)
       | Some (c, v, l), Some e ->
         Some (fun () ->
           if Bits.to_int !c = l
           then tgt := !v
           else if Bits.to_int !e = 1
           then tgt := !src))
    | Mem (_, _, _, m) ->
      let mem = Map.find_exn mem_map (uid signal) in
      let addr = Map.find_exn data_map (uid m.mem_read_address) in
      Some (fun () ->
        try
          tgt := mem.(Bits.to_int !addr)
        with _ ->
          tgt := Bits.zero (Signal.width signal))
    | Inst (_, _, i) ->
      match Combinational_ops_database.find
              combinational_ops_database ~name:i.inst_name with
      | None ->
        raise_s [%message
          "Instantiation not supported in simulation" ~name:(i.inst_name : string)]
      | Some op ->
        let f = Combinational_op.create_fn op in
        Some (fun () ->
          tgt := Bits.concat (List.rev (f (List.map deps ~f:(!)))))
  in
  let compile_reg_update (signal : Signal.t) =
    match signal with
    | Reg (_, _) ->
      let tgt = Map.find_exn data_map (uid signal) in
      let src = Map.find_exn reg_map (uid signal) in
      (fun () -> tgt := !src)
    | _ -> failwith "error while compiling reg update"
  in
  let compile_mem_update (signal : Signal.t) =
    match signal with
    | Mem (_, _, r, m) ->
      let mem = Map.find_exn mem_map (uid signal) in
      let we = Map.find_exn data_map (uid r.reg_enable) in
      let w = Map.find_exn data_map (uid m.mem_write_address) in
      let d = Map.find_exn data_map (uid (List.hd_exn (Signal.deps signal))) in
      (fun () ->
         (* XXX memories can have resets/clear etc as well *)
         if Bits.to_int !we = 1
         then (
           let w = Bits.to_int !w in
           if w >= m.mem_size
           then ()
           else mem.(w) <- !d))
    | _ -> failwith "error while compiling mem update"
  in
  let compile_reset (signal : Signal.t) =
    match signal with
    | Reg (_, r) ->
      if not (Signal.is_empty r.reg_reset)
      then
        let tgt0 = Map.find_exn data_map (uid signal) in
        let tgt1 = Map.find_exn reg_map (uid signal) in
        let value = Map.find_exn data_map (uid r.reg_reset_value) in
        Some (fun () ->
          tgt0 := !value;
          tgt1 := !value)
      else
        None
    | _ -> failwith "Only registers should have a reset"
  in
  let check_input signal =
    let signal_width = Signal.width signal in
    let name = List.hd_exn (names signal) in
    let tgt = Map.find_exn data_map (uid signal) in
    (fun () ->
       let data_width = Bits.width !tgt in
       if data_width <> signal_width
       then
         failwith
           (Printf.sprintf "'%s' has width %i but should be of width %i"
              name data_width signal_width))
  in
  (* compile the task list *)
  let tasks_check = List.map bundle.inputs ~f:check_input in
  let tasks_comb = filter_none (List.map bundle.schedule ~f:compile) in
  let tasks_regs = filter_none (List.map bundle.regs ~f:compile) in
  let tasks_seq = (List.map bundle.mems ~f:compile_mem_update) @
                  (List.map bundle.regs ~f:compile_reg_update) in
  (* reset *)
  let resets = filter_none (List.map bundle.regs ~f:compile_reset) in
  let { Io_ports.
        in_ports
      ; out_ports_after_clock_edge
      ; out_ports_before_clock_edge
      ; internal_ports } =
    io_ports ~copy:(fun (n, p) -> n, ref !p) circuit data_map internal_ports
  in
  let task_out_ports_before_clock_edge () =
    List.iter2_exn
      out_ports_before_clock_edge
      out_ports_after_clock_edge
      ~f:(fun (_, pc) (_, pn) -> pc := !pn)
  in
  let lookup_signal uid = Map.find_exn data_map uid in
  let lookup_reg uid = Map.find_exn reg_map uid in
  (* simulator structure *)
  let task tasks = fun () -> (List.iter tasks ~f:(fun f -> f ())) in
  { in_ports
  ; out_ports_after_clock_edge
  ; out_ports_before_clock_edge
  ; internal_ports
  ; inputs                      = in_ports
  ; outputs_after_clock_edge    = out_ports_after_clock_edge
  ; outputs_before_clock_edge   = out_ports_before_clock_edge
  ; cycle_check                 = task tasks_check
  ; cycle_comb0                 = task (tasks_comb
                                        @ tasks_regs
                                        @ [ task_out_ports_before_clock_edge ])
  ; cycle_seq                   = task tasks_seq
  ; cycle_comb1                 = task tasks_comb
  ; reset                       = task resets
  ; lookup_signal
  ; lookup_reg}

module Combine_error = struct
  type t =
    { cycle_no  : int
    ; port_name : string
    ; value0    : Bits.t
    ; value1    : Bits.t }
  [@@deriving sexp_of]
end

let default_on_error error =
  raise_s [%message "[Cyclesim.combine] output port values differ" (error : Combine_error.t)]

let combine
      ?(port_sets_may_differ = false)
      ?(on_error = default_on_error)
      s0 s1 =
  let si =
    Set.to_list (List.fold (s0.in_ports @ s1.in_ports) ~init:(Set.empty (module String))
                   ~f:(fun s (n, _) -> Set.add s n)) in
  let out_port_set =
    Set.to_list (List.fold (s0.out_ports_before_clock_edge
                            @ s1.out_ports_before_clock_edge)
                   ~init:(Set.empty (module String))
                   ~f:(fun s (n, _) -> Set.add s n)) in
  let try_find n s =
    try Some (List.Assoc.find_exn s ~equal:String.equal n) with _ -> None in
  let in_ports, copy_in_ports =
    let l =
      List.map si ~f:(fun n ->
        match try_find n s0.in_ports, try_find n s1.in_ports with
        | Some x, Some y -> (n, x), (fun () -> y := !x)
        | Some x, None when port_sets_may_differ -> (n, x), (fun () -> ())
        | None, Some y when port_sets_may_differ -> (n, y), (fun () -> ())
        | _ -> raise_s [%message "Input port was not found" ~name:(n : string)])
    in
    List.map l ~f:fst, List.map l ~f:snd
  in
  let cycle_no = ref 0 in
  let check n x y =
    if not (Bits.equal !x !y)
    then on_error { Combine_error.
                    cycle_no  = !cycle_no
                  ; port_name = n
                  ; value0    = !x
                  ; value1    = !y }
  in
  let associate_and_check_out_ports out_ports =
    let l =
      List.map out_port_set ~f:(fun n ->
        match
          try_find n (out_ports s0)
        , try_find n (out_ports s1)
        with
        | Some x, Some y -> (n, x), (fun () -> check n x y)
        | Some x, None when port_sets_may_differ -> (n, x), (fun () -> ())
        | None, Some y when port_sets_may_differ -> (n, y), (fun () -> ())
        | _ -> raise_s [%message "Output port was not found" ~name:(n : string)])
    in
    List.map l ~f:fst, List.map l ~f:snd
  in

  let out_ports_after_clock_edge, _ =
    associate_and_check_out_ports out_ports_after_clock_edge
  and out_ports_before_clock_edge, check_out_ports =
    associate_and_check_out_ports out_ports_before_clock_edge
  in

  let copy_in_ports   () = List.iter copy_in_ports   ~f:(fun f -> f ()) in
  let check_out_ports () = List.iter check_out_ports ~f:(fun f -> f ()) in
  let incr_cycle      () = Int.incr cycle_no in
  let cycle_check     () = s0.cycle_check (); copy_in_ports  (); s1.cycle_check () in
  let cycle_comb0     () = s0.cycle_comb0 (); s1.cycle_comb0 () in
  let cycle_seq       () = s0.cycle_seq   (); s1.cycle_seq   () in
  let cycle_comb1     () =
    s0.cycle_comb1  (); s1.cycle_comb1 ();
    check_out_ports (); incr_cycle     () in
  let reset         () = s0.reset (); s1.reset () in
  { s0 with
    in_ports
  ; out_ports_after_clock_edge
  ; out_ports_before_clock_edge
  ; internal_ports              = []
  ; reset
  ; cycle_check
  ; cycle_comb0
  ; cycle_seq
  ; cycle_comb1 }

let create_mutable
      ~is_internal_port
      ~combinational_ops_database
      ~circuit =
  (* add internally traced nodes *)
  let internal_ports = get_internal_ports circuit ~is_internal_port in
  let bundle = get_schedule circuit internal_ports in
  (* build maps *)
  let zero n = Bits.Mutable.const (String.init n ~f:(fun _ -> '0')) in
  let data_map, reg_map, mem_map =
    get_maps ~ref:(fun x -> x) ~const:Bits.Mutable.const ~zero:zero ~bundle
  in
  (* compilation *)
  let compile signal =
    let tgt = Map.find_exn data_map (uid signal) in
    let deps =
      List.map (Signal.deps signal) ~f:(fun signal ->
        try Map.find_exn data_map (uid signal)
        with _ -> Bits.Mutable.empty)
    in
    match signal with
    | Empty -> failwith "cant compile empty signal"
    | Const _ -> None
    | Op (_, op) ->
      let op2 op =
        let a = List.nth_exn deps 0 in
        let b = List.nth_exn deps 1 in
        Some (fun () -> op tgt a b;)
      in
      (match op with
       | Signal_add -> op2 Bits.Mutable.(+:)
       | Signal_sub -> op2 Bits.Mutable.(-:)
       | Signal_mulu -> op2 Bits.Mutable.( *: )
       | Signal_muls -> op2 Bits.Mutable.( *+ )
       | Signal_and -> op2 Bits.Mutable.(&:)
       | Signal_or -> op2 Bits.Mutable.(|:)
       | Signal_xor -> op2 Bits.Mutable.(^:)
       | Signal_eq -> op2 Bits.Mutable.(==:)
       | Signal_not -> Some (fun () -> Bits.Mutable.(~:) tgt (List.hd_exn deps))
       | Signal_lt -> op2 Bits.Mutable.(<:)
       | Signal_cat -> Some (fun () -> Bits.Mutable.concat tgt deps)
       | Signal_mux ->
         (* tgt := Bits.Mutable.mux !(List.hd_exn deps) (List.map (!) (List.tl_exn deps)) *)
         (* this optimisation makes a large performance difference *)
         let sel = List.hd_exn deps in
         let els = Array.of_list (List.tl_exn deps) in
         let max = Array.length els - 1 in
         Some (fun () ->
           let sel = Bits.Mutable.to_int sel in
           let sel = if sel > max then max else sel in
           Bits.Mutable.copy ~dst:tgt ~src:els.(sel)))
    | Wire _ ->
      let src = List.hd_exn deps in
      Some (fun () -> Bits.Mutable.copy ~dst:tgt ~src)
    | Select (_, h, l) ->
      let d = List.hd_exn deps in
      Some (fun () -> Bits.Mutable.select tgt d h l)
    | Reg (_, r) ->
      let tgt = Map.find_exn reg_map (uid signal) in
      let src = List.hd_exn deps in
      let clr =
        if not (Signal.is_empty r.reg_clear)
        then Some (Map.find_exn data_map (uid r.reg_clear),
                   Map.find_exn data_map (uid r.reg_clear_value),
                   Bits.Mutable.to_int (Map.find_exn data_map (uid r.reg_clear_level)))
        else None
      in
      let ena =
        if not (Signal.is_empty r.reg_enable)
        then Some (Map.find_exn data_map (uid r.reg_enable))
        else None
      in
      (match clr, ena with
       | None, None -> Some (fun () -> Bits.Mutable.copy ~dst:tgt ~src)
       | Some (c, v, l), None ->
         Some (fun () ->
           if Bits.Mutable.to_int c = l
           then Bits.Mutable.copy ~dst:tgt ~src:v
           else Bits.Mutable.copy ~dst:tgt ~src)
       | None, Some e ->
         Some (fun () -> if Bits.Mutable.to_int e  = 1 then Bits.Mutable.copy ~dst:tgt ~src)
       | Some (c, v, l), Some e ->
         Some (fun () ->
           if Bits.Mutable.to_int c = l
           then Bits.Mutable.copy ~dst:tgt ~src:v
           else if Bits.Mutable.to_int e = 1
           then Bits.Mutable.copy ~dst:tgt ~src))
    | Mem (_, _, _, m) ->
      let mem = Map.find_exn mem_map (uid signal) in
      let addr = Map.find_exn data_map (uid m.mem_read_address) in
      Some (fun () ->
        try
          Bits.Mutable.copy ~dst:tgt ~src:mem.(Bits.Mutable.to_int addr)
        with _ ->
          Bits.Mutable.copy ~dst:tgt ~src:(zero (Signal.width signal)))
    | Inst (_, _, i) ->
      match Combinational_ops_database.find
              combinational_ops_database ~name:i.inst_name with
      | None ->
        raise_s [%message
          "Instantiation not supported in simulation" ~name:(i.inst_name : string)]
      | Some op ->
        let f = Combinational_op.create_fn_mutable op in
        let outputs =
          List.map i.inst_outputs ~f:(fun (_,(b,_)) -> Bits.Mutable.create b) in
        Some (fun () ->
          f deps outputs;
          Bits.Mutable.concat tgt (List.rev outputs))
  in
  let compile_reg_update (signal : Signal.t) =
    match signal with
    | Reg (_, _) ->
      let tgt = Map.find_exn data_map (uid signal) in
      let src = Map.find_exn reg_map (uid signal) in
      (fun () -> Bits.Mutable.copy ~dst:tgt ~src)
    | _ -> failwith "error while compiling reg update"
  in
  let compile_mem_update (signal : Signal.t) =
    match signal with
    | Mem (_, _, r, m) ->
      let mem = Map.find_exn mem_map (uid signal) in
      let we = Map.find_exn data_map (uid r.reg_enable) in
      let w = Map.find_exn data_map (uid m.mem_write_address) in
      let d = Map.find_exn data_map (uid (List.hd_exn (Signal.deps signal))) in
      (fun () ->
         (* XXX memories can have resets/clear etc as well *)
         if Bits.Mutable.to_int we = 1
         then (
           let w = Bits.Mutable.to_int w in
           if w >= m.mem_size
           then
             (*Printf.printf "memory write out of bounds %i/%i\n" w m.mem_size*)
             ()
           else
             Bits.Mutable.copy ~dst:mem.(w) ~src:d))
    | _ -> failwith "error while compiling mem update"
  in
  let compile_reset (signal : Signal.t) =
    match signal with
    | Reg (_, r) ->
      if not (Signal.is_empty r.reg_reset)
      then
        let tgt0 = Map.find_exn data_map (uid signal) in
        let tgt1 = Map.find_exn reg_map (uid signal) in
        let value = Map.find_exn data_map (uid r.reg_reset_value) in
        Some (fun () ->
          Bits.Mutable.copy ~dst:tgt0 ~src:value;
          Bits.Mutable.copy ~dst:tgt1 ~src:value)
      else
        None
    | _ -> failwith "Only registers should have a reset"
  in
  let check_input signal =
    let signal_width = Signal.width signal in
    let name = List.hd_exn (names signal) in
    let tgt = Map.find_exn data_map (uid signal) in
    (fun () ->
       let data_width = Bits.Mutable.width tgt in
       if data_width <> signal_width
       then
         failwith
           (Printf.sprintf "'%s' has width %i but should be of width %i"
              name data_width signal_width))
  in
  (* compile the task list *)
  let tasks_check = List.map bundle.inputs ~f:check_input in
  let tasks_comb = filter_none (List.map bundle.schedule ~f:compile) in
  let tasks_regs = filter_none (List.map bundle.regs ~f:compile) in
  let tasks_seq = (List.map bundle.mems ~f:compile_mem_update) @
                  (List.map bundle.regs ~f:compile_reg_update) in
  (* reset *)
  let resets = filter_none (List.map bundle.regs ~f:compile_reset) in
  (* Note; in the functional simulator the out ports before/after the clock edge are the
     same here. *)
  let { Io_ports. in_ports; out_ports_after_clock_edge = out_ports; internal_ports; _ } =
    io_ports ~copy:Fn.id circuit data_map internal_ports
  in
  let lookup_signal uid = ref (Map.find_exn data_map uid |> Bits.Mutable.to_bits) in
  let lookup_reg uid = ref (Map.find_exn reg_map uid |> Bits.Mutable.to_bits) in
  (* simulator structure *)
  let task tasks = fun () -> (List.iter tasks ~f:(fun f -> f ())) in
  let tasks tasks =
    let t = List.map tasks ~f:(fun t -> task t) in
    fun () -> List.iter t ~f:(fun t -> t ())
  in
  let in_ports, in_ports_task =
    let i = List.map in_ports ~f:(fun (n, b) -> n, ref (Bits.zero (Bits.Mutable.width b))) in
    let t () =
      List.iter2_exn in_ports i ~f:(fun (_, tgt) (_, src) ->
        Bits.Mutable.copy_bits ~dst:tgt ~src:!src) in
    i, t
  in
  let out_ports_ref ports =
    let p = List.map ports ~f:(fun (n, s) -> n, ref (Bits.zero (Bits.Mutable.width s))) in
    let task () =
      List.iter2_exn p ports ~f:(fun (_, rf) (_, v) -> rf := Bits.Mutable.to_bits v)
    in
    p, task
  in
  let out_ports_before_clock_edge, out_ports_before_clock_edge_task =
    out_ports_ref out_ports in
  let out_ports_after_clock_edge, out_ports_task =
    out_ports_ref out_ports in
  let internal_ports, internal_ports_task = out_ports_ref internal_ports in
  { in_ports
  ; out_ports_after_clock_edge
  ; out_ports_before_clock_edge
  ; internal_ports
  ; inputs                      = in_ports
  ; outputs_after_clock_edge    = out_ports_after_clock_edge
  ; outputs_before_clock_edge   = out_ports_before_clock_edge
  ; cycle_check                 = task tasks_check
  ; cycle_comb0                 = tasks [ [ in_ports_task ]
                                        ; tasks_comb
                                        ; tasks_regs
                                        ; [ out_ports_before_clock_edge_task
                                          ; internal_ports_task]]
  ; cycle_seq                   = task tasks_seq
  ; cycle_comb1                 = tasks [ tasks_comb; [ out_ports_task ]]
  ; reset                       = task resets
  ; lookup_signal
  ; lookup_reg }

let create
      ?is_internal_port
      ?(combinational_ops_database=empty_ops_database)
      ~(kind : Kind.t) circuit =
  (match kind with
   | Immutable -> create_immutable
   | Mutable   -> create_mutable)
    ~is_internal_port ~combinational_ops_database ~circuit

module With_interface (I : Interface.S) (O : Interface.S) = struct

  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t [@@deriving sexp_of]

  module C = Circuit.With_interface (I) (O)

  let coerce sim =
    let find_port ports (name, width) =
      match List.Assoc.find ports name ~equal:String.equal with
      | Some x -> x
      | None -> ref (Bits.zero width) in
    let to_input ports  = I.map I.t ~f:(find_port ports) in
    let to_output ports = O.map O.t ~f:(find_port ports) in
    Private.coerce sim ~to_input ~to_output

  let create =
    Circuit.with_create_options
      (fun
        create_options
        ?is_internal_port
        ?combinational_ops_database
        ~(kind : Kind.t) ?port_checks create_fn ->
        let circuit =
          Circuit.call_with_create_options
            C.create_exn create_options ?port_checks
            ~name:(match kind with
              | Immutable -> "immutable_simulator"
              | Mutable   -> "mutable_simulator")
            create_fn
        in
        let sim = create ?is_internal_port ?combinational_ops_database ~kind circuit in
        coerce sim)
end
