open! Core0
include Cyclesim_intf

(* types defined in Cyclesim0. *)

module Port_list = Cyclesim0.Port_list

(* Implementation details. *)
module Private = struct
  include Cyclesim0.Private
  module Traced_nodes = Cyclesim0.Traced_nodes
end

module Traced = Cyclesim0.Traced
module Node = Cyclesim0.Node
module Reg = Cyclesim0.Reg
module Memory = Cyclesim0.Memory

type t_port_list = Cyclesim0.t_port_list
type ('i, 'o) t = ('i, 'o) Cyclesim0.t [@@deriving sexp_of]

let in_ports = Cyclesim0.in_ports
let inputs = Cyclesim0.inputs
let traced t = Cyclesim0.traced t
let lookup_node = Cyclesim0.lookup_node
let lookup_reg = Cyclesim0.lookup_reg
let lookup_mem = Cyclesim0.lookup_mem

module Config = Cyclesim0.Config

let circuit (sim : _ t) = sim.circuit
let cycle_check (sim : _ t) = sim.cycle_check ()
let cycle_before_clock_edge (sim : _ t) = sim.cycle_before_clock_edge ()
let cycle_at_clock_edge (sim : _ t) = sim.cycle_at_clock_edge ()
let cycle_after_clock_edge (sim : _ t) = sim.cycle_after_clock_edge ()
let reset (sim : _ t) = sim.reset ()

let cycle ?(n = 1) (sim : _ t) =
  let n = n * sim.cycle_multiple in
  for _ = 1 to n do
    cycle_check sim;
    cycle_before_clock_edge sim;
    cycle_at_clock_edge sim;
    cycle_after_clock_edge sim
  done
;;

let clock_mode (sim : _ t) = sim.clock_mode

let cycle_until_clocks_aligned (sim : _ t) =
  while not (sim.clocks_aligned ()) do
    cycle sim
  done
;;

let raise_after_timeout ?message ~(here : [%call_pos]) sim ~timeout =
  let cycle' = ref 0 in
  Private.modify
    sim
    [ (Side.Before, Reset, fun () -> cycle' := 0)
    ; ( Side.Before
      , Before_clock_edge
      , fun () ->
          let cycle = !cycle' in
          Int.incr cycle';
          if cycle = timeout
          then
            raise_s
              [%message.omit_nil
                "Cyclesim timed out"
                  ~_:(message : string option)
                  (timeout : int)
                  ~timeout_set_at:(here : Source_code_position.t)] )
    ]
;;

let with_timeout ?message ~(here : [%call_pos]) ~timeout ~f sim =
  f (raise_after_timeout ?message ~here ~timeout sim)
;;

let in_port (sim : _ Cyclesim0.t) name =
  try List.Assoc.find_exn sim.in_ports name ~equal:String.equal with
  | _ -> raise_s [%message "Couldn't find input port" name]
;;

let lookup_node_by_name (sim : _ Cyclesim0.t) name =
  let map = Lazy.force sim.node_by_name in
  let%bind.Option name = Map.find map name in
  lookup_node sim name
;;

let lookup_reg_by_name (sim : _ Cyclesim0.t) name =
  let map = Lazy.force sim.reg_by_name in
  let%bind.Option name = Map.find map name in
  lookup_reg sim name
;;

let lookup_mem_by_name (sim : _ Cyclesim0.t) name =
  let map = Lazy.force sim.memory_by_name in
  let%bind.Option name = Map.find map name in
  lookup_mem sim name
;;

let lookup_node_or_reg (sim : _ Cyclesim0.t) traced =
  match lookup_node sim traced with
  | None -> lookup_reg sim traced |> Option.map ~f:Reg.to_node
  | Some t -> Some t
;;

let lookup_node_or_reg_by_name (sim : _ Cyclesim0.t) name =
  match lookup_node_by_name sim name with
  | None -> lookup_reg_by_name sim name |> Option.map ~f:Reg.to_node
  | Some t -> Some t
;;

let out_port_after_clock_edge (sim : _ Cyclesim0.t) name =
  try List.Assoc.find_exn sim.out_ports_after_clock_edge name ~equal:String.equal with
  | _ -> raise_s [%message "Couldn't find output port" name]
;;

let out_port_before_clock_edge (sim : _ Cyclesim0.t) name =
  try List.Assoc.find_exn sim.out_ports_before_clock_edge name ~equal:String.equal with
  | _ -> raise_s [%message "Couldn't find output port" name]
;;

let out_port ?(clock_edge = Side.After) t name =
  match clock_edge with
  | Before -> out_port_before_clock_edge t name
  | After -> out_port_after_clock_edge t name
;;

let out_ports ?(clock_edge = Side.After) (t : _ t) =
  match clock_edge with
  | Before -> t.out_ports_before_clock_edge
  | After -> t.out_ports_after_clock_edge
;;

let outputs ?(clock_edge = Side.After) (t : _ t) =
  match clock_edge with
  | Before -> t.outputs_before_clock_edge
  | After -> t.outputs_after_clock_edge
;;

(* Cyclesim_combine *)

module Combine_error = Cyclesim_combine.Combine_error

let combine = Cyclesim_combine.combine

(* compilation *)

let create' ?config circuit =
  let sim = Cyclesim_compile.create ?config circuit in
  Cyclesim_coverage.For_cyclesim.maybe_wrap sim circuit
;;

let create ?config circuit = create' ?config circuit

(* interfaces *)

module With_interface (I : Interface.S) (O : Interface.S) = struct
  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t [@@deriving sexp_of]

  module C = Circuit.With_interface (I) (O)

  let coerce sim =
    let find_port (ports : Cyclesim0.Port_list.t) (name, width) =
      match List.Assoc.find ports name ~equal:String.equal with
      | Some x -> x
      | None -> ref (Bits.zero width)
    in
    let to_input ports = I.map I.port_names_and_widths ~f:(find_port ports) in
    let to_output ports = O.map O.port_names_and_widths ~f:(find_port ports) in
    Private.coerce sim ~to_input ~to_output
  ;;

  let create ?config ?circuit_config ?(name = "simulator") create_fn =
    let circuit_config =
      (* Because the circuit will only be used for simulations, we can disable a couple of
         passes we would otherwise want - combinational loop checks (will be done during
         the simulation topsort anyway) and rewriting uids which is only really relevant
         for rtl generation. *)
      match circuit_config with
      | None -> Circuit.Config.default_for_simulations
      | Some config -> config
    in
    let circuit = C.create_exn ~config:circuit_config ~name create_fn in
    let sim = create ?config circuit in
    coerce sim
  ;;
end

module Sim_bits = struct
  include Comb.Make (struct
      type t = Bits.t ref [@@deriving sexp_of, equal ~localize, compare ~localize]

      let empty = ref Bits.empty
      let is_empty t = Bits.is_empty !t
      let width t = Bits.width !t
      let of_constant t = ref (Bits.of_constant t)
      let to_constant t = Bits.to_constant !t
      let vdd = ref Bits.vdd
      let gnd = ref Bits.gnd
      let concat_msb t = ref (Bits.concat_msb (List.map t ~f:( ! )))
      let ( -- ) ~(loc : [%call_pos]) t name = ref (Bits.( -- ) ~loc !t name)
      let select t ~high ~low = ref (Bits.select !t ~high ~low)
      let op2 f a b = ref (f !a !b)
      let ( &: ) = op2 Bits.( &: )
      let ( |: ) = op2 Bits.( |: )
      let ( ^: ) = op2 Bits.( ^: )
      let ( ~: ) a = ref (Bits.( ~: ) !a)
      let to_string t = Bits.to_string !t
      let mux sel lst = ref (Bits.mux !sel (List.map lst ~f:( ! )))
      let ( +: ) = op2 Bits.( +: )
      let ( -: ) = op2 Bits.( -: )
      let ( <: ) = op2 Bits.( <: )
      let ( ==: ) = op2 Bits.( ==: )
      let ( *: ) = op2 Bits.( *: )
      let ( *+ ) = op2 Bits.( *+ )

      let cases ~default select t =
        ref (Bits.cases ~default:!default !select (List.map t ~f:(fun (s, d) -> !s, !d)))
      ;;
    end)

  let ( <-- ) (a : t) (b : t) = a := !b
  let ( <--. ) = Bits.( <--. )
  let ( <-:. ) = Bits.( <-:. )
  let ( <-+. ) = Bits.( <-+. )
end

module Waveform = struct
  module Data = Wave_data_in_cycles

  let lookup_node sim cycle (t : Traced.internal_signal) =
    let width = Signal.width t.signal in
    let data = Data.create width in
    let v = lookup_node_or_reg sim t in
    Option.map v ~f:(fun v ->
      let d = Node.data v in
      let byte_address = Node.byte_address v in
      let set_from_bytes = Data.set_from_bytes width in
      data, fun _ -> set_from_bytes data !cycle d byte_address)
  ;;

  let lookup_port find_port sim cycle (t : Traced.io_port) =
    let width = Signal.width t.signal in
    let data = Data.create width in
    let v = find_port sim t.name in
    data, fun _ -> Data.set data !cycle !v
  ;;

  let lookup_in_port sim cycle t = lookup_port in_port sim cycle t
  let lookup_out_port sim cycle t = lookup_port (out_port ~clock_edge:Before) sim cycle t

  let create_wave ~signal ~name ~typ ~is_pseudo_clock wave_data =
    { Wave_data.Wave.name
    ; width = Signal.width signal
    ; typ
    ; wave_format = Signal.Type.get_wave_format signal
    ; is_pseudo_clock
    ; wave_data
    }
  ;;

  let is_clock x ~clock_mode =
    match clock_mode with
    | `All_one_domain ->
      String.equal "clock" x
      || String.equal "clk" x
      || String.is_suffix ~suffix:"$clock" x
      || String.is_suffix ~suffix:"_clock" x
    | `By_input_clocks ->
      (* Clocks are driven in the sim and don't need to be special cased here for the
         waveform *)
      false
  ;;

  let is_reset = function
    | "reset" | "rst" -> true
    | _ -> false
  ;;

  let trace sim cycle ~clock_mode =
    let traced = traced sim in
    let io_port typ lookup (t : Traced.io_port) =
      if is_clock t.name ~clock_mode
      then
        ( create_wave
            ~signal:t.signal
            ~name:t.name
            ~typ
            ~is_pseudo_clock:true
            (Data.create 1)
        , None )
      else if is_reset t.name
      then (
        let data, _ = lookup sim cycle t in
        ( create_wave ~signal:t.signal ~name:t.name ~typ ~is_pseudo_clock:false data
        , Some (fun v -> Data.set data !cycle (if v then Bits.vdd else Bits.gnd)) ))
      else (
        let data, update = lookup sim cycle t in
        ( create_wave ~signal:t.signal ~name:t.name ~typ ~is_pseudo_clock:false data
        , Some update ))
    in
    let internal_signal (t : Traced.internal_signal) =
      Option.value_map (lookup_node sim cycle t) ~default:[] ~f:(fun (data, update) ->
        List.mapi t.mangled_names ~f:(fun i name ->
          if is_clock name ~clock_mode
          then
            ( create_wave
                ~signal:t.signal
                ~name
                ~typ:Internal
                ~is_pseudo_clock:true
                (Data.create 1)
            , None )
          else (
            (* Only trace each signal once, but add an entry per name that all points to
               the same data *)
            let update = if i = 0 then Some update else None in
            ( create_wave ~signal:t.signal ~name ~typ:Internal ~is_pseudo_clock:false data
            , update ))))
    in
    List.concat
      [ List.map traced.input_ports ~f:(io_port Input lookup_in_port)
      ; List.map traced.output_ports ~f:(io_port Output lookup_out_port)
      ; List.map traced.internal_signals ~f:internal_signal |> List.concat
      ]
  ;;

  let wrap sim =
    let cycle = ref 0 in
    let traced = trace sim cycle ~clock_mode:(clock_mode sim) in
    let waves = Array.of_list_map traced ~f:fst in
    let updates = List.filter_map traced ~f:snd |> Array.of_list in
    let tasks rst () =
      Array.iter ~f:(fun f -> f rst) updates;
      Int.incr cycle
    in
    let sim =
      Private.modify sim [ After, Reset, tasks true; Before, At_clock_edge, tasks false ]
    in
    sim, waves
  ;;

  let create sim =
    let sim, waves = wrap sim in
    Wave_data.By_cycle waves, sim
  ;;

  let create_if ~enabled sim =
    if enabled
    then (
      let waves, sim = create sim in
      Some waves, sim)
    else None, sim
  ;;
end
