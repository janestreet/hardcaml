open Base
include Cyclesim_intf

(* types defined in Cyclesim0. *)

module Port_list = Cyclesim0.Port_list

(* Implementation details. *)
module Private = struct
  include Cyclesim0.Private
  module Traced_nodes = Cyclesim_compile.Traced_nodes
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

let cycle sim =
  cycle_check sim;
  cycle_before_clock_edge sim;
  cycle_at_clock_edge sim;
  cycle_after_clock_edge sim
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

let create ?(implementation = `V2) =
  match implementation with
  | `V1 -> Cyclesim_compile.create
  | `V2 -> Cyclesim2.create
;;

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

  let create ?implementation ?config ?circuit_config create_fn =
    let circuit_config =
      (* Because the circuit will only be used for simulations, we can disable a couple of
         passes we would otherwise want - combinational loop checks (will be done during
         the simulation topsort anyway) and rewriting uids which is only really relevant
         for rtl generation. *)
      match circuit_config with
      | None -> Circuit.Config.default_for_simulations
      | Some config -> config
    in
    let circuit = C.create_exn ~config:circuit_config ~name:"simulator" create_fn in
    let sim = create ?implementation ?config circuit in
    coerce sim
  ;;
end
