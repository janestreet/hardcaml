open! Import
include Cyclesim_intf

(* types defined in Cyclesim0. *)

module Port_list = Cyclesim0.Port_list
module Private = Cyclesim0.Private

type t_port_list = Cyclesim0.t_port_list
type ('i, 'o) t = ('i, 'o) Cyclesim0.t [@@deriving sexp_of]

let lookup_reg = Cyclesim0.lookup_reg
let lookup_signal = Cyclesim0.lookup_signal
let internal_ports = Cyclesim0.internal_ports
let in_ports = Cyclesim0.in_ports
let inputs = Cyclesim0.inputs

module Violated_or_not = struct
  type t =
    | Violated of int list
    | Not_violated
  [@@deriving sexp_of]
end

let results_of_assertions (t : _ t) =
  Map.mapi t.assertions ~f:(fun ~key ~data:_ ->
    match Hashtbl.find t.violated_assertions key with
    | Some cycles -> Violated_or_not.Violated (List.rev cycles)
    | None -> Violated_or_not.Not_violated)
;;

module Config = Cyclesim0.Config

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

let create = Cyclesim_compile.create

(* interfaces *)

module With_interface (I : Interface.S) (O : Interface.S) = struct
  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t [@@deriving sexp_of]

  module C = Circuit.With_interface (I) (O)

  let coerce sim =
    let find_port ports (name, width) =
      match List.Assoc.find ports name ~equal:String.equal with
      | Some x -> x
      | None -> ref (Bits.zero width)
    in
    let to_input ports = I.map I.t ~f:(find_port ports) in
    let to_output ports = O.map O.t ~f:(find_port ports) in
    Private.coerce sim ~to_input ~to_output
  ;;

  let create ?config ?circuit_config create_fn =
    let circuit = C.create_exn ?config:circuit_config ~name:"simulator" create_fn in
    let sim = create ?config circuit in
    coerce sim
  ;;
end
