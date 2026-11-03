open! Import
open Signal

type scenario =
  { num_ports : int
  ; internal_nodes_per_port : int
  ; named_internal_period : int
  ; width : int
  ; cycles : int
  ; event_period : int
  }

let width = 32
let event_period = 32

let value_mask width =
  if width >= Int.num_bits - 1 then Int.max_value else (1 lsl width) - 1
;;

let bits_for_cycle ~width ~port_index ~event_index =
  let value = ((event_index * 1_103) + (port_index * 9_973)) land value_mask width in
  Bits.of_unsigned_int ~width value
;;

let circuit_name
  { num_ports
  ; internal_nodes_per_port
  ; named_internal_period
  ; width
  ; cycles
  ; event_period
  }
  =
  Printf.sprintf
    "waveform_trace_%dp_%dn_%d_named_%dw_%dc_%dep"
    num_ports
    internal_nodes_per_port
    named_internal_period
    width
    cycles
    event_period
;;

let port_name prefix index = Printf.sprintf "%s_%04d" prefix index

let internal_name port_index node_index =
  Printf.sprintf "n_%04d_%04d" port_index node_index
;;

let set_name name signal =
  Signal.set_names signal [ { Name_and_loc.name; loc = [%here] } ];
  signal
;;

let create_circuit scenario =
  let inputs =
    Array.init scenario.num_ports ~f:(fun i -> input (port_name "i" i) scenario.width)
  in
  let outputs =
    Array.mapi inputs ~f:(fun port_index input ->
      let signal = ref input in
      for node_index = 0 to scenario.internal_nodes_per_port - 1 do
        let next_signal =
          !signal
          +: of_unsigned_int
               ~width:scenario.width
               ((port_index + node_index + 1) land value_mask scenario.width)
        in
        signal
        := if node_index % scenario.named_internal_period = 0
           then set_name (internal_name port_index node_index) next_signal
           else next_signal
      done;
      output (port_name "o" port_index) !signal)
    |> Array.to_list
  in
  Circuit.create_exn ~name:(circuit_name scenario) outputs
;;

let create_sim scenario =
  Cyclesim.create ~config:(Cyclesim.Config.trace `All_named) (create_circuit scenario)
;;

let drive_inputs scenario inputs cycle =
  if cycle % scenario.event_period = 0
  then (
    let event_index = cycle / scenario.event_period in
    Array.iteri inputs ~f:(fun port_index input ->
      input := bits_for_cycle ~width:scenario.width ~port_index ~event_index))
;;

let run scenario =
  let waves, sim = create_sim scenario |> Cyclesim.Waveform.create in
  let inputs =
    Array.init scenario.num_ports ~f:(fun i -> Cyclesim.in_port sim (port_name "i" i))
  in
  for cycle = 0 to scenario.cycles - 1 do
    drive_inputs scenario inputs cycle;
    Cyclesim.cycle sim
  done;
  ignore (Stdlib.Sys.opaque_identity waves : Wave_data.t)
;;

let bench scenario () = run scenario

let cycles ~num_ports ~internal_nodes_per_port =
  match num_ports, internal_nodes_per_port with
  | 10, 10 -> 131_072
  | 10, 100 -> 24_576
  | 10, 500 -> 4_096
  | 40, 10 -> 24_576
  | 40, 100 -> 4_096
  | 40, 500 -> 512
  | _ -> 512
;;

let scenario ~num_ports ~internal_nodes_per_port ~named_internal_period =
  { num_ports
  ; internal_nodes_per_port
  ; named_internal_period
  ; width
  ; cycles = cycles ~num_ports ~internal_nodes_per_port
  ; event_period
  }
;;

let n10_m10_p2 =
  scenario ~num_ports:10 ~internal_nodes_per_port:10 ~named_internal_period:2
;;

let n10_m10_p4 =
  scenario ~num_ports:10 ~internal_nodes_per_port:10 ~named_internal_period:4
;;

let n10_m10_p8 =
  scenario ~num_ports:10 ~internal_nodes_per_port:10 ~named_internal_period:8
;;

let n10_m100_p2 =
  scenario ~num_ports:10 ~internal_nodes_per_port:100 ~named_internal_period:2
;;

let n10_m100_p4 =
  scenario ~num_ports:10 ~internal_nodes_per_port:100 ~named_internal_period:4
;;

let n10_m100_p8 =
  scenario ~num_ports:10 ~internal_nodes_per_port:100 ~named_internal_period:8
;;

let n10_m500_p2 =
  scenario ~num_ports:10 ~internal_nodes_per_port:500 ~named_internal_period:2
;;

let n10_m500_p4 =
  scenario ~num_ports:10 ~internal_nodes_per_port:500 ~named_internal_period:4
;;

let n10_m500_p8 =
  scenario ~num_ports:10 ~internal_nodes_per_port:500 ~named_internal_period:8
;;

let n40_m10_p2 =
  scenario ~num_ports:40 ~internal_nodes_per_port:10 ~named_internal_period:2
;;

let n40_m10_p4 =
  scenario ~num_ports:40 ~internal_nodes_per_port:10 ~named_internal_period:4
;;

let n40_m10_p8 =
  scenario ~num_ports:40 ~internal_nodes_per_port:10 ~named_internal_period:8
;;

let n40_m100_p2 =
  scenario ~num_ports:40 ~internal_nodes_per_port:100 ~named_internal_period:2
;;

let n40_m100_p4 =
  scenario ~num_ports:40 ~internal_nodes_per_port:100 ~named_internal_period:4
;;

let n40_m100_p8 =
  scenario ~num_ports:40 ~internal_nodes_per_port:100 ~named_internal_period:8
;;

let n40_m500_p2 =
  scenario ~num_ports:40 ~internal_nodes_per_port:500 ~named_internal_period:2
;;

let n40_m500_p4 =
  scenario ~num_ports:40 ~internal_nodes_per_port:500 ~named_internal_period:4
;;

let n40_m500_p8 =
  scenario ~num_ports:40 ~internal_nodes_per_port:500 ~named_internal_period:8
;;

let%bench_fun "waveform tracing/mutable-event/10/10/p2" = bench n10_m10_p2
let%bench_fun "waveform tracing/mutable-event/10/10/p4" = bench n10_m10_p4
let%bench_fun "waveform tracing/mutable-event/10/10/p8" = bench n10_m10_p8
let%bench_fun "waveform tracing/mutable-event/10/100/p2" = bench n10_m100_p2
let%bench_fun "waveform tracing/mutable-event/10/100/p4" = bench n10_m100_p4
let%bench_fun "waveform tracing/mutable-event/10/100/p8" = bench n10_m100_p8
let%bench_fun "waveform tracing/mutable-event/10/500/p2" = bench n10_m500_p2
let%bench_fun "waveform tracing/mutable-event/10/500/p4" = bench n10_m500_p4
let%bench_fun "waveform tracing/mutable-event/10/500/p8" = bench n10_m500_p8
let%bench_fun "waveform tracing/mutable-event/40/10/p2" = bench n40_m10_p2
let%bench_fun "waveform tracing/mutable-event/40/10/p4" = bench n40_m10_p4
let%bench_fun "waveform tracing/mutable-event/40/10/p8" = bench n40_m10_p8
let%bench_fun "waveform tracing/mutable-event/40/100/p2" = bench n40_m100_p2
let%bench_fun "waveform tracing/mutable-event/40/100/p4" = bench n40_m100_p4
let%bench_fun "waveform tracing/mutable-event/40/100/p8" = bench n40_m100_p8
let%bench_fun "waveform tracing/mutable-event/40/500/p2" = bench n40_m500_p2
let%bench_fun "waveform tracing/mutable-event/40/500/p4" = bench n40_m500_p4
let%bench_fun "waveform tracing/mutable-event/40/500/p8" = bench n40_m500_p8
