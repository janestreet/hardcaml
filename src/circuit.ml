(* A circuit is defined by transitively following the dependencies of its outputs,
   stopping at unassigned wires or constants.  [Signal_graph.inputs] does this.
   All such unassigned wires are circuit inputs.  As a consequence, all other wires
   in a circuit are assigned, and hence cannot be changed. *)

open! Import

module Uid_map = Signal.Uid_map
module Uid_set = Signal.Uid_set

module Signal_map = struct
  type t = Signal.t Uid_map.t [@@deriving sexp_of]

  let create graph =
    let add_signal map signal =
      let uid = Signal.uid signal in
      Map.add_exn map ~key:uid ~data:signal
    in
    Signal_graph.depth_first_search graph ~init:Uid_map.empty ~f_before:add_signal
end

type t =
  { name          : string
  ; signal_by_uid : Signal_map.t
  ; inputs        : Signal.t list
  ; outputs       : Signal.t list
  ; signal_graph  : Signal_graph.t
  (* [fan_in] and [fan_out] are lazily computed.  One might worry that this would interact
     poorly with signals, which have some mutable components (e.g. wires).  But those have
     already been set by the time a circuit is created, so a circuit is not mutable. *)
  ; fan_out       : Signal.Uid_set.t Signal.Uid_map.t Lazy.t
  ; fan_in        : Signal.Uid_set.t Signal.Uid_map.t Lazy.t }
[@@deriving fields, sexp_of]

type 'a with_create_options
  =  ?detect_combinational_loops : bool
  -> ?normalize_uids : bool
  -> 'a

module Create_options = struct
  type t =
    { detect_combinational_loops : bool option
    ; normalize_uids             : bool option }
  [@@deriving sexp_of]
end

let with_create_options f ?detect_combinational_loops ?normalize_uids =
  f { Create_options.detect_combinational_loops; normalize_uids }

let call_with_create_options t
      { Create_options. detect_combinational_loops; normalize_uids } =
  t ?detect_combinational_loops ?normalize_uids

let create_exn
      ?(detect_combinational_loops = true)
      ?(normalize_uids = true)
      ~name outputs =
  let signal_graph = Signal_graph.create outputs in
  (* check that all outputs are assigned wires with 1 name *)
  ignore (ok_exn (Signal_graph.outputs ~validate:true signal_graph) : Signal.t list);
  (* uid normalization *)
  let signal_graph =
    if normalize_uids
    then Signal_graph.normalize_uids signal_graph
    else signal_graph
  in
  (* get new output wires *)
  let outputs = Signal_graph.outputs signal_graph |> ok_exn in
  (* get inputs checking that they are valid *)
  let inputs = ok_exn (Signal_graph.inputs signal_graph) in
  (* check for combinational loops *)
  if detect_combinational_loops
  then ok_exn (Signal_graph.detect_combinational_loops signal_graph);
  (* construct the circuit *)
  { name          = name
  ; signal_by_uid = Signal_map.create signal_graph
  ; inputs
  ; outputs
  ; signal_graph
  ; fan_out       = lazy (Signal_graph.fan_out_map signal_graph)
  ; fan_in        = lazy (Signal_graph.fan_in_map  signal_graph) }

let with_name t ~name = { t with name }

let uid_equal a b = Int64.equal (Signal.uid a) (Signal.uid b)

let is_input  t signal = List.mem t.inputs  signal ~equal:uid_equal
let is_output t signal = List.mem t.outputs signal ~equal:uid_equal

let find_signal_exn t uid = Map.find_exn t.signal_by_uid uid

let fan_out_map t = Lazy.force t.fan_out
let fan_in_map  t = Lazy.force t.fan_in

let signal_map c = c.signal_by_uid

let structural_compare ?check_names c0 c1 =
  try
    (* outputs, including names, are the same *)
    List.fold2_exn (outputs c0) (outputs c1) ~init:true ~f:(fun b o0 o1 ->
      b
      && [%compare.equal: string list] (Signal.names o0) (Signal.names o1)
      && Signal.width o0 = Signal.width o1)
    &&
    (* inputs, including names, are the same *)
    List.fold2_exn (inputs c0) (inputs c1) ~init:true ~f:(fun b i0 i1 ->
      b
      &&  [%compare.equal: string list] (Signal.names i0) (Signal.names i1)
      && Signal.width i0 = Signal.width i1)
    &&
    (* check full structural comparision from each output *)
    List.fold2_exn (outputs c0) (outputs c1) ~init:true ~f:(fun b s t ->
      b && (Signal.structural_compare ?check_names s t))
  with Not_found_s _ | Caml.Not_found ->
    false

module With_interface (I : Interface.S) (O : Interface.S) = struct
  type create = Signal.t Interface.Create_fn(I)(O).t

  let create_exn =
    with_create_options (fun create_options ~name logic ->
      let outputs = logic (I.map I.t ~f:(fun (n, b) -> Signal.input n b)) in
      let circuit =
        call_with_create_options
          create_exn create_options ~name
          (O.to_list (O.map2 O.t outputs ~f:(fun (n, _) s -> Signal.output n s)))
      in
      circuit)
end
