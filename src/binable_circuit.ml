open! Core0
module Uid = Signal.Type.Uid

module Binable_signal = struct
  type t =
    | Const of Signal.Type.Const.t
    | Op2 of Uid.t Signal.Type.Op2.t
    | Mux of Uid.t Signal.Type.Mux.t
    | Cases of Uid.t Signal.Type.Cases.t
    | Cat of Uid.t Signal.Type.Cat.t
    | Not of Uid.t Signal.Type.Not.t
    | Wire of Uid.t Signal.Type.Wire.t
    | Select of Uid.t Signal.Type.Select.t
    | Reg of Uid.t Signal.Type.Reg.t
    | Multiport_mem of Uid.t Signal.Type.Multiport_mem.t
    | Mem_read_port of Uid.t Signal.Type.Mem_read_port.t
    | Inst of Uid.t Signal.Type.Inst.t
  [@@deriving bin_io, sexp_of]

  let of_signal (signal : Signal.t) : t =
    match signal with
    | Empty -> raise_s [%message "Empty not supported"]
    | Const const -> Const const
    | Op2 op2 -> Op2 (Signal.Type.Op2.map op2 ~f:Signal.uid)
    | Mux mux -> Mux (Signal.Type.Mux.map mux ~f:Signal.uid)
    | Cases cases -> Cases (Signal.Type.Cases.map cases ~f:Signal.uid)
    | Cat cat -> Cat (Signal.Type.Cat.map cat ~f:Signal.uid)
    | Not not_ -> Not (Signal.Type.Not.map not_ ~f:Signal.uid)
    | Wire wire -> Wire (Signal.Type.Wire.map wire ~f:Signal.uid)
    | Select select -> Select (Signal.Type.Select.map select ~f:Signal.uid)
    | Reg reg -> Reg (Signal.Type.Reg.map reg ~f:Signal.uid)
    | Multiport_mem multiport_mem ->
      Multiport_mem (Signal.Type.Multiport_mem.map multiport_mem ~f:Signal.uid)
    | Mem_read_port mem_read_port ->
      Mem_read_port (Signal.Type.Mem_read_port.map mem_read_port ~f:Signal.uid)
    | Inst inst -> Inst (Signal.Type.Inst.map inst ~f:Signal.uid)
  ;;

  module Wire_to_set = struct
    type t =
      { undriven_wire_to_set : Signal.t
      ; driver_uid : Uid.t
      }
  end

  let to_signal t ~(lookup_signal : Uid.t -> Signal.t) : Signal.t * Wire_to_set.t option =
    match t with
    | Const const -> Const const, None
    | Op2 op2 -> Op2 (Signal.Type.Op2.map op2 ~f:lookup_signal), None
    | Mux mux -> Mux (Signal.Type.Mux.map mux ~f:lookup_signal), None
    | Cases cases -> Cases (Signal.Type.Cases.map cases ~f:lookup_signal), None
    | Cat cat -> Cat (Signal.Type.Cat.map cat ~f:lookup_signal), None
    | Not not_ -> Not (Signal.Type.Not.map not_ ~f:lookup_signal), None
    | Wire { info; driver } ->
      let wire = Signal.Type.Wire { info; driver = None } in
      let wire_to_set =
        Option.map driver ~f:(fun driver_uid : Wire_to_set.t ->
          { undriven_wire_to_set = wire; driver_uid })
      in
      wire, wire_to_set
    | Select select -> Select (Signal.Type.Select.map select ~f:lookup_signal), None
    | Reg reg -> Reg (Signal.Type.Reg.map reg ~f:lookup_signal), None
    | Multiport_mem multiport_mem ->
      Multiport_mem (Signal.Type.Multiport_mem.map multiport_mem ~f:lookup_signal), None
    | Mem_read_port mem_read_port ->
      Mem_read_port (Signal.Type.Mem_read_port.map mem_read_port ~f:lookup_signal), None
    | Inst inst -> Inst (Signal.Type.Inst.map inst ~f:lookup_signal), None
  ;;

  let uid (signal_kind : t) : Uid.t =
    match signal_kind with
    | Const { info; _ }
    | Op2 { info; _ }
    | Mux { info; _ }
    | Cases { info; _ }
    | Cat { info; _ }
    | Not { info; _ }
    | Wire { info; _ }
    | Select { info; _ }
    | Reg { info; _ }
    | Multiport_mem { info; _ }
    | Mem_read_port { info; _ }
    | Inst { info; _ } -> info.uid
  ;;
end

type t =
  { signal_by_uid : Binable_signal.t Uid.Table.t
  ; output_uids : Uid.t list
  }
[@@deriving bin_io, sexp_of]

let of_outputs (outputs : Signal.t list) : t =
  let signal_by_uid = Uid.Table.create () in
  Signal_graph.create outputs
  |> Signal_graph.depth_first_search ~init:() ~f_before:(fun () signal ->
    let signal = Binable_signal.of_signal signal in
    let uid = Binable_signal.uid signal in
    Hashtbl.add_exn signal_by_uid ~key:uid ~data:signal);
  let output_uids = List.map outputs ~f:Signal.uid in
  { signal_by_uid; output_uids }
;;

let to_outputs { signal_by_uid; output_uids } : Signal.t list =
  let output_signal_by_uid = Uid.Table.create () in
  let wires_to_set = ref [] in
  let rec lookup_signal uid =
    match Hashtbl.find output_signal_by_uid uid with
    | None ->
      let signal = Hashtbl.find_exn signal_by_uid uid in
      let signal, maybe_wire_to_set = Binable_signal.to_signal signal ~lookup_signal in
      Option.iter maybe_wire_to_set ~f:(fun wire_to_set ->
        wires_to_set := wire_to_set :: !wires_to_set);
      Hashtbl.add_exn output_signal_by_uid ~key:uid ~data:signal;
      signal
    | Some signal -> signal
  in
  Hashtbl.iter_keys signal_by_uid ~f:(fun uid ->
    let (_ : Signal.t) = lookup_signal uid in
    ());
  List.iter !wires_to_set ~f:(fun { undriven_wire_to_set; driver_uid } ->
    match undriven_wire_to_set with
    | Wire wire ->
      (match wire.driver with
       | Some _ -> raise_s [%message "setting wire multiple times"]
       | None -> ());
      wire.driver <- Some (Hashtbl.find_exn output_signal_by_uid driver_uid)
    | _ -> raise_s [%message "expected wire"]);
  List.map output_uids ~f:(fun uid -> Hashtbl.find_exn output_signal_by_uid uid)
;;
