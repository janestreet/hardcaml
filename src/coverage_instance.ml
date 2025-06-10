open Base

module Mux = struct
  type t =
    { select_node : Cyclesim_lookup.Node.t
    ; coverage : Signal_coverage.Mux.t
    }
end

module Cases = struct
  type t =
    { select_node : Cyclesim_lookup.Node.t
    ; case_nodes : Cyclesim_lookup.Node.t list
    ; coverage : Signal_coverage.Cases.t
    }
end

module Reg = struct
  type t =
    { reg : Cyclesim_lookup.Reg.t
    ; previous_value : Bytes.t
    ; coverage : Signal_coverage.Reg.t
    }

  let data t = Cyclesim_lookup.Reg.data t.reg
  let byte_address t = Cyclesim_lookup.Reg.byte_address t.reg
  let size_in_bytes t = Bytes.length t.previous_value
  let bit_width t = Cyclesim_lookup.Reg.width_in_bits t.reg

  let update_previous_value t =
    Bytes.blit
      ~dst:t.previous_value
      ~dst_pos:0
      ~len:(size_in_bytes t)
      ~src:(data t)
      ~src_pos:(byte_address t)
  ;;

  let create reg coverage =
    let size_in_bytes =
      Int.round_up ~to_multiple_of:8 (Cyclesim_lookup.Reg.width_in_bits reg) / 8
    in
    let t = { reg; previous_value = Bytes.create size_in_bytes; coverage } in
    update_previous_value t;
    t
  ;;

  let get_byte t ~i = Bytes.get (data t) (i + byte_address t)
  let get_previous_byte t ~i = Bytes.get t.previous_value i
end

type t =
  | Mux of Mux.t
  | Cases of Cases.t
  | Reg of Reg.t

let lookup_node_exn sim signal =
  Cyclesim0.lookup_node_by_id sim (Signal.uid signal) |> Option.value_exn
;;

let lookup_reg_exn sim signal =
  Cyclesim0.lookup_reg_by_id sim (Signal.uid signal) |> Option.value_exn
;;

module type To_sexp = sig
  type t

  val sexp_of_t : t -> Sexplib.Sexp.t
end

let raise_kind_mismatch (type a) (module M : To_sexp with type t = a) signal coverage =
  raise_s [%message "Mismatch signal kinds" (signal : Signal.t) (coverage : M.t)]
;;

let create_mux sim (signal : Signal.t) coverage =
  match signal with
  | Mux { select; cases; _ } ->
    if List.length cases <> Signal_coverage.Mux.case_count coverage
    then
      raise_s
        [%message
          "Mux signals with different case counts"
            (signal : Signal.t)
            (coverage : Signal_coverage.Mux.t)];
    Mux { select_node = lookup_node_exn sim select; coverage }
  | _ -> raise_kind_mismatch (module Signal_coverage.Mux) signal coverage
;;

let create_cases sim (signal : Signal.t) coverage =
  match signal with
  | Cases { select; cases; _ } ->
    let num_cases_including_default = List.length cases + 1 in
    if num_cases_including_default <> List.length (Signal_coverage.Cases.cases coverage)
    then
      raise_s
        [%message
          "Cases signals with different case counts"
            (signal : Signal.t)
            (coverage : Signal_coverage.Cases.t)];
    Cases
      { select_node = lookup_node_exn sim select
      ; case_nodes = List.map cases ~f:(fun (match_, _) -> lookup_node_exn sim match_)
      ; coverage
      }
  | _ -> raise_kind_mismatch (module Signal_coverage.Cases) signal coverage
;;

let create_reg sim (signal : Signal.t) coverage =
  match signal with
  | Reg _ ->
    if Signal.width signal <> Signal_coverage.Reg.bits coverage
    then
      raise_s
        [%message
          "Register signals with different bit widths"
            (signal : Signal.t)
            (coverage : Signal_coverage.Reg.t)];
    let reg = lookup_reg_exn sim signal in
    Reg (Reg.create reg coverage)
  | _ -> raise_kind_mismatch (module Signal_coverage.Reg) signal coverage
;;

let create sim signal (coverage : Signal_coverage.t) =
  let debug_info = Signal_coverage.Debug_info.create signal in
  if not (Signal_coverage.Debug_info.equal debug_info coverage.debug_info)
  then
    raise_s
      [%message
        "Signals with different debug info"
          (debug_info : Signal_coverage.Debug_info.t)
          (coverage.debug_info : Signal_coverage.Debug_info.t)];
  match coverage.data with
  | Mux coverage -> create_mux sim signal coverage
  | Reg coverage -> create_reg sim signal coverage
  | Cases coverage -> create_cases sim signal coverage
;;

let sample_mux (mux : Mux.t) =
  let%tydi { select_node; coverage } = mux in
  Signal_coverage.Mux.mark_choice coverage (Cyclesim_lookup.Node.to_int select_node)
;;

let sample_cases (cases : Cases.t) =
  let%tydi { select_node; case_nodes; coverage } = cases in
  let select = Cyclesim_lookup.Node.to_int select_node in
  let choice =
    match
      List.find_mapi case_nodes ~f:(fun i case_node ->
        if select = Cyclesim_lookup.Node.to_int case_node then Some i else None)
    with
    | None -> Signal_coverage.Cases.Case.Default
    | Some index -> Specified index
  in
  Signal_coverage.Cases.mark_choice coverage choice
;;

let sample_reg (reg : Reg.t) =
  let changed = ref false in
  let bit_width = Reg.bit_width reg in
  let select_bit char ~i = (Char.to_int char lsr i) land 1 in
  let sample_bit ~byte ~prev_byte ~bit_index ~offset =
    let i = bit_index - offset in
    let bit = select_bit byte ~i in
    if bit <> select_bit prev_byte ~i
    then (
      changed := true;
      Signal_coverage.Reg.mark_toggled_bit reg.coverage ~bit:bit_index ~on:(bit <> 0))
  in
  let sample_byte ~byte_index =
    let byte = Reg.get_byte reg ~i:byte_index in
    let prev_byte = Reg.get_previous_byte reg ~i:byte_index in
    if not (Char.equal prev_byte byte)
    then (
      let offset = byte_index * 8 in
      let to_ = min (bit_width - 1) (offset + 7) in
      for bit_index = offset to to_ do
        sample_bit ~byte ~prev_byte ~bit_index ~offset
      done)
  in
  for byte_index = 0 to Reg.size_in_bytes reg - 1 do
    sample_byte ~byte_index
  done;
  if !changed then Reg.update_previous_value reg
;;

let sample t =
  match t with
  | Mux mux -> sample_mux mux
  | Cases cases -> sample_cases cases
  | Reg reg -> sample_reg reg
;;
