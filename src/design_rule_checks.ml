open! Import


let verify_clock_pins ~expected_clock_pins (t : Circuit.t) =
  let rec transitively_resolve (signal : Signal.t) =
    match signal with
    | Empty -> assert false
    | Op (id, _)
    | Const (id, _)
    | Select (id, _, _)
    | Reg (id, _)
    | Mem (id, _, _, _)
    | Multiport_mem (id, _, _)
    | Mem_read_port (id, _, _)
    | Inst (id, _, _) -> id
    | Wire (id, reference) ->
      (match !reference with
       | Empty -> id
       | otherwise -> transitively_resolve otherwise)
  in
  let clock_domains =
    Signal_graph.depth_first_search
      (Circuit.signal_graph t)
      ~init:Signal.Uid_map.empty
      ~f_before:(fun unchanged signal ->
        match signal with
        | Mem (_, _, r, _) | Reg (_, r) ->
          let clock_domain = transitively_resolve r.reg_clock in
          Map.add_multi unchanged ~key:clock_domain.s_id ~data:(clock_domain, signal)
        | Multiport_mem (_, _, write_ports) ->
          Array.fold write_ports ~init:unchanged ~f:(fun acc port ->
            let clock_domain = transitively_resolve port.write_clock in
            Map.add_multi acc ~key:clock_domain.s_id ~data:(clock_domain, signal))
        | _ -> unchanged)
  in
  let expected_clock_domains = Hash_set.of_list (module String) expected_clock_pins in
  Map.iteri clock_domains ~f:(fun ~key:signal_uid ~data:all ->
    let clock_domain_in_expected =
      List.exists all ~f:(fun (clock_domain, _) ->
        List.exists clock_domain.s_names ~f:(fun name ->
          if Hash_set.mem expected_clock_domains name
          then (
            Hash_set.remove expected_clock_domains name;
            true)
          else false))
    in
    let signals = List.map ~f:snd all in
    if not clock_domain_in_expected
    then
      raise_s
        [%message
          "The following sequential elements have unexpected clock pin connections"
            (signal_uid : int64)
            (signals : Signal.t list)])
;;
