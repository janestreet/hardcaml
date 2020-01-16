open! Import


let verify_clock_pins ~expected_clock_pins (t : Circuit.t) =
  let rec transitively_resolve (signal : Signal.t) =
    match signal with
    | Empty -> assert false
    | Wire { signal_id; driver } ->
      (match !driver with
       | Empty -> signal_id
       | otherwise -> transitively_resolve otherwise)
    | Op2 _
    | Not _
    | Cat _
    | Mux _
    | Const _
    | Select _
    | Reg _
    | Mem _
    | Multiport_mem _
    | Mem_read_port _
    | Inst _ ->
      (match Signal.signal_id signal with
       | None -> assert false
       | Some s -> s)
  in
  let clock_domains =
    Signal_graph.depth_first_search
      (Circuit.signal_graph t)
      ~init:Signal.Uid_map.empty
      ~f_before:(fun unchanged signal ->
        match signal with
        | Mem { register = r; _ } | Reg { register = r; _ } ->
          let clock_domain = transitively_resolve r.reg_clock in
          Map.add_multi unchanged ~key:clock_domain.s_id ~data:(clock_domain, signal)
        | Multiport_mem { write_ports; _ } ->
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
