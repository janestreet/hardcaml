open Base

let verify_clock_pins ~expected_clock_pins (t : Circuit.t) =
  let rec transitively_resolve (signal : Signal.t) =
    match signal with
    | Empty -> assert false
    | Wire { signal_id = _; driver } ->
      (match !driver with
       | Empty -> signal
       | otherwise -> transitively_resolve otherwise)
    | Op2 _
    | Not _
    | Cat _
    | Mux _
    | Const _
    | Select _
    | Reg _
    | Multiport_mem _
    | Mem_read_port _
    | Inst _ ->
      (match Signal.Type.signal_id signal with
       | None -> assert false
       | Some _ -> signal)
  in
  let clock_domains =
    Signal_graph.depth_first_search
      (Circuit.signal_graph t)
      ~init:(Map.empty (module Signal.Uid))
      ~f_before:(fun unchanged signal ->
        match signal with
        | Reg { register = r; _ } ->
          let clock_domain = transitively_resolve r.reg_clock in
          Map.add_multi
            unchanged
            ~key:(Signal.uid clock_domain)
            ~data:(clock_domain, signal)
        | Multiport_mem { write_ports; _ } ->
          Array.fold write_ports ~init:unchanged ~f:(fun acc port ->
            let clock_domain = transitively_resolve port.write_clock in
            Map.add_multi acc ~key:(Signal.uid clock_domain) ~data:(clock_domain, signal))
        | _ -> unchanged)
  in
  let expected_clock_domains = Hash_set.of_list (module String) expected_clock_pins in
  Map.iteri clock_domains ~f:(fun ~key:signal_uid ~data:all ->
    let clock_domain_in_expected =
      List.exists all ~f:(fun (clock_domain, _) ->
        List.exists (Signal.names clock_domain) ~f:(fun name ->
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
            (signal_uid : Signal.Uid.t)
            (signals : Signal.t list)])
;;
