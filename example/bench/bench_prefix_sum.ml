open! Import

let create_sim ~config n_bits =
  let open Signal in
  let circuit =
    Circuit.create_exn ~name:"lfsr"
      [ output "o"
          (Prefix_sum.create (module Signal) ~config
             ~input1:(input "a" n_bits)
             ~input2:(input "b" n_bits)
             ~carry_in:(input "carry" 1))]
  in
  let sim = Cyclesim.create circuit in
  (fun () -> for _=0 to 14 do Cyclesim.cycle sim done)

let%bench_fun "prefix-sum 8 serial" =
  let step = create_sim ~config:Serial 8 in
  fun () -> step ()

let%bench_fun "prefix-sum 8 kogge-stone" =
  let step = create_sim ~config:Kogge_stone 8 in
  fun () -> step ()
