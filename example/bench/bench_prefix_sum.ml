open! Import

let create_sim ~config ~kind n_bits =
  let open Signal in
  let circuit =
    Circuit.create_exn ~name:"lfsr"
      [ output "o"
          (Prefix_sum.create (module Signal) ~config
             ~input1:(input "a" n_bits)
             ~input2:(input "b" n_bits)
             ~carry_in:(input "carry" 1))]
  in
  let sim = Cyclesim.create ~kind circuit in
  (fun () -> for _=0 to 14 do Cyclesim.cycle sim done)

let%bench_fun "prefix-sum 8 serial Immutable" =
  let step = create_sim ~config:Serial ~kind:Immutable 8 in
  fun () -> step ()

let%bench_fun "prefix-sum 8 serial Mutable" =
  let step = create_sim ~config:Serial ~kind:Mutable 8 in
  fun () -> step ()

let%bench_fun "prefix-sum 8 kogge-stone Immutable" =
  let step = create_sim ~config:Kogge_stone ~kind:Immutable 8 in
  fun () -> step ()

let%bench_fun "prefix-sum 8 kogge-stone Mutable" =
  let step = create_sim ~config:Kogge_stone ~kind:Mutable 8 in
  fun () -> step ()
