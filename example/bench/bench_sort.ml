open! Import
open! Sorting_network

let create_sim ~kind ~config n_coefs n_bits =
  let open Signal in
  let inputs = List.init n_coefs ~f:(fun j -> input ("i" ^ Int.to_string j) n_bits) in
  let sort_ascending_unsigned : Signal.t compare_and_swap =
    (fun a b ->
       let sel = a <: b in
       { min = mux2 sel a b; max = mux2 sel b a }) in
  let sorted = create config sort_ascending_unsigned inputs in
  let sorted = List.mapi sorted ~f:(fun j o -> output ("o" ^ Int.to_string j) o) in
  let circuit = Circuit.create_exn ~name:"sort" sorted in
  let sim = Cyclesim.create ~kind circuit in
  (fun () ->
     for _=0 to 19 do
       Cyclesim.cycle sim
     done)

let%bench_fun "sorting network bitonic Immutable" =
  let step = create_sim ~kind:Immutable ~config:Bitonic_sort 16 8 in
  fun () -> step ()

let%bench_fun "sorting network bitonic Mutable" =
  let step = create_sim ~kind:Mutable ~config:Bitonic_sort 16 8 in
  fun () -> step ()

let%bench_fun "sorting network odd_even_merge Immutable" =
  let step = create_sim ~kind:Immutable ~config:Odd_even_merge_sort 16 8 in
  fun () -> step ()

let%bench_fun "sorting network odd_even_merge Mutable" =
  let step = create_sim ~kind:Mutable ~config:Odd_even_merge_sort 16 8 in
  fun () -> step ()
