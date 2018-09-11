open! Import

let create_sim ~kind n_bits =
  let open Signal in
  let circuit =
    Circuit.create_exn ~name:"lfsr"
      [ output "o" (Lfsr.create (module Signal) (input "i" n_bits)) ]
  in
  let sim = Cyclesim.create ~kind circuit in
  let i = Cyclesim.in_port sim "i" in
  let o = Cyclesim.out_port sim "o" in
  i := Bits.consti ~width:n_bits 1;
  (fun () ->
     for _=0 to 99 do
       Cyclesim.cycle sim;
       i := !o
     done)

let%bench_fun "LFSR 8 Immutable" =
  let step = create_sim ~kind:Immutable 8 in
  fun () -> step ()

let%bench_fun "LFSR 8 Mutable" =
  let step = create_sim ~kind:Mutable 8 in
  fun () -> step ()

let%bench_fun "LFSR 32 Immutable" =
  let step = create_sim ~kind:Immutable 32 in
  fun () -> step ()

let%bench_fun "LFSR 32 Mutable" =
  let step = create_sim ~kind:Mutable 32 in
  fun () -> step ()

let%bench_fun "LFSR 168 Immutable" =
  let step = create_sim ~kind:Immutable 168 in
  fun () -> step ()

let%bench_fun "LFSR 168 Mutable" =
  let step = create_sim ~kind:Mutable 168 in
  fun () -> step ()
