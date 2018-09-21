open! Import

let create_sim n_bits =
  let open Signal in
  let circuit =
    Circuit.create_exn ~name:"lfsr"
      [ output "o" (Mul.create ~config:Wallace
                      (module Signal)
                      (input "a" n_bits)
                      (input "b" n_bits)) ]
  in
  let sim = Cyclesim.create circuit in
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  (fun () ->
     for i=0 to (1 lsl n_bits) - 1 do
       for j=0 to (1 lsl n_bits) - 1 do
         a := Bits.consti ~width:n_bits i;
         b := Bits.consti ~width:n_bits j;
         Cyclesim.cycle sim
       done
     done)

let%bench_fun "mul 3 bit" =
  let mul = create_sim 3 in
  fun () -> mul ()
