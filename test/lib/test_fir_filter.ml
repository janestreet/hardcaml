(* FIR filter simulation test *)

open! Import

module I = struct
  type 'a t =
    { clk : 'a
    ; clr : 'a
    ; enable : 'a
    ; d : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { q : 'a [@bits 32] } [@@deriving sexp_of, hardcaml]
end

let f coefs (i : Signal.t I.t) =
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock:i.clk ~clear:i.clr in
  let ntaps = List.length coefs in
  let rec taps j =
    if j = ntaps
    then i.d, []
    else (
      let d, l = taps (j + 1) in
      let r = reg reg_spec ~enable:i.enable d in
      r, r :: l)
  in
  { O.q =
      List.map2_exn ~f:( *+ ) (snd (taps 0)) coefs |> tree ~arity:2 ~f:(reduce ~f:( +: ))
  }
;;

let%expect_test "testbench" =
  let module G = Cyclesim.With_interface (I) (O) in
  let module S = Cyclesim in
  let sim = G.create (f (List.map ~f:(Signal.of_int ~width:16) [ 3; 5; 2; 1 ])) in
  let i, o = S.inputs sim, S.outputs sim in
  S.reset sim;
  i.enable := Bits.vdd;
  i.d := Bits.of_int ~width:16 1;
  S.cycle sim;
  print_s [%message "" ~q:(Bits.to_int !(o.q) : int)];
  i.d := Bits.of_int ~width:16 2;
  S.cycle sim;
  print_s [%message "" ~q:(Bits.to_int !(o.q) : int)];
  i.d := Bits.of_int ~width:16 1;
  S.cycle sim;
  print_s [%message "" ~q:(Bits.to_int !(o.q) : int)];
  i.d := Bits.of_int ~width:16 3;
  S.cycle sim;
  print_s [%message "" ~q:(Bits.to_int !(o.q) : int)];
  [%expect {|
    (q 1)
    (q 4)
    (q 10)
    (q 18) |}]
;;
