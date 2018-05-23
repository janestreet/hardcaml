(* Demonstrate the C based simulator.

   In this flow a c-model of the hardware design is written, then
   compiled as a shared libary and loaded through ctypes.  A
   standard simulator object is constructed.
*)
open Hardcaml
open Signal

(* simple example hardware design - 2 inputs and 2 outputs *)

module I = interface
  a[6] b[8]
end

module O = interface
  c[9] d[8]
end

let f i = 
  { O.c = I.(uresize i.a 9 +: uresize i.b 9);
      d = I.(uresize i.a 8 &: uresize i.b 8);    
  }

(* generate the C simulation model.  Most of the magic happens in 
   Hardcaml_csim.Sim.make *)

module G = Interface.Circ(I)(O)
module S = Cyclesim.Api

let make () = 
  let circ = G.make "adderizer" f in
  Hardcaml_csim.Sim.make circ 

(* testbench *)

open Hardcaml.Bits.Comb.ArraybitsInt32
open I
open O

let test () = 

  let sim = make () in

  let i = I.(map (fun (n,_) -> S.in_port sim n) t) in
  let o = O.(map (fun (n,_) -> S.out_port sim n) t) in

  begin
    i.a := consti 6 3;
    i.b := consti 8 6;
    S.cycle sim;
    Printf.printf "%i %i\n" (to_int !(o.c)) (to_int !(o.d))
  end

let () = test ()

