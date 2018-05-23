#require "hardcaml";;

open Hardcaml
open Signal
open Signal.Seq

let a = input "a" 8
let w = wire 8
let r = reg r_none vdd w
let () = w <== (r +: a)

open Api
open Hardcaml.Cyclesim.Api

let circ = Circuit.make "test_next" [ output "r" r; output "w" w ]
let sim = Cyclesim.make circ
let a = in_port sim "a"
let r, r' = out_port sim "r", out_port_next sim "r"
let w, w' = out_port sim "w", out_port_next sim "w"

let show() = 
  Printf.printf "r=%i rn=%i w=%i wn=%i\n" 
    (B.to_int !r) (B.to_int !r') 
    (B.to_int !w) (B.to_int !w')

let () = 
  S.reset sim;
  a := B.consti 8 3;
  S.cycle sim; show();
  S.cycle sim; show();
  S.cycle sim; show();
  S.cycle sim; show();
  S.cycle sim; show()

