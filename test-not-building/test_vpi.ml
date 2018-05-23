open Hardcaml
open Signal
open Signal.Seq

module I = interface 
  clock[1] reset[1] clear[1] enable a[2] b[2]
end

module O = interface
  c[2] d[2]
end

let reg i enable d = 
  reg Signal.Types.{ r_full with
    reg_clock = i.I.clock;
    reg_reset = i.I.reset;
    reg_clear = i.I.clear;
  } enable d

let f i = 
  let c = i.I.a +: i.I.b in
  let d = reg i enable c in
  O.{ c; d }

module B = Bits
module G = Interface.Gen(B)(I)(O)
module Gc = Interface.Gen_cosim(B)(I)(O)
module S = Cyclesim.Api

open I
open O

module Vcd = Vcd_ext.Make(B)

let _,sim0,i,o = G.make "cosimtest" f
let sim0 = Vcd.gtkwave ~args:"-S gwShowall.tcl" sim0
let _,sim1,_,_ = Gc.make "cosimtest" f

module Cs = Cyclesim.Make(B)
let sim = Cs.combine_relaxed sim0 sim1


