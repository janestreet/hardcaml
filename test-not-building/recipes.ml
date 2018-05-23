(*
  > utop
  $ #require "hardcaml,ppx_deriving_hardcaml,ppx_let";;
  $ #use "test/recipes.ml";;
  $ Test_mult.test ();;
  $ Test_sha1.test ();;
*)
open Hardcaml
open Signal
open Recipe
open Monad

module B = Bits
module S = Cyclesim.Api
module Vcd = Vcd.Gtkwave(B)

module Let_syntax = struct
  type 'a t = 'a recipe
  let return x = Monad.return
  let bind e f = Monad.bind e f
end
module Test_mult = struct

  (* interface to the serial multiplier *)
  module I = struct
    type 'a t = {
      start : 'a[@bits 1]; 
      a : 'a[@bits 8];
      b : 'a[@bits 8];
    }[@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      fin : 'a[@bits 1];
      mult : 'a[@bits 8];
    }[@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type 'a t = {
      a : 'a[@bits 8];
      b : 'a[@bits 8];
      acc : 'a[@bits 8];
    }[@@deriving sexp_of, hardcaml]
  end
  module SState = Same(State)

  (* shift 'a' up and 'b' down while 'b' does not equal zero.  
  * Add 'b' to the accumulator when the lsb of 'b' is non-zero. *)
  let step s = 
    let open State in
    { a = sll s.a 1; b = srl s.b 1; acc=mux2 (lsb s.b) (s.acc +: s.a) s.acc }

  let mult a_in b_in = 
    let open State in
    (*perform 
      (* create registers *)
      state <-- SState.newVar();
      (* set inputs and clear accumulator *)
      SState.apply (fun _ -> { a=a_in; b=b_in; acc=zero 8 }) state;
      (* serial multiplier *)
      SVar.while_ (fun b -> b <>:. 0) state.b 
        SState.(apply step state);
      (* return output *)
      acc <-- SVar.read state.acc;
      return acc
    *)
    let%bind state = SState.newVar () in
    let%bind () = SState.apply (fun _ -> { a=a_in; b=b_in; acc=zero 8 }) state in
    (* serial multiplier *)
    let%bind () = 
      SVar.while_ (fun b -> b <>:. 0) state.b 
        SState.(apply step state)
    in
    (* return output *)
    let%bind acc = SVar.read state.acc in
    return acc

  let top i = 
    let fin, mult = follow i.I.start @@ mult i.I.a i.I.b in
    O.{ fin; mult }

  (* simulation *)

  module G = Interface.Gen(B)(I)(O)

  let test () =
    let open I in
    let circ,sim,i,o,_ = G.make "serial_mult" top in
    (*let () = Rtl.Verilog.write print_string circ*) 

    let sim = Vcd.gtkwave ~args:"-S test/gwShowall.tcl" sim in
    let enable = S.in_port sim "enable" in
    S.reset sim;

    enable := B.vdd;
    i.a := B.consti 8 11;
    i.b := B.consti 8 17;
    i.start := B.vdd;
    S.cycle sim;
    i.start := B.gnd;
    for i=0 to 10 do S.cycle sim done;

    ignore @@ input_line stdin

end

(* need arrays, indexed by signals, to complete this *)
module Test_sha1 = struct

  module I = struct
    type 'a t = {
      start : 'a[@bits 1];
      d : 'a[@bits 32];
      d_valid : 'a[@bits 1];
    }[@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      fin : 'a[@bits 1];
      hash : 'a array[@length 5][@bits 32];
      redundant : 'a[@bits 1];
    }[@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type 'a t = {
      counter : 'a[@bits 7];
      h : 'a array[@length 5][@bits 32];
      (*w{|16|}[32]*)
    }[@@deriving sexp_of, hardcaml]
  end
  module SState = Same(State)

  let sha1 i = 
    let open I in
    let open State in
    let%bind state = SState.newVar() in
    (* reset state *)
    let%bind () = SState.apply (fun _ -> 
      { 
        counter=zero 7; 
        h=Array.map (consti32 32) 
          [| 0x67452301l; 0xEFCDAB89l; 0x98BADCFEl; 0x10325476l; 0xC3D2E1F0l |];
        (*w=Array.init 16 (fun _ -> consti 32 0);*)
      }) state in
    let%bind () = SVar.while_ (fun s -> s <:. 16) state.counter
      begin 
        (SVar.apply (fun d -> d +:. 1) state.counter);
      end in
    let%bind () = SState.while_ (fun s -> s.counter <:. 20) state
      begin 
        SVar.apply (fun d -> d +:. 1) state.counter;
      end in
    let%bind () = SState.while_ (fun s -> s.counter <:. 40) state
      begin 
        SVar.apply (fun d -> d +:. 1) state.counter;
      end in
    let%bind () = SState.while_ (fun s -> s.counter <:. 60) state
      begin 
        SVar.apply (fun d -> d +:. 1) state.counter;
      end in
    let%bind () = SState.while_ (fun s -> s.counter <:. 80) state
      begin 
        SVar.apply (fun d -> d +:. 1) state.counter;
      end in
    SState.read state

  let top i = 
    let fin, state = follow i.I.start @@ sha1 i in
    O.{ 
      fin; 
      hash=state.State.h;
      redundant = reduce (|:) (I.to_list @@ I.map lsb i);    
    }

  module G = Interface.Gen(B)(I)(O)

  let test () =
    let open I in
    let circ,sim,i,o,_ = G.make "sha1" top in

    let sim = Vcd.gtkwave ~args:"-S test/gwShowall.tcl" sim in
    let enable = S.in_port sim "enable" in
    S.reset sim;

    enable := B.vdd;
    i.start := B.vdd;
    S.cycle sim;
    i.start := B.gnd;
    for i=0 to 90 do S.cycle sim done;

    ignore @@ input_line stdin

  (*let () = test ()*)

end


