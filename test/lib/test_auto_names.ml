open Base
open Hardcaml
open Signal

let naming_scheme = Scope.Naming_scheme.Auto

module Make (X : sig
  val name : string
  val op : Scope.t -> Signal.t -> Signal.t
end) =
struct
  module I = struct
    type 'a t = { i : 'a [@rtlprefix X.name ^ "_port_"] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { o : 'a [@rtlprefix X.name ^ "_port_"] } [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) = { O.o = X.op scope i.i -- (X.name ^ "_internal") }

  let hierarchy scope i =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~instance:X.name ~name:X.name ~scope create i
  ;;
end

module A = Make (struct
  let name = "A"
  let op _ i = i +:. 1
end)

module B = Make (struct
  let name = "B"
  let op scope i = (A.hierarchy scope { A.I.i }).o +:. 1
end)

module C = Make (struct
  let name = "C"
  let op scope i = (B.hierarchy scope { B.I.i }).o +:. 1
end)

module Circuit = Circuit.With_interface (C.I) (C.O)

let%expect_test "C" =
  let scope =
    Scope.create
      ~flatten_design:true
      ~naming_scheme
      ~auto_label_hierarchical_ports:true
      ()
  in
  let circuit = Circuit.create_exn ~name:"top" (C.hierarchy scope) in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module top (
        C_port_i,
        C_port_o
    );

        input C_port_i;
        output C_port_o;

        wire _14;
        wire _2;
        wire C$i$C_port_i;
        wire C$B$i$B_port_i;
        wire C$B$A$i$A_port_i;
        wire C$B$A$A_internal;
        wire C$B$A$o$A_port_o;
        wire C$B$B_internal;
        wire C$B$o$B_port_o;
        wire C$C_internal;
        wire C$o$C_port_o;
        assign _14 = 1'b1;
        assign _2 = C_port_i;
        assign C$i$C_port_i = _2;
        assign C$B$i$B_port_i = C$i$C_port_i;
        assign C$B$A$i$A_port_i = C$B$i$B_port_i;
        assign C$B$A$A_internal = C$B$A$i$A_port_i + _14;
        assign C$B$A$o$A_port_o = C$B$A$A_internal;
        assign C$B$B_internal = C$B$A$o$A_port_o + _14;
        assign C$B$o$B_port_o = C$B$B_internal;
        assign C$C_internal = C$B$o$B_port_o + _14;
        assign C$o$C_port_o = C$C_internal;
        assign C_port_o = C$o$C_port_o;

    endmodule
    |}]
;;

module Sim = Cyclesim.With_interface (C.I) (C.O)

let sim () =
  let scope =
    Scope.create
      ~flatten_design:true
      ~naming_scheme
      ~auto_label_hierarchical_ports:true
      ()
  in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (C.hierarchy scope) in
  let waves, sim = Hardcaml_waveterm_cyclesim.Waveform.create sim in
  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;
  waves
;;
