open Hardcaml

let clocks = ["clock",1]
let resets = ["reset",1]
let inputs = ["a",2; "b",3]
let outputs = ["c",4; "d",5]

let verilog_file_name = "test.v"
let testbench_file_name = "testbench.v"
let vvp_file_name = "test.vvp"
let vcd_file_name = "vpi_load.vcd"
let module_name = "test"

(* write testbench *)
let file = open_out testbench_file_name
let () = Cosim.write_testbench 
  ~dump_file:vcd_file_name ~name:module_name
  ~inputs:(clocks@resets@inputs) ~outputs (output_string file)
let () = close_out file

let () = Cosim.compile [verilog_file_name; testbench_file_name] vvp_file_name

module B = Bits
module Co = Cosim.Make(B)
module S = Cyclesim.Api

let sim = Co.load ~clocks ~resets ~inputs ~outputs vvp_file_name
(*
let () = 
  S.reset sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim
*)
