(** {1 Design} *)

module Always = Hardcaml_kernel.Always
module Assertions = Assertions
module Caller_id = Hardcaml_kernel.Caller_id
module Call_stack = Hardcaml_kernel.Call_stack
module Comb = Hardcaml_kernel.Comb
module Constant = Hardcaml_kernel.Constant
module Interface = Hardcaml_kernel.Interface
module Instantiation = Instantiation
module Parameter = Hardcaml_kernel.Parameter
module Parameter_name = Hardcaml_kernel.Parameter_name
module Property = Property
module Property_manager = Property_manager
module Scope = Scope
module Signal = Hardcaml_kernel.Signal
module Structural = Structural

(** {1 Circuits} *)

module Circuit = Circuit
module Circuit_database = Circuit_database
module Circuit_utilization = Circuit_utilization
module Hierarchy = Hierarchy
module Mangler = Mangler

(** {1 Simulation and modelling} *)

module Bits = Hardcaml_kernel.Bits
module Bits_list = Hardcaml_kernel.Bits_list
module Combinational_op = Combinational_op
module Combinational_ops_database = Combinational_ops_database
module Coverage_prim = Hardcaml_kernel.Coverage_prim
module Cyclesim = Cyclesim
module Cyclesim_clock_domain = Cyclesim_clock_domain
module Cyclesim_coverage_expect_test = Cyclesim_coverage.For_expect_tests
module Cyclesim_float_ops = Cyclesim_float_ops
module Four_state_logic = Four_state_logic
module Logic = Hardcaml_kernel.Logic
module Vcd = Vcd
module Wave_data = Wave_data
module Wave_data_in_mutable_events = Wave_data_in_mutable_events
module Wave_data_in_mutable_events_four_state = Wave_data_in_mutable_events_four_state
module Wave_format = Wave_format

(** {1 Rtl generation} *)

module Rtl = struct
  include Rtl
  module Ast = Rtl_ast
  module Name = Rtl_name
  module Verilog = Rtl_verilog_of_ast
  module Vhdl = Rtl_vhdl_of_ast
  module Config = Rtl_config
end

module Rtl_attribute = Hardcaml_kernel.Rtl_attribute
module Reserved_words = Reserved_words

(** {1 Transformations and passes} *)

module Design_rule_checks = Design_rule_checks
module Dedup = Dedup
module Signal_graph = Signal_graph

(** {1 Misc types} *)

module Architecture = Architecture
module Before_and_after_edge = Before_and_after_edge
module Binable_circuit = Binable_circuit
module Build_mode = Build_mode
module Clocking = Hardcaml_kernel.Clocking
module Edge = Hardcaml_kernel.Edge
module Enum = Hardcaml_kernel.Enum
module Flags_vector = Flags_vector
module Level = Hardcaml_kernel.Level
module Name_and_loc = Hardcaml_kernel.Name_and_loc
module Read_port = Read_write_port.Read_port
module Side = Side
module Signedness = Hardcaml_kernel.Signedness
module Types = Hardcaml_kernel.Types
module With_valid = Hardcaml_kernel.With_valid
module Write_port = Read_write_port.Write_port

(** {1 Core circuits} *)

module Async_fifo = Async_fifo
module Fifo = Fifo
module Ram = Ram

(**/**)

(* These are exposed for code that does [@@deriving hardcaml]. *)

let sexp_of_array = Core.sexp_of_array
let sexp_of_list = Core.sexp_of_list
let equal_list = Core.equal_list
let equal_list__local = Core.equal_list__local

(** {1 Private interface}

    These modules should only be used by hardcaml adjacent libraries and are not intended
    to be used by end users of hardcaml. *)
module Private = struct
  module Bits_packed = Bits_packed
  module Cyclesim0 = Cyclesim0
  module Cyclesim_combine = Cyclesim_combine
  module Cyclesim_coverage = Cyclesim_coverage
  module Cyclesim_ops = Cyclesim_ops
  module Simulation_memory = Simulation_memory
end
