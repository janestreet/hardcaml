(** {1 Design} *)

module Always = Always
module Assertions = Assertions
module Caller_id = Caller_id
module Call_stack = Call_stack
module Comb = Comb
module Constant = Constant
module Interface = Interface
module Instantiation = Instantiation
module Parameter = Parameter
module Parameter_name = Parameter_name
module Property = Property
module Property_manager = Property_manager
module Scope = Scope
module Signal = Signal
module Structural = Structural

(** {1 Circuits} *)

module Circuit = Circuit
module Circuit_database = Circuit_database
module Circuit_utilization = Circuit_utilization
module Hierarchy = Hierarchy
module Mangler = Mangler

(** {1 Simulation and modelling} *)

module Bits = Bits
module Bits_list = Bits_list
module Combinational_op = Combinational_op
module Combinational_ops_database = Combinational_ops_database
module Coverage_prim = Coverage_prim
module Cyclesim = Cyclesim
module Cyclesim_clock_domain = Cyclesim_clock_domain
module Cyclesim_coverage_expect_test = Cyclesim_coverage.For_expect_tests
module Cyclesim_float_ops = Cyclesim_float_ops
module Logic = Logic
module Vcd = Vcd
module Wave_data = Wave_data
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

module Rtl_attribute = Rtl_attribute
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
module Clocking = Clocking
module Edge = Edge
module Enum = Enum
module Flags_vector = Flags_vector
module Level = Level
module Name_and_loc = Name_and_loc
module Read_port = Read_port
module Side = Side
module Signedness = Signedness
module Types = Types
module With_valid = With_valid
module Write_port = Write_port

(** {1 Core circuits} *)

module Async_fifo = Async_fifo
module Fifo = Fifo
module Ram = Ram

(** {1 Circuit design with clock domain checking} *)

module Clock_domain = Clock_domain
module Clocked_design = Clocked_design
module Clocked_signal = Clocked_signal

(**/**)

module Tools_config = Tools_config

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
