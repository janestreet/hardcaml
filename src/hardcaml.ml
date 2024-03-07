(** {1 Design} *)

module Always = Always
module Assertions = Assertions
module Caller_id = Caller_id
module Comb = Comb
module Constant = Constant
module Interface = Interface
module Instantiation = Instantiation
module Parameter = Parameter
module Parameter_name = Parameter_name
module Property = Property
module Property_manager = Property_manager
module Reg_spec = Reg_spec
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
module Cyclesim = Cyclesim
module Cyclesim_float_ops = Cyclesim_float_ops
module Logic = Logic
module Vcd = Vcd
module Wave_format = Wave_format
module Wave_data = Wave_data

(** {1 Rtl generation} *)

module Rtl = struct
  include Rtl
  module Ast = Rtl_ast
  module Deprecated = Rtl_deprecated
  module Name = Rtl_name
  module Verilog = Rtl_verilog_of_ast
  module Vhdl = Rtl_vhdl_of_ast
end

module Rtl_attribute = Rtl_attribute
module Reserved_words = Reserved_words

(** {1 Transformations and passes} *)

module Design_rule_checks = Design_rule_checks
module Dedup = Dedup
module Signal_graph = Signal_graph

(** {1 Misc types} *)

module Architecture = Architecture
module Build_mode = Build_mode
module Edge = Edge
module Enum = Enum
module Flags_vector = Flags_vector
module Level = Level
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

(**/**)

(** These are exposed for code that does [@@deriving hardcaml]. *)
let sexp_of_array = Base.sexp_of_array

let sexp_of_list = Base.sexp_of_list
