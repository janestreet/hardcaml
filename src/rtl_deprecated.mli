open Base
module Uid_with_index = Rtl_ast.Signals_name_map.Uid_with_index

type signals_name_map_t = Rtl_ast.Signals_name_map.t
type io = string -> unit

module VhdlNames : sig
  val case_sensitive : bool
  val prefix : string
  val reserved : string list
  val legalize : string -> string
end

module Vhdl : sig
  val write : bool -> io -> Circuit.t -> signals_name_map_t
end

module VerilogNames : sig
  val case_sensitive : bool
  val prefix : string
  val reserved : string list
  val legalize : string -> string
end

module Verilog : sig
  val write : bool -> io -> Circuit.t -> signals_name_map_t
end
