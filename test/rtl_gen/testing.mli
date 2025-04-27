val hdl
  :  ?database:Hardcaml.Circuit_database.t
  -> ?config:Hardcaml.Rtl.Config.t
  -> bool
  -> Hardcaml.Rtl.Language.t
  -> Hardcaml.Circuit.t
  -> string

val analyse
  :  ?quiet:bool
  -> Hardcaml.Rtl.Language.t
  -> Hardcaml.Circuit.t
  -> string
  -> unit

val analyse_vhdl_and_verilog
  :  ?quiet:bool
  -> ?show:bool
  -> ?blackbox:bool
  -> ?database:Hardcaml.Circuit_database.t
  -> ?config:Hardcaml.Rtl.Config.t
  -> Hardcaml.Circuit.t
  -> unit
