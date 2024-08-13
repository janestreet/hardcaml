val hdl : bool -> Hardcaml.Rtl.Language.t -> Hardcaml.Circuit.t -> string

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
  -> Hardcaml.Circuit.t
  -> unit
