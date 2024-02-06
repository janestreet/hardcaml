val hdl_new : bool -> Hardcaml.Rtl.Language.t -> Hardcaml.Circuit.t -> string
val hdl_old : bool -> Hardcaml.Rtl.Language.t -> Hardcaml.Circuit.t -> string
val diff : Hardcaml.Rtl.Language.t -> Hardcaml.Circuit.t -> unit

val analyse
  :  ?quiet:bool
  -> Hardcaml.Rtl.Language.t
  -> Hardcaml.Circuit.t
  -> string
  -> unit

val diff_and_analyse
  :  ?quiet:bool
  -> ?show:bool
  -> ?blackbox:bool
  -> Hardcaml.Rtl.Language.t
  -> Hardcaml.Circuit.t
  -> unit

val analyse_vhdl_and_verilog
  :  ?quiet:bool
  -> ?show:bool
  -> ?blackbox:bool
  -> Hardcaml.Circuit.t
  -> unit

val diff_and_analyse_vhdl_and_verilog
  :  ?quiet:bool
  -> ?show:bool
  -> ?blackbox:bool
  -> Hardcaml.Circuit.t
  -> unit
