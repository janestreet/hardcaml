val create
  :  ?config:Cyclesim0.Config.t
  -> Circuit.t
  -> ((string * Bits.t ref) list, (string * Bits.t ref) list) Cyclesim0.t
