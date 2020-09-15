val create
  :  ?is_internal_port:(Signal.t -> bool)
  -> ?combinational_ops_database:Combinational_ops_database.t
  -> Circuit.t
  -> ((string * Bits.t ref) list, (string * Bits.t ref) list) Cyclesim0.t
