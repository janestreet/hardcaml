(** Opens the clocked variants for [Signal.S] and [Always.S] which track and check clock
    domain crossings across the design.

    Open at the top level of modules that need to be clocked. *)

module Signal = struct
  include Clocked_signal
  include Clocked_signal.Overrides
end

module Always = struct
  include Always.Clocked

  module Variable = struct
    include Always.Clocked.Variable
    include Always.Clocked.Variable_overrides
  end
end
