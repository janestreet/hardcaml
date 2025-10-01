module Signal : sig
  include module type of Clocked_signal
  include module type of Clocked_signal.Overrides
end

module Always : sig
  include module type of Always.Clocked

  module Variable : sig
    include module type of Always.Clocked.Variable
    include module type of Always.Clocked.Variable_overrides
  end
end
