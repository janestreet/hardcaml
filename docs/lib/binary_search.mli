open Base
open Hardcaml
module Clocking = Clocking

module type Data = sig
  include Interface.S

  val key : Signal.t t -> Signal.t
end

module type Config = sig
  module Data : Data

  val log_size : int
  val key_size : int
end

module Make (Config : Config) : sig
  module Data_with_valid : With_valid.Wrap.M(Config.Data).S

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; d : 'a Config.Data.t
      ; find_key : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; index : 'a
      ; q : 'a Data_with_valid.t
      ; address : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end

module Make_with_memory (Config : Config) : sig
  module Binary_search : module type of Make (Config)

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; write_enable : 'a
      ; write_data : 'a Config.Data.t
      ; write_address : 'a
      ; start : 'a
      ; find_key : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; index : 'a
      ; q : 'a Binary_search.Data_with_valid.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
end
