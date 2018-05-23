module X : sig
  module Intf : sig
    type 'a t = { a : 'a }[@@deriving sexp_of, hardcaml]
  end
  module Ex : Interface.Ex with type 'a t := 'a Intf.t 
  include Interface.S with type 'a t = 'a Intf.t
end = struct
  module Intf = struct
    type 'a t = { a : 'a }[@@deriving sexp_of, hardcaml]
  end
  module Ex : Interface.Ex with type 'a t := 'a Intf.t = Interface.Ex(Intf) 
  include Intf 
end

module Y : sig
  type 'a t = { a : 'a }[@@deriving sexp_of, hardcaml]
  module Ex : Interface.Ex with type 'a t := 'a t 
end = struct
  type 'a t = { a : 'a }[@@deriving sexp_of, hardcaml]
  module Ex = Interface.Ex(struct
      type 'a x = 'a t
      type 'a t = 'a x (* avoid cyclic type error *)
      let t = t
      let map = map
      let map2 = map2
      let to_list = to_list
  end) 
end
