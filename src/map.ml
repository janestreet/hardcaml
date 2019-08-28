module Map_intf_in_this_directory = Map_intf
open! Import
include Map_intf_in_this_directory
include Base.Map

module Make (Key : Key) = struct
  module Key = Key

  type 'a t = 'a Base.Map.M(Key).t [@@deriving sexp_of]

  let empty = Base.Map.empty (module Key)
end
