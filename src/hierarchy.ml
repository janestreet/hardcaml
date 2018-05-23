open! Import

module With_interface (I : Interface.S) (O : Interface.S) = struct
  module Inst = Instantiation.With_interface (I) (O)
  module Circ = Circuit.With_interface (I) (O)

  let create =
    Circuit.with_create_options
      (fun create_options db ~name create_fn inputs ->
         let circuit =
           Circuit.call_with_create_options
             Circ.create_exn create_options ~name create_fn
         in
         let name = Circuit_database.insert db circuit in
         Inst.create ~name inputs)
end
