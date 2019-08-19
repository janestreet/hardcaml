open! Core
open Hardcaml
open Signal

module Make (I : Interface.S) (O : Interface.S) = struct
  module I_with_clock = struct
    type 'a t =
      { clock : 'a
      ; i : 'a I.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create create scope (i : _ I_with_clock.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    I.map i.i ~f:(reg spec ~enable:vdd)
    |> create scope
    |> O.map ~f:(reg spec ~enable:vdd)
  ;;

  let hier ~name create_fn scope (i : _ I_with_clock.t) =
    let module Scoped = Hierarchy.In_scope (I_with_clock) (O) in
    Scoped.hierarchical ~scope ~name (create create_fn) i
  ;;
end
