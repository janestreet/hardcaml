open! Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock_src : 'a
    ; clock_dst : 'a
    ; d : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { q : 'a [@bits 8] } [@@deriving hardcaml]
end

let create { I.clock_src; clock_dst; d } =
  let spec_src = Reg_spec.create ~clock:clock_src () in
  let spec_dst = Reg_spec.create ~clock:clock_dst () in
  { O.q = reg spec_src d |> reg spec_dst |> reg spec_dst }
;;
