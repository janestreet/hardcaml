open! Core
module Synth = Hardcaml_xilinx_reports

module Make (Bits : sig
    val bits : int
  end) =
struct
  open Hardcaml.Signal

  module I = struct
    type 'a t = { data : 'a [@bits Bits.bits] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { count : 'a [@bits Int.ceil_log2 Bits.bits] } [@@deriving hardcaml]
  end

  let create_with_priority_mux _scope (i : _ I.t) = { O.count = leading_zeros i.data }

  let create_recursive_decomp _scope (i : _ I.t) =
    let rec leading_zeros_pow2 t =
      if width t = 2
      then ~:(t.:(1))
      else (
        let lhs, rhs = split_in_half_msb t in
        let lhs_empty = lhs ==:. 0 in
        lhs_empty @: leading_zeros_pow2 (mux2 lhs_empty rhs lhs))
    in
    { O.count = leading_zeros_pow2 i.data }
  ;;

  module With_regs = Hardcaml_xilinx_reports.Wrap_with_registers.Make (I) (O)

  let create scope (i : _ With_regs.I_with_clock.t) =
    { O.count =
        reduce
          ~f:( |: )
          [ (With_regs.hier
               ~name:"priority_mux"
               (`Combinational create_with_priority_mux)
               scope
               i)
              .count
          ; (With_regs.hier
               ~name:"rec_decomp"
               (`Combinational create_recursive_decomp)
               scope
               i)
              .count
          ]
    }
  ;;
end

let command =
  Async.Command.async
    ~summary:"count leading zeros logic"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and flags = Hardcaml_xilinx_reports.Command.Command_flags.flags ()
     and bits = flag "bits" (required int) ~doc:"BITS Number of input bits" in
     fun () ->
       let module Clz =
         Make (struct
           let bits = bits
         end)
       in
       let module Synth =
         Synth.Command.With_interface (Clz.With_regs.I_with_clock) (Clz.O)
       in
       Synth.run ~name:"count_leading_zeros" ~flags Clz.create)
;;
