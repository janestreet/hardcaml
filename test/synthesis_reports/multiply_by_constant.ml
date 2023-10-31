open! Core
module Synth = Hardcaml_xilinx_reports

module Make (Bits : sig
  val multiply_by : int
  val bits : int
end) =
struct
  open Hardcaml.Signal

  module I = struct
    type 'a t = { input : 'a [@bits Bits.bits] } [@@deriving hardcaml]
  end

  module O = struct
    let num_bits = Bits.bits + Int.ceil_log2 Bits.multiply_by

    type 'a t = { result : 'a [@bits num_bits] } [@@deriving hardcaml]
  end

  module With_regs = Hardcaml_xilinx_reports.Wrap_with_registers.Make (I) (O)

  let create _scope (i : _ With_regs.I_with_clock.t) =
    let multiply_by =
      of_int ~width:(num_bits_to_represent Bits.multiply_by) Bits.multiply_by
    in
    { O.result = i.i.input *: multiply_by }
  ;;
end

let command =
  Async.Command.async
    ~summary:"multiply by constant logic"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and flags = Hardcaml_xilinx_reports.Command.Command_flags.flags
     and bits = flag "bits" (required int) ~doc:"BITS Number of input bits"
     and multiply_by =
       flag "multiply-by" (required int) ~doc:"N Multiply input by this value"
     in
     fun () ->
       let module Clz =
         Make (struct
           let bits = bits
           let multiply_by = multiply_by
         end)
       in
       let module Synth =
         Synth.Command.With_interface (Clz.With_regs.I_with_clock) (Clz.O)
       in
       Synth.run ~name:"multiply_by_constant" ~flags Clz.create)
    ~behave_nicely_in_pipeline:false
;;
