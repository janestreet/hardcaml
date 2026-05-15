open! Core
open Hardcaml

let%expect_test "user and derived name are the same" =
  let module I = struct
    type 'a t = { a : 'a [@rtlname "_wire"] } [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { b : 'a [@rtlname "_add"] } [@@deriving hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"naming" (fun { a } ->
      let open Signal in
      let tmp1 = ~:a in
      let tmp2 = a +: tmp1 in
      let b = a +: tmp2 -- "_not" in
      { b })
  in
  Rtl.print Verilog circuit;
  (* Derived names should get suffixes before user names *)
  [%expect
    {|
    module naming (
        _wire,
        _add
    );

        input _wire;
        output _add;

        wire signal_not;
        wire signal_add;
        wire signal_wire;
        wire _not;
        assign signal_not = ~ signal_wire;
        assign signal_add = signal_wire + signal_not;
        assign signal_wire = _wire;
        assign _not = signal_wire + signal_add;
        assign _add = _not;

    endmodule
    |}]
;;
