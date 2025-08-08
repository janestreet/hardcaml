open Import
open Clocked_design

let `New gen_uid, `Reset _ = Clock_domain.Uid.generator ()
let clock_domain_1 = Clock_domain.Expert.create_with_uid "domain_1" (gen_uid ())
let clock_domain_2 = Clock_domain.Expert.create_with_uid "domain_2" (gen_uid ())
let create_clocked_signal ~dom ~name ~width = Signal.input ~dom name width

let create_exact_domain domain =
  Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain domain)
;;

(* Define a simple interface for testing *)
module Test_interface = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; valid : 'a [@bits 1]
    ; ready : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module%test Interface_of_clocked_signal_wires_domain_validation = struct
  let%expect_test "Interface.Of_clocked_signal.wires with `From succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let (source : Test_interface.Of_clocked_signal.t) =
      { data = create_clocked_signal ~dom ~name:"src_data" ~width:8
      ; valid = create_clocked_signal ~dom ~name:"src_valid" ~width:1
      ; ready = create_clocked_signal ~dom ~name:"src_ready" ~width:1
      }
    in
    ignore
      (Test_interface.Of_clocked_signal.wires (`From source) : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.wires with `Domains succeeds" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let domains =
      (Test_interface.
         { data = dom1; valid = dom1; ready = dom2 (* Different domain for ready *) }
       : Clock_domain.Runtime.t Test_interface.t)
    in
    ignore
      (Test_interface.Of_clocked_signal.wires (`Domains domains)
       : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.wires with named wires" =
    let dom = create_exact_domain clock_domain_1 in
    let (source : Test_interface.Of_clocked_signal.t) =
      { data = create_clocked_signal ~dom ~name:"src_data" ~width:8
      ; valid = create_clocked_signal ~dom ~name:"src_valid" ~width:1
      ; ready = create_clocked_signal ~dom ~name:"src_ready" ~width:1
      }
    in
    ignore
      (Test_interface.Of_clocked_signal.wires ~named:true (`From source)
       : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;
end

module%test Interface_of_clocked_signal_inputs_domain_validation = struct
  let%expect_test "Interface.Of_clocked_signal.inputs with uniform domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let domains =
      (Test_interface.{ data = dom; valid = dom; ready = dom }
       : Clock_domain.Runtime.t Test_interface.t)
    in
    ignore (Test_interface.Of_clocked_signal.inputs domains : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.inputs with mixed domains succeeds" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let domains =
      (Test_interface.
         { data = dom1; valid = dom1; ready = dom2 (* Different domain for ready *) }
       : Clock_domain.Runtime.t Test_interface.t)
    in
    ignore (Test_interface.Of_clocked_signal.inputs domains : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.inputs with constant domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let domains =
      (Test_interface.
         { data = dom
         ; valid = Clock_domain.Runtime.Constant (* Constant domain *)
         ; ready = dom
         }
       : Clock_domain.Runtime.t Test_interface.t)
    in
    ignore (Test_interface.Of_clocked_signal.inputs domains : Signal.t Test_interface.t);
    [%expect {| |}]
  ;;
end

module%test Interface_of_clocked_signal_outputs_domain_validation = struct
  let%expect_test "Interface.Of_clocked_signal.outputs with uniform domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let (data : Test_interface.Of_clocked_signal.t) =
      { data = create_clocked_signal ~dom ~name:"out_data" ~width:8
      ; valid = create_clocked_signal ~dom ~name:"out_valid" ~width:1
      ; ready = create_clocked_signal ~dom ~name:"out_ready" ~width:1
      }
    in
    ignore
      (Test_interface.Of_clocked_signal.outputs data : Test_interface.Of_clocked_signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.outputs with mixed domains succeeds" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let (data : Test_interface.Of_clocked_signal.t) =
      { data = create_clocked_signal ~dom:dom1 ~name:"out_data" ~width:8
      ; valid = create_clocked_signal ~dom:dom1 ~name:"out_valid" ~width:1
      ; ready =
          create_clocked_signal ~dom:dom2 ~name:"out_ready" ~width:1
          (* Different domain *)
      }
    in
    ignore
      (Test_interface.Of_clocked_signal.outputs data : Test_interface.Of_clocked_signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Interface.Of_clocked_signal.outputs with constant domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let (data : Test_interface.Of_clocked_signal.t) =
      { data = create_clocked_signal ~dom ~name:"out_data" ~width:8
      ; valid = Signal.of_unsigned_int ~width:1 1 (* Constant signal *)
      ; ready = create_clocked_signal ~dom ~name:"out_ready" ~width:1
      }
    in
    ignore
      (Test_interface.Of_clocked_signal.outputs data : Test_interface.Of_clocked_signal.t);
    [%expect {| |}]
  ;;
end
