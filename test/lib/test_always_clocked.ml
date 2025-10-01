open Import
open Clocked_design

let `New gen_uid, `Reset _ = Clock_domain.Uid.generator ()
let clock_domain_1 = Clock_domain.Expert.create_with_uid "domain_1" (gen_uid ())
let clock_domain_2 = Clock_domain.Expert.create_with_uid "domain_2" (gen_uid ())
let create_clocked_signal ~dom ~name ~width = Signal.input ~dom name width

let create_exact_domain domain =
  Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain domain)
;;

module%test Always_clocked_variable_wire_domain_validation = struct
  let%expect_test "Variable.wire with default from same domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let default = create_clocked_signal ~dom ~name:"default" ~width:8 in
    ignore (Always.Variable_overrides.wire ~default ~dom : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Variable.wire with default from different domain fails" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let default = create_clocked_signal ~dom:dom2 ~name:"default" ~width:8 in
    require_does_raise (fun () ->
      ignore (Always.Variable_overrides.wire ~default ~dom:dom1 : Always.Variable.t));
    [%expect
      {|
      ("[ensure_domain] Non-const Signal has unexpected clock domain"
       (loc  lib/hardcaml/hardcaml/src/always.ml:LINE:COL)
       (name default)
       (signal (
         (base (wire (names (default)) (width 8)))
         (dom (
           Exact (
             (uid  3)
             (name domain_2_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_always_clocked.ml:LINE:COL))))))
       (expected (
         (uid  2)
         (name domain_1_gen)
         (loc lib/hardcaml/hardcaml/test/lib/test_always_clocked.ml:LINE:COL)))
       (actual (
         (uid  3)
         (name domain_2_gen)
         (loc lib/hardcaml/hardcaml/test/lib/test_always_clocked.ml:LINE:COL))))
      |}]
  ;;

  let%expect_test "Variable.wire with constant default succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let default = Signal.of_unsigned_int ~width:8 42 in
    ignore (Always.Variable_overrides.wire ~default ~dom : Always.Variable.t);
    [%expect {| |}]
  ;;
end

module%test Always_clocked_set_variable_dom_validation = struct
  let%expect_test "set_variable_dom with compatible domain succeeds" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let default = Signal.of_unsigned_int ~width:8 0 in
    let var = Always.Variable_overrides.wire ~default ~dom:dom1 in
    ignore (Always.set_variable_dom var ~dom:dom2 : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "set_variable_dom preserves variable structure" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let default = Signal.of_unsigned_int ~width:16 255 in
    let var = Always.Variable_overrides.wire ~default ~dom:dom1 in
    ignore (Always.set_variable_dom var ~dom:dom2 : Always.Variable.t);
    [%expect {| |}]
  ;;
end

module%test Always_clocked_assignment_domain_validation = struct
  let%expect_test "(<--) assignment with same domain succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let default = Signal.of_unsigned_int ~width:8 0 in
    let var = Always.Variable_overrides.wire ~default ~dom in
    let value = create_clocked_signal ~dom ~name:"value" ~width:8 in
    ignore (Always.(var <-- value) : Always.t);
    [%expect {| |}]
  ;;

  let%expect_test "(<--) assignment with different domain fails" =
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    let default = Signal.of_unsigned_int ~width:8 0 in
    let var = Always.Variable_overrides.wire ~default ~dom:dom1 in
    let value = create_clocked_signal ~dom:dom2 ~name:"value" ~width:8 in
    require_does_raise (fun () -> ignore (Always.(var <-- value) : Always.t));
    [%expect
      {|
      ("[Always.(<--)] expects the clock domain of the operands to be compatible, but they're not:"
       ((dst (
          Exact (
            (uid  10)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_always_clocked.ml:LINE:COL))))
        (src (
          Exact (
            (uid  11)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_always_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "(<--) assignment with constant value succeeds" =
    let dom = create_exact_domain clock_domain_1 in
    let default = Signal.of_unsigned_int ~width:8 0 in
    let var = Always.Variable_overrides.wire ~default ~dom in
    let value = Signal.of_unsigned_int ~width:8 123 in
    ignore (Always.(var <-- value) : Always.t);
    [%expect {| |}]
  ;;
end
