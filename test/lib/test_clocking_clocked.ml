open Import
open Clocked_design
module Clocking = Clocking.Clocked

let reset_uid_for_test () = Clock_domain.Exact.Expert.reset_uid ()
let clock_domain_1 = Clock_domain.create "domain_1"
let clock_domain_2 = Clock_domain.create "domain_2"
let create_clocked_signal ~dom ~name ~width = Signal.input ~dom name width

let create_exact_domain domain =
  Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain domain)
;;

module%test Clocking_reg_domain_validation = struct
  let%expect_test "Clocking.reg with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg clocking data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg with clock and clear from different domains fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom2 ~name:"clr" ~width:1
        }
      in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.reg clocking data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.reg with data from different domain than clock fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let data = create_clocked_signal ~dom:dom2 ~name:"data" ~width:8 in
      ignore (Clocking.reg clocking data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_reg_no_clear_domain_validation = struct
  let%expect_test "Clocking.reg_no_clear with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg_no_clear clocking data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg_no_clear with data from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let data = create_clocked_signal ~dom:dom2 ~name:"data" ~width:8 in
      ignore (Clocking.reg_no_clear clocking data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_cut_through_reg_domain_validation = struct
  let%expect_test "Clocking.cut_through_reg with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.cut_through_reg clocking ~enable data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.cut_through_reg with enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.cut_through_reg clocking ~enable data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_pipeline_domain_validation = struct
  let%expect_test "Clocking.pipeline with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.pipeline clocking ~n:3 data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.pipeline with data from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let data = create_clocked_signal ~dom:dom2 ~name:"data" ~width:8 in
      ignore (Clocking.pipeline clocking ~n:3 data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.pipeline with enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.pipeline clocking ~enable ~n:3 data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_reg_fb_domain_validation = struct
  let%expect_test "Clocking.reg_fb with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    ignore
      (Clocking.reg_fb clocking ~width:8 ~f:(fun s ->
         Signal.(s +: Signal.of_unsigned_int ~width:8 1))
       : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg_fb with function returning different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      ignore
        (Clocking.reg_fb clocking ~width:8 ~f:(fun _s ->
           create_clocked_signal ~dom:dom2 ~name:"feedback" ~width:8)
         : Signal.t));
    [%expect
      {|
      ("[assign] expects the clock domain of the two signals to be compatible"
       ((dst (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (src (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_var_reg_domain_validation = struct
  let%expect_test "Clocking.Var.reg with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    ignore (Clocking.Var.reg clocking ~width:8 : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Var.reg with enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      ignore (Clocking.Var.reg clocking ~enable ~width:8 : Always.Variable.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_add_clear_domain_validation = struct
  let%expect_test "Clocking.add_clear with clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    let new_clocking = Clocking.add_clear clocking additional_clear in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg new_clocking data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.add_clear with clear from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr1" ~width:1
        }
      in
      let additional_clear = create_clocked_signal ~dom:dom2 ~name:"clr2" ~width:1 in
      ignore (Clocking.add_clear clocking additional_clear : Signal.t Clocking.t));
    [%expect
      {|
      ("|: expects the clock domain of the two operands to be compatible."
       (operands_with_mismatching_clock_domains (
         (lhs (
           Exact (
             (uid  1)
             (name domain_1_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
         (rhs (
           Exact (
             (uid  2)
             (name domain_2_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL)))))))
      |}]
  ;;
end

module%test Clocking_cdc_domain_validation = struct
  let%expect_test "Clocking.Cdc.stretch with signal from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let pulse = create_clocked_signal ~dom ~name:"pulse" ~width:1 in
    ignore (Clocking.Cdc.stretch clocking ~n:3 pulse : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Cdc.stretch with pulse from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let pulse = create_clocked_signal ~dom:dom2 ~name:"pulse" ~width:1 in
      ignore (Clocking.Cdc.stretch clocking ~n:3 pulse : Signal.t));
    [%expect
      {|
      ("[mux] expects the clock domain of the operands to be compatible, but they're not:"
       ((sel (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (case0 (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (case1 Constant)))
      |}]
  ;;

  let%expect_test "Clocking.Cdc.with_valid_pulse_detect_rising_edge with signal from \
                   same domain succeeds"
    =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let with_valid =
      { With_valid.valid = create_clocked_signal ~dom ~name:"valid" ~width:1
      ; value = create_clocked_signal ~dom ~name:"value" ~width:8
      }
    in
    ignore
      (Clocking.Cdc.with_valid_pulse_detect_rising_edge clocking with_valid
       : Signal.t With_valid.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Cdc.with_valid_pulse_detect_rising_edge with valid from \
                   different domain fails"
    =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let with_valid =
        { With_valid.valid = create_clocked_signal ~dom:dom2 ~name:"valid" ~width:1
        ; value = create_clocked_signal ~dom:dom1 ~name:"value" ~width:8
        }
      in
      ignore
        (Clocking.Cdc.with_valid_pulse_detect_rising_edge clocking with_valid
         : Signal.t With_valid.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.Cdc.reg_no_clear_with_async_reg_annotation with signal from \
                   same domain succeeds"
    =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore
      (Clocking.Cdc.reg_no_clear_with_async_reg_annotation
         clocking
         ~num_additional_pipeline_stages:2
         data
       : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Cdc.reg_no_clear_with_async_reg_annotation with data from \
                   different domain fails"
    =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let data = create_clocked_signal ~dom:dom2 ~name:"data" ~width:8 in
      ignore
        (Clocking.Cdc.reg_no_clear_with_async_reg_annotation
           clocking
           ~num_additional_pipeline_stages:2
           data
         : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_reg_fb_no_clear_domain_validation = struct
  let%expect_test "Clocking.reg_fb_no_clear with signals from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    ignore
      (Clocking.reg_fb_no_clear clocking ~width:8 ~f:(fun s ->
         Signal.(s +: Signal.of_unsigned_int ~width:8 1))
       : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg_fb_no_clear with function returning different domain \
                   fails"
    =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      ignore
        (Clocking.reg_fb_no_clear clocking ~width:8 ~f:(fun _s ->
           create_clocked_signal ~dom:dom2 ~name:"feedback" ~width:8)
         : Signal.t));
    [%expect
      {|
      ("[assign] expects the clock domain of the two signals to be compatible"
       ((dst (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (src (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_reg_override_arguments_domain_validation = struct
  let%expect_test "Clocking.reg with ~enable from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg clocking ~enable data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg with ~enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.reg clocking ~enable data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.reg with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg clocking ~clear:additional_clear data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg with ~clear from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr1" ~width:1
        }
      in
      let additional_clear = create_clocked_signal ~dom:dom2 ~name:"clr2" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.reg clocking ~clear:additional_clear data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.reg with ~clear_to from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let clear_to = Signal.of_unsigned_int ~width:8 42 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.reg clocking ~clear_to data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg with ~clear_to from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let clear_to = create_clocked_signal ~dom:dom2 ~name:"clear_to" ~width:8 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.reg clocking ~clear_to data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear_to (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.reg with multiple override arguments from same domain \
                   succeeds"
    =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    let clear_to = Signal.of_unsigned_int ~width:8 255 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore
      (Clocking.reg clocking ~enable ~clear:additional_clear ~clear_to data : Signal.t);
    [%expect {| |}]
  ;;
end

module%test Clocking_cut_through_reg_override_arguments_domain_validation = struct
  let%expect_test "Clocking.cut_through_reg with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore
      (Clocking.cut_through_reg clocking ~enable ~clear:additional_clear data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.cut_through_reg with ~clear from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr1" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom1 ~name:"enable" ~width:1 in
      let additional_clear = create_clocked_signal ~dom:dom2 ~name:"clr2" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore
        (Clocking.cut_through_reg clocking ~enable ~clear:additional_clear data
         : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.cut_through_reg with ~clear_to from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let clear_to = Signal.of_unsigned_int ~width:8 128 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.cut_through_reg clocking ~enable ~clear_to data : Signal.t);
    [%expect {| |}]
  ;;
end

module%test Clocking_pipeline_override_arguments_domain_validation = struct
  let%expect_test "Clocking.pipeline with ~enable from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.pipeline clocking ~enable ~n:2 data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.pipeline with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.pipeline clocking ~clear:additional_clear ~n:2 data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.pipeline with ~clear_to from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let clear_to = Signal.of_unsigned_int ~width:8 99 in
    let data = create_clocked_signal ~dom ~name:"data" ~width:8 in
    ignore (Clocking.pipeline clocking ~clear_to ~n:2 data : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.pipeline with ~enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      let data = create_clocked_signal ~dom:dom1 ~name:"data" ~width:8 in
      ignore (Clocking.pipeline clocking ~enable ~n:2 data : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Clocking_reg_fb_override_arguments_domain_validation = struct
  let%expect_test "Clocking.reg_fb with ~enable from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    ignore
      (Clocking.reg_fb clocking ~enable ~width:8 ~f:(fun s ->
         Signal.(s +: of_unsigned_int ~width:8 1))
       : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg_fb with ~enable from different domain fails" =
    reset_uid_for_test ();
    let dom1 = create_exact_domain clock_domain_1 in
    let dom2 = create_exact_domain clock_domain_2 in
    require_does_raise (fun () ->
      let clocking =
        { Clocking.clock = create_clocked_signal ~dom:dom1 ~name:"clk" ~width:1
        ; clear = create_clocked_signal ~dom:dom1 ~name:"clr" ~width:1
        }
      in
      let enable = create_clocked_signal ~dom:dom2 ~name:"enable" ~width:1 in
      ignore
        (Clocking.reg_fb clocking ~enable ~width:8 ~f:(fun s ->
           Signal.(s +: of_unsigned_int ~width:8 1))
         : Signal.t));
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (in (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  2)
            (name domain_2_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))
        (clear (
          Exact (
            (uid  1)
            (name domain_1_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocking_clocked.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "Clocking.reg_fb with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    ignore
      (Clocking.reg_fb clocking ~clear:additional_clear ~width:8 ~f:(fun s ->
         Signal.(s +: of_unsigned_int ~width:8 1))
       : Signal.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.reg_fb with ~clear_to from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let clear_to = Signal.of_unsigned_int ~width:8 10 in
    ignore
      (Clocking.reg_fb clocking ~clear_to ~width:8 ~f:(fun s ->
         Signal.(s +: of_unsigned_int ~width:8 1))
       : Signal.t);
    [%expect {| |}]
  ;;
end

module%test Clocking_var_reg_override_arguments_domain_validation = struct
  let%expect_test "Clocking.Var.reg with ~enable from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    ignore (Clocking.Var.reg clocking ~enable ~width:8 : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Var.reg with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    ignore
      (Clocking.Var.reg clocking ~clear:additional_clear ~width:8 : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Var.reg with ~clear_to from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let clear_to = Signal.of_unsigned_int ~width:8 77 in
    ignore (Clocking.Var.reg clocking ~clear_to ~width:8 : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Var.reg_with_int_default with ~enable from same domain \
                   succeeds"
    =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    ignore
      (Clocking.Var.reg_with_int_default clocking ~enable ~width:8 ~clear_to:123
       : Always.Variable.t);
    [%expect {| |}]
  ;;

  let%expect_test "Clocking.Var.cut_through_reg with ~clear from same domain succeeds" =
    reset_uid_for_test ();
    let dom = create_exact_domain clock_domain_1 in
    let clocking =
      { Clocking.clock = create_clocked_signal ~dom ~name:"clk" ~width:1
      ; clear = create_clocked_signal ~dom ~name:"clr1" ~width:1
      }
    in
    let enable = create_clocked_signal ~dom ~name:"enable" ~width:1 in
    let additional_clear = create_clocked_signal ~dom ~name:"clr2" ~width:1 in
    ignore
      (Clocking.Var.cut_through_reg clocking ~enable ~clear:additional_clear ~width:8
       : Always.Variable.t);
    [%expect {| |}]
  ;;
end
