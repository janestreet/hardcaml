open! Import

(* We cannot run these tests in CI as they are not stable when code is compiled with
   different build options, such as flambda. *)

(* {[
     let%expect_test "Top_of_stack" =
       Caller_id.set_mode Top_of_stack;
       require_does_raise [%here]
         (fun () -> Signal.(of_int ~width:1 0 +: of_int ~width:2 0) |> ignore);
       [%expect {|
    ("[+:] got inputs of different widths"
      ((const
         (loc   test_callerid.ml:LINE:COL)
         (width 1)
         (value 0b0))
       (const
         (loc   test_callerid.ml:LINE:COL)
         (width 2)
         (value 0b00)))
      (loc test_callerid.ml:LINE:COL)) |}]
     ;;

     let%expect_test "Full" =
       Caller_id.set_mode Full_trace;
       require_does_raise [%here]
         (fun () -> Signal.(of_int ~width:1 0 +: of_int ~width:2 0) |> ignore);
       [%expect {|
    ("[+:] got inputs of different widths"
      ((const
         (loc (
           (caller_id.ml:LINE:COL)
           (signal.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           (expect_test_helpers_base.ml:LINE:COL)
           (exn.ml:LINE:COL)
           (expect_test_helpers_base.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           (expect_test_collector.ml:LINE:COL)
           (expect_test_collector.ml:LINE:COL)
           (runtime.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           ()))
         (width 1)
         (value 0b0))
       (const
         (loc (
           (caller_id.ml:LINE:COL)
           (signal.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           (expect_test_helpers_base.ml:LINE:COL)
           (exn.ml:LINE:COL)
           (expect_test_helpers_base.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           (expect_test_collector.ml:LINE:COL)
           (expect_test_collector.ml:LINE:COL)
           (runtime.ml:LINE:COL)
           (test_callerid.ml:LINE:COL)
           ()))
         (width 2)
         (value 0b00)))
      (loc (
        (caller_id.ml:LINE:COL)
        (comb.ml:LINE:COL)
        (comb.ml:LINE:COL)
        (test_callerid.ml:LINE:COL)
        (expect_test_helpers_base.ml:LINE:COL)
        (exn.ml:LINE:COL)
        (expect_test_helpers_base.ml:LINE:COL)
        (test_callerid.ml:LINE:COL)
        (expect_test_collector.ml:LINE:COL)
        (expect_test_collector.ml:LINE:COL)
        (runtime.ml:LINE:COL)
        (test_callerid.ml:LINE:COL)
        ()))) |}]
     ;;
   ]} *)
