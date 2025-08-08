open Core
open Hardcaml
module C = Clock_domain

let test alist =
  (* We don't rely on the UIDs being unique across tests. Calling this here helps with
     test stability.
  *)
  Clock_domain.Exact.Expert.reset_uid ();
  let mapping = C.construct_mapping_from_spec_to_exact alist in
  print_s ([%sexp_of: C.mapped_clock_domain C.Uid.Map.t] mapping);
  Expect_test_helpers_base.hide_positions_in_expect_test_output ()
;;

let g_read =
  (* Use [create_with_uid] so that the uids are stable as tests in the tree changes. This
     is technically unsafe, but okay for this test since it's self-contained.
  *)
  C.Expert.create_with_uid "read" C.Uid.one
;;

let%expect_test "construct_mapping_from_spec_to_concrete doesn't allow a spec to map to \
                 multiple concrete domains"
  =
  test
    [ g_read, Exact (C.generate_fresh_exact_domain g_read)
    ; g_read, Exact (C.generate_fresh_exact_domain g_read)
    ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to
        (Error
         (Maps_to_multiple
          (((uid 2) (name read_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL))
           ((uid 1) (name read_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))))))))
    |}]
;;

let%expect_test "construct_mapping_from_spec_to_concrete allow a spec that maps to a \
                 concrete to show up multiple times in the list."
  =
  let concrete = C.generate_fresh_exact_domain g_read in
  test [ g_read, Exact concrete; g_read, Exact concrete ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to
        (Ok
         (Exact
          ((uid 1) (name read_gen)
           (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL))))))))
    |}]
;;

let%expect_test "construct_mapping_from_spec_to_concrete correctly handle constants" =
  let concrete = C.generate_fresh_exact_domain g_read in
  test [ g_read, Exact concrete; g_read, Constant ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to
        (Ok
         (Exact
          ((uid 1) (name read_gen)
           (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL))))))))
    |}]
;;

let%expect_test "construct_mapping_from_spec_to_concrete disallows Unknowns" =
  let concrete = C.generate_fresh_exact_domain g_read in
  test [ g_read, Exact concrete; g_read, Unknown ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to (Error Maps_to_unknown)))))
    |}];
  test [ g_read, Unknown ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to (Error Maps_to_unknown)))))
    |}]
;;

let%expect_test "construct_mapping_from_spec_to_concrete correctly handles spec entry \
                 whose concreate signals are constants"
  =
  test [ g_read, Constant; g_read, Constant ];
  [%expect
    {|
    ((1
      ((specification_domain
        ((uid 1) (name read)
         (loc lib/hardcaml/hardcaml/test/lib/test_clock_domain.ml:LINE:COL)))
       (maps_to (Ok All_constants)))))
    |}]
;;
