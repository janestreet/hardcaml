open! Import
open! Mangler

let%expect_test "case-sensitive mangler" =
  let t = create ~case_sensitive:true in
  add_identifiers_exn t [ "fudge"; "bramble" ];
  (* must not clash with reserved works *)
  print_s
    [%message
      "adding existing identifier" ~_:(add_identifier t "fudge" : [ `Ok | `Duplicate ])];
  [%expect {| ("adding existing identifier" Duplicate) |}];
  show_raise (fun () -> add_identifiers_exn t [ "bramble" ]);
  [%expect
    {|
    (raised (
      "Failed to add identifier to mangler as it is already present"
      (invalid_identifier bramble)))
    |}];
  let mangle name =
    let mangled_name = mangle t name in
    print_s [%message (name : string) (mangled_name : string) ~mangler:(t : t)]
  in
  mangle "spoodle";
  [%expect
    {|
    ((name         spoodle)
     (mangled_name spoodle)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble 0)
         (fudge   0)
         (spoodle 1))))))
    |}];
  mangle "spoodle";
  [%expect
    {|
    ((name         spoodle)
     (mangled_name spoodle_1)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble   0)
         (fudge     0)
         (spoodle   2)
         (spoodle_1 1))))))
    |}];
  mangle "spoodle";
  [%expect
    {|
    ((name         spoodle)
     (mangled_name spoodle_2)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble   0)
         (fudge     0)
         (spoodle   3)
         (spoodle_1 1)
         (spoodle_2 1))))))
    |}];
  mangle "labradoodle";
  [%expect
    {|
    ((name         labradoodle)
     (mangled_name labradoodle)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble     0)
         (fudge       0)
         (labradoodle 1)
         (spoodle     3)
         (spoodle_1   1)
         (spoodle_2   1))))))
    |}];
  (* try to subvert mangler by adding the next mangled name *)
  mangle "labradoodle_1";
  [%expect
    {|
    ((name         labradoodle_1)
     (mangled_name labradoodle_1)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble       0)
         (fudge         0)
         (labradoodle   1)
         (labradoodle_1 1)
         (spoodle       3)
         (spoodle_1     1)
         (spoodle_2     1))))))
    |}];
  mangle "labradoodle";
  [%expect
    {|
    ((name         labradoodle)
     (mangled_name labradoodle_1_1)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble         0)
         (fudge           0)
         (labradoodle     2)
         (labradoodle_1   2)
         (labradoodle_1_1 1)
         (spoodle         3)
         (spoodle_1       1)
         (spoodle_2       1))))))
    |}]
;;

let%expect_test "case-insensitive mangler" =
  let t = create ~case_sensitive:false in
  let mangle name =
    let mangled_name = mangle t name in
    print_s [%message (name : string) (mangled_name : string) (t : t)]
  in
  mangle "fudge";
  [%expect
    {|
    ((name         fudge)
     (mangled_name fudge)
     (t ((case_sensitive false) (table ((fudge 1))))))
    |}];
  mangle "FUDGE";
  [%expect
    {|
    ((name         FUDGE)
     (mangled_name FUDGE_1)
     (t (
       (case_sensitive false)
       (table (
         (fudge   2)
         (FUDGE_1 1))))))
    |}]
;;
