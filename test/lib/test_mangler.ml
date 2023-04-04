open! Import
open! Mangler

let%expect_test "case-sensitive mangler" =
  let t = create ~case_sensitive:true in
  add_identifiers_exn t [ "fudge"; "bramble" ];
  (* must not clash with reserved works *)
  print_s
    [%message
      "adding existing identifier" ~_:(add_identifier t "fudge" : [ `Ok | `Duplicate ])];
  [%expect {|
    ("adding existing identifier" Duplicate) |}];
  show_raise (fun () -> add_identifiers_exn t [ "bramble" ]);
  [%expect
    {|
    (raised (
      "Failed to add identifier to mangler as it is already present"
      (invalid_identifier bramble))) |}];
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
         (spoodle 0)))))) |}];
  mangle "spoodle";
  [%expect
    {|
    ((name         spoodle)
     (mangled_name spoodle_0)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble   0)
         (fudge     0)
         (spoodle   1)
         (spoodle_0 0)))))) |}];
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
         (spoodle_0 0)
         (spoodle_1 0)))))) |}];
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
         (labradoodle 0)
         (spoodle     2)
         (spoodle_0   0)
         (spoodle_1   0)))))) |}];
  (* try to subvert mangler by adding the next mangled name *)
  mangle "labradoodle_0";
  [%expect
    {|
    ((name         labradoodle_0)
     (mangled_name labradoodle_0)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble       0)
         (fudge         0)
         (labradoodle   0)
         (labradoodle_0 0)
         (spoodle       2)
         (spoodle_0     0)
         (spoodle_1     0)))))) |}];
  mangle "labradoodle";
  [%expect
    {|
    ((name         labradoodle)
     (mangled_name labradoodle_0_0)
     (mangler (
       (case_sensitive true)
       (table (
         (bramble         0)
         (fudge           0)
         (labradoodle     1)
         (labradoodle_0   1)
         (labradoodle_0_0 0)
         (spoodle         2)
         (spoodle_0       0)
         (spoodle_1       0)))))) |}]
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
     (t ((case_sensitive false) (table ((fudge 0)))))) |}];
  mangle "FUDGE";
  [%expect
    {|
    ((name         FUDGE)
     (mangled_name FUDGE_0)
     (t (
       (case_sensitive false)
       (table (
         (fudge   1)
         (FUDGE_0 0)))))) |}]
;;
