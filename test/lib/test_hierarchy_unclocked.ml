open! Core
open! Expect_test_helpers_base
open Hardcaml
open! Signal

let g1 = Clock_domain.create "g1"
let g2 = Clock_domain.create "g2"
let foo = Clock_domain.Exact.Expert.create_domain "foo"
let bar = Clock_domain.Exact.Expert.create_domain "bar"

module I = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    }
  [@@deriving hardcaml]
end

module H = Hierarchy.In_scope (I) (O)

let print_s sexp = print_s ~hide_positions:true sexp
let require_does_raise = require_does_raise ~hide_positions:true

let print_concrete_domains (outputs : _ O.t) =
  print_s
    ([%sexp_of: Clock_domain.Runtime.t O.t] (O.map ~f:Clocked_signal.get_domain outputs))
;;

let trivial_create_fn (_scope : Scope.t) { I.a; b } = { O.x = a; y = b }

let instantiate ?(create_fn = trivial_create_fn) ~input_domains ~output_domains inputs =
  let scope = Scope.create ~flatten_design:false () in
  H.hierarchical_unclocked
    ~input_domains
    ~output_domains
      (* We are not going to test [caller_signal_type:Signal] as it is a noop. *)
    ~caller_signal_type:Clocked
    ~scope
    create_fn
    inputs
;;

open Clocked_signal

let%expect_test "valid examples" =
  print_concrete_domains
    (instantiate
       ~input_domains:(I.const g1)
       ~output_domains:(O.const g1)
       { I.a = vdd; b = gnd });
  [%expect
    {|
    ((x (
       Exact (
         (uid  5)
         (name g1_gen)
         (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL))))
     (y (
       Exact (
         (uid  5)
         (name g1_gen)
         (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL)))))
    |}];
  let inputs_to_test =
    [ { I.a = Unsafe.set_domain vdd ~dom:(Exact foo)
      ; b = Unsafe.set_domain vdd ~dom:(Exact foo)
      }
    ; { I.a = vdd; b = Unsafe.set_domain vdd ~dom:(Exact foo) }
    ]
  in
  List.iter inputs_to_test ~f:(fun inputs ->
    let output =
      instantiate ~input_domains:(I.const g1) ~output_domains:(O.const g1) inputs
    in
    O.iter output ~f:(fun s ->
      assert (Clock_domain.Exact.equal foo (Clock_domain.Runtime.exact_exn (get_domain s)))));
  (* Simple example with multiple clock domains. *)
  print_concrete_domains
    (instantiate
       ~input_domains:{ a = g1; b = g2 }
       ~output_domains:{ x = g1; y = g2 }
       { I.a = Unsafe.set_domain vdd ~dom:(Exact foo)
       ; b = Unsafe.set_domain vdd ~dom:(Exact bar)
       });
  [%expect
    {|
    ((x (
       Exact (
         (uid  3)
         (name foo)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL))))
     (y (
       Exact (
         (uid  4)
         (name bar)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL)))))
    |}];
  (* Multiple clock domains in specification, but in runtime, both of them map to the same
     concrete clock domain.
  *)
  print_concrete_domains
    (instantiate
       ~input_domains:{ a = g1; b = g2 }
       ~output_domains:{ x = g1; y = g2 }
       { I.a = Unsafe.set_domain vdd ~dom:(Exact foo)
       ; b = Unsafe.set_domain vdd ~dom:(Exact foo)
       });
  [%expect
    {|
    ((x (
       Exact (
         (uid  3)
         (name foo)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL))))
     (y (
       Exact (
         (uid  3)
         (name foo)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL)))))
    |}]
;;

let%expect_test "invalid examples" =
  (* Raises when clock domains on the input doesn't match the given specification *)
  require_does_raise (fun () ->
    instantiate
      ~input_domains:(I.const g1)
      ~output_domains:(O.const g1)
      { I.a = Unsafe.set_domain vdd ~dom:(Exact foo)
      ; b = Unsafe.set_domain vdd ~dom:(Exact bar)
      });
  [%expect
    {|
    ("The following input ports are expected to have the same clock domain, but do not"
     (clock_domain (
       (uid  10)
       (name g1)
       (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL)))
     ((a (
        Exact (
          (uid  3)
          (name foo)
          (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL))))
      (b (
        Exact (
          (uid  4)
          (name bar)
          (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL))))))
    |}];
  (* One of the inputs is an unknown clock domain *)
  require_does_raise (fun () ->
    instantiate
      ~input_domains:(I.const g1)
      ~output_domains:(O.const g1)
      { I.a = Unsafe.set_domain vdd ~dom:Constant
      ; b = Unsafe.set_domain vdd ~dom:Unknown
      });
  [%expect
    {|
    ("The following input ports are expected to have the same clock domain, but one of them is unexpectedly an unknown."
     (clock_domain (
       (uid  10)
       (name g1)
       (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_unclocked.ml:LINE:COL)))
     (port_names_with_unknown_clock_domains (b)))
    |}]
;;
