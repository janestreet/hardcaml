open Core
open Expect_test_helpers_base
open Hardcaml
open Clocked_design
open Signal

let g1 = Clock_domain.create "g1"
let g2 = Clock_domain.create "g2"

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

let foo = Clock_domain.Exact.Expert.create_domain "foo"
let bar = Clock_domain.Exact.Expert.create_domain "bar"
let print_s sexp = print_s ~hide_positions:true sexp

let print_concrete_domains (outputs : _ O.t) =
  print_s ([%sexp_of: Clock_domain.Runtime.t O.t] (O.map ~f:Signal.get_domain outputs))
;;

let require_does_raise = require_does_raise ~hide_positions:true
let trivial_create_fn (_scope : Scope.t) { I.a; b } = { O.x = a; y = b }

let instantiate ~input_domains ~output_domains ?(create_fn = trivial_create_fn) inputs =
  let module H =
    Hierarchy.In_clocked_scope
      (struct
        include I

        let domains = input_domains
      end)
      (struct
        include O

        let domains = output_domains
      end)
  in
  let scope = Scope.create ~flatten_design:false () in
  H.hierarchical ~caller_signal_type:Clocked ~scope create_fn inputs
;;

let instantiate_unclocked
  ~input_domains
  ~output_domains
  ?(create_fn = trivial_create_fn)
  inputs
  =
  let module H =
    Hierarchy.In_clocked_scope
      (struct
        include I

        let domains = input_domains
      end)
      (struct
        include O

        let domains = output_domains
      end)
  in
  let scope = Scope.create ~flatten_design:true () in
  let (_ : _ O.t) = H.hierarchical ~caller_signal_type:Signal ~scope create_fn inputs in
  ()
;;

module%test Both_domains_specified_on_clocked_caller = struct
  let%expect_test "valid examples" =
    (* When domains are specified, constants are coerced to a specified domain. As in this
       case, the input domains doesn't have a concrete domain associated ot the dangling
       domain, they're all assigned a generated domain, as implied by the _gen suffix in
       the name.
    *)
    print_concrete_domains
      (instantiate
         ~input_domains:(I.const g1)
         ~output_domains:(O.const g1)
         { I.a = vdd; b = vdd });
    [%expect
      {|
      ((x (
         Exact (
           (uid  3)
           (name g1_gen)
           (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL))))
       (y (
         Exact (
           (uid  3)
           (name g1_gen)
           (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL)))))
      |}];
    (* The following tests non-constants.

       The interesting behaviour we're testing here is that even though one of the inputs
       is a constant, on the way our, we coerce the signals all to the same clock domain,
       including those that would have been a constant if the specification is not
       specified.
    *)
    print_s ([%sexp_of: Clock_domain.Exact.t] foo);
    [%expect
      {|
      ((uid  1)
       (name foo)
       (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))
      |}];
    let inputs_to_test =
      [ { I.a = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
        ; b = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
        }
      ; { I.a = vdd; b = Signal.Unsafe.set_domain vdd ~dom:(Exact foo) }
      ]
    in
    List.iter inputs_to_test ~f:(fun inputs ->
      let output =
        instantiate ~input_domains:(I.const g1) ~output_domains:(O.const g1) inputs
      in
      O.iter output ~f:(fun s ->
        assert (
          Clock_domain.Exact.equal
            foo
            (Clock_domain.Runtime.exact_exn (Signal.get_domain s)))));
    (* Simple example with multiple clock domains. *)
    print_concrete_domains
      (instantiate
         ~input_domains:{ a = g1; b = g2 }
         ~output_domains:{ x = g1; y = g2 }
         { I.a = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
         ; b = Signal.Unsafe.set_domain vdd ~dom:(Exact bar)
         });
    [%expect
      {|
      ((x (
         Exact (
           (uid  1)
           (name foo)
           (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))))
       (y (
         Exact (
           (uid  2)
           (name bar)
           (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))))
      |}];
    (* Multiple clock domains in specification, but in runtime, both of them map to the
       same concrete clock domain.
    *)
    print_concrete_domains
      (instantiate
         ~input_domains:{ a = g1; b = g2 }
         ~output_domains:{ x = g1; y = g2 }
         { I.a = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
         ; b = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
         });
    [%expect
      {|
      ((x (
         Exact (
           (uid  1)
           (name foo)
           (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))))
       (y (
         Exact (
           (uid  1)
           (name foo)
           (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))))
      |}]
  ;;

  let%expect_test "invalid examples" =
    (* Raises when clock domains on the input doesn't match the given specification *)
    require_does_raise (fun () ->
      instantiate
        ~input_domains:(I.const g1)
        ~output_domains:(O.const g1)
        { I.a = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
        ; b = Signal.Unsafe.set_domain vdd ~dom:(Exact bar)
        });
    [%expect
      {|
      ("The following input ports are expected to have the same clock domain, but do not"
       (clock_domain (
         (uid  8)
         (name g1)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))
       ((a (
          Exact (
            (uid  1)
            (name foo)
            (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))))
        (b (
          Exact (
            (uid  2)
            (name bar)
            (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))))))
      |}];
    (* Raises when clock domains on the output doesn't match the given specification *)
    require_does_raise (fun () ->
      instantiate
        ~input_domains:{ I.a = g1; b = g2 }
        ~output_domains:{ O.x = g2; y = g1 }
        { I.a = Signal.Unsafe.set_domain vdd ~dom:(Exact foo)
        ; b = Signal.Unsafe.set_domain vdd ~dom:(Exact bar)
        });
    [%expect
      {|
      ("The output port x has an incorrect clock domain."
       (provided (
         (uid  1)
         (name foo)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))
       (expected (
         (uid  2)
         (name bar)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL))))
      |}];
    (* One of the inputs is an unknown clock domain *)
    require_does_raise (fun () ->
      instantiate
        ~input_domains:(I.const g1)
        ~output_domains:(O.const g1)
        { I.a = Signal.Unsafe.set_domain vdd ~dom:Constant
        ; b = Signal.Unsafe.set_domain vdd ~dom:Unknown
        });
    [%expect
      {|
      ("The following input ports are expected to have the same clock domain, but one of them is unexpectedly an unknown."
       (clock_domain (
         (uid  8)
         (name g1)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))
       (port_names_with_unknown_clock_domains (b)))
      |}];
    (* One of the domain output is an unknown *)
    require_does_raise (fun () ->
      instantiate
        ~create_fn:(fun (_ : Scope.t) (i : _ I.t) ->
          { O.x = i.a; y = Clocked_signal.Unsafe.set_domain i.b ~dom:Unknown })
        ~input_domains:{ a = g1; b = g2 }
        ~output_domains:{ x = g1; y = g2 }
        { I.a = Signal.Unsafe.set_domain vdd ~dom:Constant
        ; b = Signal.Unsafe.set_domain vdd ~dom:Unknown
        });
    [%expect
      {|
      ("The following input ports are expected to have the same clock domain, but one of them is unexpectedly an unknown."
       (clock_domain (
         (uid  9)
         (name g2)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))
       (port_names_with_unknown_clock_domains (b)))
      |}]
  ;;
end

module%test Both_domains_specified_on_unclocked_caller = struct
  module Signal = Hardcaml.Signal

  let%expect_test "valid examples" =
    (* Trivially valid cases *)
    instantiate_unclocked
      ~input_domains:(I.const g1)
      ~output_domains:(O.const g1)
      { I.a = Signal.vdd; b = Signal.vdd };
    instantiate_unclocked
      ~input_domains:{ a = g1; b = g2 }
      ~output_domains:{ x = g1; y = g2 }
      { I.a = Signal.vdd; b = Signal.vdd };
    (* The following is valid because, [g2] is a dangling clock domain specification in
       the output interface, but got bound to whatever [g1] is bound to after elaborating
       the implementation.
    *)
    instantiate_unclocked
      ~input_domains:(I.const g1)
      ~output_domains:{ O.x = g1; y = g2 }
      { I.a = Signal.vdd; b = Signal.vdd };
    (* This is valid as constants is compatable with all clock domains *)
    instantiate_unclocked
      ~input_domains:(I.const g1)
      ~output_domains:(O.const g1)
      ~create_fn:(fun (_ : Scope.t) (_ : _ I.t) ->
        { O.x = Clocked_signal.vdd; y = Clocked_signal.vdd })
      { I.a = Signal.vdd; b = Signal.vdd };
    (* Similar to above, there are mix of constants and non-constants in a clock domain.
       The expect test output below shows the domain of [input.a] below.
    *)
    instantiate_unclocked
      ~input_domains:(I.const g1)
      ~output_domains:(O.const g1)
      ~create_fn:(fun (_ : Scope.t) (i : _ I.t) ->
        let dom_a = Clocked_signal.get_domain i.a in
        print_s [%message (dom_a : Clock_domain.Runtime.t)];
        { O.x = i.a; y = Clocked_signal.vdd })
      { I.a = Signal.vdd; b = Signal.vdd };
    [%expect
      {|
      (dom_a (
        Exact (
          (uid  10)
          (name g1_gen)
          (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL))))
      |}]
  ;;

  let%expect_test "invalid examples" =
    (* The following is invalid as [output.x] is expected to be bound to the concrete
       domain of [input.a], but got the concreate domain of [input.b] instead.
    *)
    require_does_raise (fun () ->
      instantiate_unclocked
        ~input_domains:{ a = g1; b = g2 }
        ~output_domains:{ O.x = g1; y = g2 }
        ~create_fn:(fun (_ : Scope.t) (i : _ I.t) -> { O.x = i.b; y = i.b })
        { I.a = Signal.vdd; b = Signal.vdd });
    [%expect
      {|
      ("The output port x has an incorrect clock domain."
       (provided (
         (uid  11)
         (name g2_gen)
         (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL)))
       (expected (
         (uid  12)
         (name g1_gen)
         (loc  lib/hardcaml/hardcaml/src/hierarchy.ml:LINE:COL))))
      |}];
    (* This is invalid due to a present of an unknown clock domain in the implementation *)
    require_does_raise (fun () ->
      instantiate_unclocked
        ~input_domains:{ a = g1; b = g2 }
        ~output_domains:{ O.x = g1; y = g2 }
        ~create_fn:(fun (_ : Scope.t) (i : _ I.t) ->
          { O.x = Clocked_signal.Unsafe.set_domain ~dom:Unknown i.a; y = i.b })
        { I.a = Signal.vdd; b = Signal.vdd });
    [%expect
      {|
      ("The following output ports are expected to have the same clock domain, but one of them is unexpectedly an unknown."
       (clock_domain (
         (uid  8)
         (name g1)
         (loc lib/hardcaml/hardcaml/test/lib/test_hierarchy_clocked.ml:LINE:COL)))
       (port_names_with_unknown_clock_domains (x)))
      |}]
  ;;
end
