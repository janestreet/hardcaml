open Core
open Hardcaml
open Expect_test_helpers_base

let g = Clock_domain.create "foo"

module%test Validate_signals_are_consistent = struct
  let test list =
    print_s
      ~hide_positions:true
      ([%sexp_of: Clock_domain.Runtime.t]
         (Clocked_signal.validate_signals_are_consistent ~op_name:"test_op" list))
  ;;

  let%expect_test "non-constants are respected in \
                   [validate_signals_has_compataible_concreate_domains]"
    =
    let dom = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    test
      [ "foo", Clocked_signal.to_clocked ~dom (Signal.input "x" 1)
      ; "bar", Clocked_signal.to_clocked ~dom (Signal.input "y" 1)
      ];
    [%expect
      {|
      (Exact (
        (uid  1)
        (name foo_gen)
        (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))
      |}]
  ;;

  let%expect_test "Empty is not compatible with each other, or even on its own" =
    require_does_raise (fun () ->
      test [ "x", Clocked_signal.to_clocked ~dom:Unknown (Signal.input "x" 1) ]);
    [%expect
      {|
      ("[test_op] does not allow any of the the operands to have an unknown clock domain. Operands with unknown clock domains:"
       (x))
      |}];
    require_does_raise (fun () ->
      test
        [ "x", Clocked_signal.to_clocked ~dom:Unknown (Signal.input "x" 1)
        ; "y", Clocked_signal.to_clocked ~dom:Unknown (Signal.input "y" 1)
        ]);
    [%expect
      {|
      ("[test_op] does not allow any of the the operands to have an unknown clock domain. Operands with unknown clock domains:"
       (x y))
      |}];
    require_does_raise (fun () ->
      test
        [ "x", Clocked_signal.to_clocked ~dom:Unknown (Signal.input "x" 1)
        ; "y", Clocked_signal.to_clocked ~dom:Constant (Signal.input "y" 1)
        ]);
    [%expect
      {|
      ("[test_op] does not allow any of the the operands to have an unknown clock domain. Operands with unknown clock domains:"
       (x))
      |}]
  ;;

  let%expect_test "Constants are ignored in \
                   [validate_signals_has_compataible_concreate_domains]"
    =
    let dom = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    test
      [ "foo", Clocked_signal.to_clocked ~dom (Signal.of_unsigned_int ~width:32 1)
      ; "bar", Clocked_signal.to_clocked ~dom (Signal.input "foo" 1)
      ];
    [%expect
      {|
      (Exact (
        (uid  2)
        (name foo_gen)
        (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))
      |}]
  ;;

  let%expect_test "Mismatch non-constant domains in \
                   [validate_signals_has_compataible_concreate_domains] raises"
    =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    require_does_raise ~hide_positions:true (fun () ->
      test
        [ "foo", Clocked_signal.to_clocked ~dom:dom1 (Signal.input "y" 1)
        ; "bar", Clocked_signal.to_clocked ~dom:dom2 (Signal.input "x" 1)
        ]);
    [%expect
      {|
      ("[test_op] expects the clock domain of the operands to be compatible, but they're not:"
       ((foo (
          Exact (
            (uid  3)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (bar (
          Exact (
            (uid  4)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}]
  ;;
end

module%test Ensure_domain = struct
  let test ~required_domain actual =
    ignore
      (Clocked_signal.ensure_domain
         ~required_domain
         (Clocked_signal.Unsafe.set_domain Clocked_signal.vdd ~dom:actual)
       : Clocked_signal.t)
  ;;

  let test_require_raises ~required_domain actual =
    require_does_raise ~hide_positions:true (fun () ->
      Clocked_signal.ensure_domain
        ~required_domain
        (Clocked_signal.Unsafe.set_domain Clocked_signal.vdd ~dom:actual))
  ;;

  let c1 = Clock_domain.generate_fresh_exact_domain g
  let c2 = Clock_domain.generate_fresh_exact_domain g

  let%expect_test "required_domain is Constant" =
    test ~required_domain:Constant Constant;
    [%expect {| |}];
    test_require_raises ~required_domain:Constant Unknown;
    [%expect
      {| "[ensure_domain] is called with actual_domain = Unknown. This is not allowed. A value with an unknown clock domain is not comaptible with any runtime clock domain including Unknown itself." |}];
    test_require_raises ~required_domain:Constant (Exact c1);
    [%expect
      {|
      ("required domain is [Constant], but the value's provided domain is an exact domain."
       (actual_domain (
         Exact (
           (uid  5)
           (name foo_gen)
           (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))))
      |}]
  ;;

  let%expect_test "required_domain is Unknown" =
    test_require_raises ~required_domain:Unknown Constant;
    [%expect
      {| "[ensure_domain] is called with required_domain = Unknown. This is not allowed" |}];
    test_require_raises ~required_domain:Unknown Unknown;
    [%expect
      {| "[ensure_domain] is called with required_domain = Unknown. This is not allowed" |}];
    test_require_raises ~required_domain:Unknown (Exact c1);
    [%expect
      {| "[ensure_domain] is called with required_domain = Unknown. This is not allowed" |}]
  ;;

  let%expect_test "required_domain is Exact" =
    test ~required_domain:(Exact c1) Constant;
    [%expect {| |}];
    test ~required_domain:(Exact c1) (Exact c1);
    [%expect {| |}];
    test_require_raises ~required_domain:(Exact c1) Unknown;
    [%expect
      {| "[ensure_domain] is called with actual_domain = Unknown. This is not allowed. A value with an unknown clock domain is not comaptible with any runtime clock domain including Unknown itself." |}];
    test_require_raises ~required_domain:(Exact c1) (Exact c2);
    [%expect
      {|
      ("[ensure_domain] Non-const Signal has unexpected clock domain"
       (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)
       (signal (
         (base (
           const
           (names (vdd))
           (width 1)
           (value 0b1)))
         (dom (
           Exact (
             (uid  6)
             (name foo_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
       (expected (
         (uid  5)
         (name foo_gen)
         (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))
       (actual (
         (uid  6)
         (name foo_gen)
         (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
      |}]
  ;;
end

(* Verify that the signal operations does appropriate clock domain checking *)
module%test Signal_ops = struct
  let%expect_test "binops" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid f =
      let dom = dom1 in
      ignore
        (f
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "foo" 1) ~dom)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "bar" 1) ~dom)
         : Clocked_signal.t)
    in
    let test_invalid f =
      ignore
        (f
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "foo" 1) ~dom:dom1)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "bar" 1) ~dom:dom2)
         : Clocked_signal.t)
    in
    let test ?(display_output = false) f =
      test_valid f;
      require_does_raise ~hide_positions:true (fun () -> test_invalid f);
      (* Swallow the test output when it's not all that meaningful *)
      if not display_output then ignore (expect_test_output () : string)
    in
    (* Display output only for 1 *)
    test ~display_output:true Clocked_signal.( +: );
    [%expect
      {|
      ("+: expects the clock domain of the two operands to be compatible."
       (operands_with_mismatching_clock_domains (
         (lhs (
           Exact (
             (uid  7)
             (name foo_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
         (rhs (
           Exact (
             (uid  8)
             (name foo_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))))))
      |}];
    test Clocked_signal.( -: );
    test Clocked_signal.( &: );
    test Clocked_signal.( *: );
    test Clocked_signal.( |: );
    test Clocked_signal.( ^: );
    test Clocked_signal.( *+ );
    test Clocked_signal.( <: );
    test Clocked_signal.( <+ );
    test Clocked_signal.( >: );
    test Clocked_signal.( >+ );
    test Clocked_signal.( >=: );
    test Clocked_signal.( >=+ );
    test Clocked_signal.( <=: );
    test Clocked_signal.( <=+ )
  ;;

  let%expect_test "concat_msb" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      ignore
        (Clocked_signal.concat_msb
           [ Clocked_signal.Unsafe.set_domain (Clocked_signal.input "foo" 4) ~dom
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "bar" 4) ~dom
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "baz" 4) ~dom
           ]
         : Clocked_signal.t)
    in
    let test_invalid () =
      ignore
        (Clocked_signal.concat_msb
           [ Clocked_signal.Unsafe.set_domain (Clocked_signal.input "foo" 4) ~dom:dom1
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "bar" 4) ~dom:dom2
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "baz" 4) ~dom:dom1
           ]
         : Clocked_signal.t)
    in
    let test ?(display_output = false) () =
      test_valid ();
      require_does_raise ~hide_positions:true test_invalid;
      if not display_output then ignore (expect_test_output () : string)
    in
    test ~display_output:true ();
    [%expect
      {|
      ("[concat] expects the clock domain of the operands to be compatible, but they're not:"
       ((concat0 (
          Exact (
            (uid  9)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (concat1 (
          Exact (
            (uid  10)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (concat2 (
          Exact (
            (uid  9)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "mux" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      ignore
        (Clocked_signal.mux
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 2) ~dom)
           [ Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case0" 8) ~dom
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case1" 8) ~dom
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case2" 8) ~dom
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case3" 8) ~dom
           ]
         : Clocked_signal.t)
    in
    test_valid ();
    (* The following below are all invalid cases. *)
    let test_invalid_sel () =
      ignore
        (Clocked_signal.mux
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 2) ~dom:dom2)
           [ Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case0" 8) ~dom:dom1
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case1" 8) ~dom:dom1
           ]
         : Clocked_signal.t)
    in
    let test_invalid_cases () =
      ignore
        (Clocked_signal.mux
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 2) ~dom:dom1)
           [ Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case0" 8) ~dom:dom1
           ; Clocked_signal.Unsafe.set_domain (Clocked_signal.input "case1" 8) ~dom:dom2
           ]
         : Clocked_signal.t)
    in
    let runtest ?(display_output = false) test_fn =
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    runtest ~display_output:true test_invalid_sel;
    [%expect
      {|
      ("[mux] expects the clock domain of the operands to be compatible, but they're not:"
       ((sel (
          Exact (
            (uid  12)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (case0 (
          Exact (
            (uid  11)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (case1 (
          Exact (
            (uid  11)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    runtest test_invalid_cases
  ;;

  let%expect_test "cases" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      ignore
        (Clocked_signal.cases
           ~default:
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "default" 8) ~dom)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 4) ~dom)
           [ ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 0)
                 ~dom
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val0" 8) ~dom )
           ; ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 1)
                 ~dom
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val1" 8) ~dom )
           ; ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 2)
                 ~dom
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val2" 8) ~dom )
           ]
         : Clocked_signal.t)
    in
    test_valid ();
    (* Invalid cases follow *)
    let test_invalid_default () =
      ignore
        (Clocked_signal.cases
           ~default:
             (Clocked_signal.Unsafe.set_domain
                (Clocked_signal.input "default" 8)
                ~dom:dom2)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 4) ~dom:dom1)
           [ ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 0)
                 ~dom:dom1
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val0" 8) ~dom:dom1
             )
           ]
         : Clocked_signal.t)
    in
    let test_invalid_sel () =
      ignore
        (Clocked_signal.cases
           ~default:
             (Clocked_signal.Unsafe.set_domain
                (Clocked_signal.input "default" 8)
                ~dom:dom1)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 4) ~dom:dom2)
           [ ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 0)
                 ~dom:dom1
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val0" 8) ~dom:dom1
             )
           ]
         : Clocked_signal.t)
    in
    let test_invalid_match () =
      ignore
        (Clocked_signal.cases
           ~default:
             (Clocked_signal.Unsafe.set_domain
                (Clocked_signal.input "default" 8)
                ~dom:dom1)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 4) ~dom:dom1)
           [ ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 0)
                 ~dom:dom2
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val0" 8) ~dom:dom1
             )
           ]
         : Clocked_signal.t)
    in
    let test_invalid_value () =
      ignore
        (Clocked_signal.cases
           ~default:
             (Clocked_signal.Unsafe.set_domain
                (Clocked_signal.input "default" 8)
                ~dom:dom1)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "sel" 4) ~dom:dom1)
           [ ( Clocked_signal.Unsafe.set_domain
                 (Clocked_signal.of_int_trunc ~width:4 0)
                 ~dom:dom1
             , Clocked_signal.Unsafe.set_domain (Clocked_signal.input "val0" 8) ~dom:dom2
             )
           ]
         : Clocked_signal.t)
    in
    let runtest ?(display_output = false) test_fn =
      test_valid ();
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    runtest ~display_output:true test_invalid_default;
    [%expect
      {|
      ("[cases] expects the clock domain of the operands to be compatible, but they're not:"
       ((default (
          Exact (
            (uid  14)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (sel (
          Exact (
            (uid  13)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (match0 (
          Exact (
            (uid  13)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (val0 (
          Exact (
            (uid  13)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    runtest test_invalid_sel;
    runtest test_invalid_match;
    runtest test_invalid_value
  ;;

  let%expect_test "reg" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg
           spec
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    test_valid ();
    (* Invalid cases follow *)
    let test_invalid_clock () =
      let clock =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom:dom1
      in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg
           spec
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom:dom2)
         : Clocked_signal.t)
    in
    let test_invalid_enable () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg
           ~enable:
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom:dom2)
           spec
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    let test_invalid_reset_to () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg
           ~reset_to:(Bits.ones 8)
           spec
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    let runtest ?(display_output = false) test_fn =
      test_valid ();
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    runtest ~display_output:true test_invalid_clock;
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  15)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (in (
          Exact (
            (uid  16)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    runtest test_invalid_enable;
    runtest test_invalid_reset_to
  ;;

  let%expect_test "reg_fb" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg_fb spec ~width:8 ~f:(fun fb ->
           Clocked_signal.( +: )
             fb
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "inc" 8) ~dom))
         : Clocked_signal.t)
    in
    let test_invalid_clock () =
      let clock =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom:dom1
      in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.reg_fb spec ~width:8 ~f:(fun fb ->
           Clocked_signal.( +: )
             fb
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "inc" 8) ~dom:dom2))
         : Clocked_signal.t)
    in
    test_valid ();
    require_does_raise ~hide_positions:true test_invalid_clock;
    [%expect
      {|
      ("+: expects the clock domain of the two operands to be compatible."
       (operands_with_mismatching_clock_domains (
         (lhs (
           Exact (
             (uid  17)
             (name foo_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
         (rhs (
           Exact (
             (uid  18)
             (name foo_gen)
             (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL)))))))
      |}]
  ;;

  let%expect_test "pipeline" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.pipeline
           spec
           ~n:3
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    test_valid ();
    (* Invalid cases follow *)
    let test_invalid_clock () =
      let clock =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom:dom1
      in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.pipeline
           spec
           ~n:3
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom:dom2)
         : Clocked_signal.t)
    in
    let test_invalid_enable () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.pipeline
           ~enable:
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom:dom2)
           spec
           ~n:3
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    let runtest ?(display_output = false) test_fn =
      test_valid ();
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    runtest ~display_output:true test_invalid_clock;
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  19)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (in (
          Exact (
            (uid  20)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    runtest test_invalid_enable
  ;;

  let%expect_test "cut_through_reg" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.cut_through_reg
           spec
           ~enable:(Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    let test_invalid_clock () =
      let clock =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom:dom1
      in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.cut_through_reg
           spec
           ~enable:
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom:dom1)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom:dom2)
         : Clocked_signal.t)
    in
    let test_invalid_enable () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      ignore
        (Clocked_signal.cut_through_reg
           spec
           ~enable:
             (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom:dom2)
           (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom)
         : Clocked_signal.t)
    in
    let runtest ?(display_output = false) test_fn =
      test_valid ();
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    runtest ~display_output:true test_invalid_clock;
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  21)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (in (
          Exact (
            (uid  22)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  21)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    runtest test_invalid_enable
  ;;

  let%expect_test "prev" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      let data = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom in
      let prev_fn = Clocked_signal.prev spec data in
      let prev_data = Staged.unstage prev_fn 2 in
      ignore (prev_data : Clocked_signal.t)
    in
    let test_invalid_clock () =
      let clock =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom:dom1
      in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      let data =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom:dom2
      in
      let prev_fn = Clocked_signal.prev spec data in
      let prev_data = Staged.unstage prev_fn 2 in
      ignore (prev_data : Clocked_signal.t)
    in
    let test_invalid_enable () =
      let dom = dom1 in
      let clock = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "clk" 1) ~dom in
      let spec = Clocked_signal.Reg_spec.create ~clock () in
      let data = Clocked_signal.Unsafe.set_domain (Clocked_signal.input "data" 8) ~dom in
      let prev_fn =
        Clocked_signal.prev
          ~enable:
            (Clocked_signal.Unsafe.set_domain (Clocked_signal.input "en" 1) ~dom:dom2)
          spec
          data
      in
      let prev_data = Staged.unstage prev_fn 2 in
      ignore (prev_data : Clocked_signal.t)
    in
    let test ?(display_output = false) test_fn =
      test_valid ();
      require_does_raise ~hide_positions:true test_fn;
      if not display_output then ignore (expect_test_output () : string)
    in
    test ~display_output:true test_invalid_clock;
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  23)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (in (
          Exact (
            (uid  24)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}];
    test test_invalid_enable
  ;;

  let%expect_test "multiport_memory - write port domain consistency" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom
        }
      in
      let read_addresses =
        [| Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom |]
      in
      ignore
        (Clocked_signal.multiport_memory 16 ~write_ports:[| write_port |] ~read_addresses
         : Clocked_signal.t array)
    in
    test_valid ();
    (* Invalid case: inconsistent signals within a write port *)
    let test_invalid_write_port_signals () =
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom:dom1
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom:dom2
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom:dom2
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom:dom2
        }
      in
      let read_addresses =
        [| Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom:dom2 |]
      in
      ignore
        (Clocked_signal.multiport_memory 16 ~write_ports:[| write_port |] ~read_addresses
         : Clocked_signal.t array)
    in
    test_valid ();
    require_does_raise ~hide_positions:true test_invalid_write_port_signals;
    [%expect
      {|
      ("[multiport_memory] expects the clock domain of the operands to be compatible, but they're not:"
       ((write_clock (
          Exact (
            (uid  25)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_address (
          Exact (
            (uid  26)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_enable (
          Exact (
            (uid  26)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_data (
          Exact (
            (uid  26)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "ram_rbw - read port domain consistency" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom
        }
      in
      let read_port =
        { Read_port.read_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "rclk" 1) ~dom
        ; read_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom
        ; read_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "ren" 1) ~dom
        }
      in
      ignore (Clocked_signal.ram_rbw ~write_port ~read_port 16 : Clocked_signal.t)
    in
    test_valid ();
    (* Invalid case: inconsistent read port signals *)
    let test_invalid_read_port_signals () =
      let dom = dom1 in
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom
        }
      in
      let read_port =
        { Read_port.read_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "rclk" 1) ~dom:dom2
        ; read_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom:dom2
        ; read_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "ren" 1) ~dom:dom2
        }
      in
      ignore (Clocked_signal.ram_rbw ~write_port ~read_port 16 : Clocked_signal.t)
    in
    test_valid ();
    require_does_raise ~hide_positions:true test_invalid_read_port_signals;
    [%expect
      {|
      ("[reg] expects the clock domain of the operands to be compatible, but they're not:"
       ((clock (
          Exact (
            (uid  28)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (in (
          Exact (
            (uid  27)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (enable (
          Exact (
            (uid  28)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}]
  ;;

  let%expect_test "memory - write port domain consistency" =
    let dom1 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let dom2 = Clock_domain.Runtime.Exact (Clock_domain.generate_fresh_exact_domain g) in
    let test_valid () =
      let dom = dom1 in
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom
        }
      in
      let read_address =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom
      in
      ignore (Clocked_signal.memory 16 ~write_port ~read_address : Clocked_signal.t)
    in
    test_valid ();
    (* Invalid case: inconsistent signals within write port *)
    let test_invalid_write_port_signals () =
      let write_port =
        { Write_port.write_clock =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wclk" 1) ~dom:dom1
        ; write_address =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "waddr" 4) ~dom:dom2
        ; write_enable =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wen" 1) ~dom:dom2
        ; write_data =
            Clocked_signal.Unsafe.set_domain (Clocked_signal.input "wdata" 8) ~dom:dom2
        }
      in
      let read_address =
        Clocked_signal.Unsafe.set_domain (Clocked_signal.input "raddr" 4) ~dom:dom2
      in
      ignore (Clocked_signal.memory 16 ~write_port ~read_address : Clocked_signal.t)
    in
    test_valid ();
    require_does_raise ~hide_positions:true test_invalid_write_port_signals;
    [%expect
      {|
      ("[multiport_memory] expects the clock domain of the operands to be compatible, but they're not:"
       ((write_clock (
          Exact (
            (uid  29)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_address (
          Exact (
            (uid  30)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_enable (
          Exact (
            (uid  30)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))
        (write_data (
          Exact (
            (uid  30)
            (name foo_gen)
            (loc lib/hardcaml/hardcaml/test/lib/test_clocked_signal.ml:LINE:COL))))))
      |}]
  ;;
end
