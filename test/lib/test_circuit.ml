open! Import
open! Signal

let show t = print_s [%sexp (t : Circuit.t)]

(* Signal.Types.reset_id () *)

let%expect_test "[sexp_of_t]" =
  show (Circuit.create_exn ~name:"name" []);
  [%expect
    {|
    ((name name)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs         ())
     (outputs        ())
     (phantom_inputs ())
     (signal_graph (
       (outputs ())
       (upto    ())))
     (assertions     ())
     (instantiations ()))
    |}]
;;

let%expect_test "[sexp_of_t] with an output" =
  show (Circuit.create_exn ~name:"name" [ wireof vdd -- "output" ]);
  [%expect
    {|
    ((name name)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs ())
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in 0b1))))
     (phantom_inputs ())
     (signal_graph (
       (outputs ((
         wire
         (names (output))
         (width   1)
         (data_in 0b1))))
       (upto ())))
     (assertions     ())
     (instantiations ()))
    |}]
;;

let%expect_test "[sexp_of_t] with an input" =
  let output = wire 1 in
  output <-- input "input" 1;
  show (Circuit.create_exn ~name:"name" [ output -- "output" ]);
  [%expect
    {|
    ((name name)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs ((wire (names (input)) (width 1))))
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in input))))
     (phantom_inputs ())
     (signal_graph (
       (outputs ((
         wire
         (names (output))
         (width   1)
         (data_in input))))
       (upto ())))
     (assertions     ())
     (instantiations ()))
    |}]
;;

let%expect_test "[sexp_of_t] with an operator" =
  let output = wire 1 in
  output <-- ~:(input "input" 1);
  show (Circuit.create_exn ~name:"name" [ output -- "output" ]);
  [%expect
    {|
    ((name name)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs ((wire (names (input)) (width 1))))
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in not))))
     (phantom_inputs ())
     (signal_graph (
       (outputs ((
         wire
         (names (output))
         (width   1)
         (data_in not))))
       (upto ())))
     (assertions     ())
     (instantiations ()))
    |}]
;;

let%expect_test "Output signal not driven" =
  require_does_raise (fun () -> Circuit.create_exn ~name:"test" [ wire 1 ]);
  [%expect {| ("circuit output signal is not driven" (output_signal (wire (width 1)))) |}]
;;

let%expect_test "Output signal with no name" =
  require_does_raise (fun () -> Circuit.create_exn ~name:"test" [ wireof vdd ]);
  [%expect
    {|
    ("circuit output signal must have a port name"
     (output_signal (
       wire
       (width   1)
       (data_in 0b1))))
    |}]
;;

let%expect_test "Output signal with multiple names" =
  require_does_raise (fun () ->
    Circuit.create_exn ~name:"test" [ wireof vdd -- "a" -- "b" ]);
  [%expect
    {|
    ("circuit output signal should only have one port name"
     (output_signal (
       wire
       (names (b a))
       (width   1)
       (data_in 0b1))))
    |}]
;;

let%expect_test "Output signal must be a wire" =
  require_does_raise (fun () -> Circuit.create_exn ~name:"test" [ gnd ]);
  [%expect
    {|
    ("circuit output signal must be a wire" (
      output_signal (
        const
        (names (gnd))
        (width 1)
        (value 0b0))))
    |}]
;;

let%expect_test "Port names must be unique" =
  require_does_raise (fun () ->
    Circuit.create_exn ~name:"test" [ output "a" (input "a" 1) ]);
  [%expect
    {| ("Port names are not unique" (circuit_name test) (input_and_output_names (a))) |}];
  require_does_raise (fun () ->
    Circuit.create_exn
      ~name:"test"
      [ output "a" (input "b" 1 +: input "b" 1 +: input "c" 1) ]);
  [%expect {| ("Input port names are not unique" (circuit_name test) (repeated (b))) |}];
  require_does_raise (fun () ->
    Circuit.create_exn ~name:"test" [ output "a" (input "b" 1); output "a" (input "c" 1) ]);
  [%expect {| ("Output port names are not unique" (circuit_name test) (repeated (a))) |}]
;;

(* This probably shouldn't, otherwise we would have to check all reserved identifiers for
   VHDL, Verilog and C (and other backends). *)
let%expect_test "Port name clashes with reserved name" =
  require_does_not_raise (fun () ->
    ignore (Circuit.create_exn ~name:"test" [ output "module" (input "x" 1) ] : Circuit.t));
  [%expect {| |}]
;;

let%expect_test "input with no name" =
  require_does_raise (fun () -> Circuit.create_exn ~name:"test" [ output "o" (wire 1) ]);
  [%expect
    {|
    ("circuit input signal must have a port name (unassigned wire?)"
     (input_signal (wire (width 1))))
    |}]
;;

let%expect_test "input with multiple names" =
  require_does_raise (fun () ->
    Circuit.create_exn ~name:"test" [ output "o" (wire 1 -- "a" -- "b") ]);
  [%expect
    {|
    ("circuit input signal should only have one port name"
     (input_signal (wire (names (b a)) (width 1))))
    |}]
;;

let%expect_test "phantom inputs" =
  let circuit = Circuit.create_exn ~name:"test" [ output "b" (input "a" 1) ] in
  (* Add a single phantom inputs *)
  Circuit.set_phantom_inputs circuit [ "c", 1 ] |> show;
  [%expect
    {|
    ((name test)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs ((wire (names (a)) (width 1))))
     (outputs ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (phantom_inputs ((c 1)))
     (signal_graph (
       (outputs ((
         wire
         (names (b))
         (width   1)
         (data_in a))))
       (upto ())))
     (assertions     ())
     (instantiations ()))
    |}];
  (* Add 2, one of which is already an input (and will be removed) *)
  Circuit.set_phantom_inputs circuit [ "a", 1; "c", 1 ] |> show;
  [%expect
    {|
    ((name test)
     (caller_id ())
     (config (
       (detect_combinational_loops true)
       (normalize_uids             true)
       (assertions ())
       (port_checks        Port_sets_and_widths)
       (add_phantom_inputs true)
       (modify_outputs     <opaque>)))
     (inputs ((wire (names (a)) (width 1))))
     (outputs ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (phantom_inputs ((c 1)))
     (signal_graph (
       (outputs ((
         wire
         (names (b))
         (width   1)
         (data_in a))))
       (upto ())))
     (assertions     ())
     (instantiations ()))
    |}];
  (* Add 2, one of which is already an output *)
  require_does_raise (fun () ->
    Circuit.set_phantom_inputs circuit [ "b", 1; "c", 1 ] |> show);
  [%expect
    {|
    ("Phantom input is also a circuit output"
      (phantom_inputs (
        (b 1)
        (c 1)))
      (outputs ((b 1)))
      (circuit (
        (name test)
        (input_ports  (a))
        (output_ports (b)))))
    |}]
;;

let%expect_test "phantom input aliases an internal name" =
  let f () =
    let a = input "a" 1 in
    let b = ~:a -- "b" in
    output "c" b
  in
  let circuit = Circuit.create_exn ~name:"test" [ f () ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module test (
        a,
        c
    );

        input a;
        output c;

        wire b;
        assign b = ~ a;
        assign c = b;

    endmodule
    |}];
  (* Note that the internal name [b] is now mangled correctly *)
  let circuit = Circuit.set_phantom_inputs circuit [ "b", 1 ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module test (
        a,
        b,
        c
    );

        input a;
        input b;
        output c;

        wire b_0;
        assign b_0 = ~ a;
        assign c = b_0;

    endmodule
    |}]
;;

let%expect_test "verify_clock_pins" =
  let circuit =
    let foo = input "foo" 1 in
    let clock = input "clock" 1 in
    let bar = reg (Reg_spec.create ~clock ()) ~enable:vdd foo in
    let bar = output "bar" bar in
    Circuit.create_exn ~name:"the_foo_bar" [ bar ]
  in
  require_does_not_raise (fun () ->
    Design_rule_checks.verify_clock_pins circuit ~expected_clock_pins:[ "clock" ]);
  require_does_raise (fun () ->
    Design_rule_checks.verify_clock_pins circuit ~expected_clock_pins:[ "bar" ]);
  [%expect
    {|
    ("The following sequential elements have unexpected clock pin connections"
     (signal_uid 1)
     (signals ((
       register
       (width 1)
       ((clock      clock)
        (clock_edge Rising))
       (data_in foo)))))
    |}];
  let circuit_with_wired_clock =
    let foo = input "foo" 1 in
    let clock = wireof (input "clock" 1) -- "clock_wire" in
    let bar = reg (Reg_spec.create ~clock ()) ~enable:vdd foo in
    let bar = output "bar" bar in
    Circuit.create_exn ~name:"the_foo_bar" [ bar ]
  in
  require_does_not_raise (fun () ->
    Design_rule_checks.verify_clock_pins
      circuit_with_wired_clock
      ~expected_clock_pins:[ "clock" ]);
  require_does_raise (fun () ->
    Design_rule_checks.verify_clock_pins
      circuit_with_wired_clock
      ~expected_clock_pins:[ "clock_wire" ]);
  [%expect
    {|
    ("The following sequential elements have unexpected clock pin connections"
     (signal_uid 1)
     (signals ((
       register
       (width 1)
       ((clock      clock_wire)
        (clock_edge Rising))
       (data_in foo)))))
    |}];
  let circuit_with_multiport_memory =
    let foo = input "address" 7 in
    let clock0 = input "clock0" 1 in
    let clock1 = input "clock1" 1 in
    let clock2 = input "clock2" 1 in
    let clock3 = input "clock3" 1 in
    let ram_outputs =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:128
        ~write_ports:
          [| { write_enable = vdd
             ; write_address = foo
             ; write_data = zero 8
             ; write_clock = clock0
             }
           ; { write_enable = vdd
             ; write_address = foo
             ; write_data = zero 8
             ; write_clock = clock1
             }
          |]
        ~read_ports:
          [| { read_enable = vdd; read_address = foo; read_clock = clock2 }
           ; { read_enable = vdd; read_address = foo; read_clock = clock3 }
          |]
        ()
    in
    Circuit.create_exn
      ~name:"name"
      (List.mapi
         ~f:(fun i s -> output (Printf.sprintf "ram_%d" i) s)
         (Array.to_list ram_outputs))
  in
  require_does_not_raise (fun () ->
    Design_rule_checks.verify_clock_pins
      circuit_with_multiport_memory
      ~expected_clock_pins:[ "clock0"; "clock1"; "clock2"; "clock3" ]);
  require_does_raise (fun () ->
    Design_rule_checks.verify_clock_pins
      circuit_with_multiport_memory
      ~expected_clock_pins:[ "clock0"; "clock1"; "clock2"; "clock5" ]);
  [%expect
    {|
    ("The following sequential elements have unexpected clock pin connections"
     (signal_uid 1)
     (signals ((
       register
       (width 8)
       ((clock      clock3)
        (clock_edge Rising))
       (data_in memory_read_port)))))
    |}]
;;

let%expect_test "Raises when encounters duplicated ports in interfaces." =
  let module I = struct
    type 'a t =
      { foo : 'a [@rtlname "hello"]
      ; bar : 'a [@rtlname "hello"]
      }
    [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { baz : 'a } [@@deriving hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  Expect_test_helpers_base.require_does_raise (fun () ->
    C.create_exn ~name:"circuit" (fun (input : _ I.t) ->
      { O.baz = input.foo +: uresize input.bar ~width:30 }));
  [%expect
    {| ("Input port names are not unique" (circuit_name circuit) (repeated (hello))) |}]
;;
