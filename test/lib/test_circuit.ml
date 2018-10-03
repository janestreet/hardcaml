open! Import
open! Signal

let show t =
  print_s [%sexp (t : Circuit.t)];
  (* Signal.Types.reset_id () *)
;;

let%expect_test "[sexp_of_t]" =
  show (Circuit.create_exn ~name:"name" []);
  [%expect {|
    ((name name)
     (signal_by_uid  ())
     (inputs         ())
     (outputs        ())
     (phantom_inputs ())
     (signal_graph   ())
     (fan_out        ())
     (fan_in         ())) |}]
;;

let%expect_test "[sexp_of_t] with an output" =
  show (Circuit.create_exn ~name:"name" [ wireof vdd -- "output" ]);
  [%expect {|
    ((name name)
     (signal_by_uid (
       (1 (
         wire
         (names (output))
         (width   1)
         (data_in 0b1)))
       (2 (
         const
         (names (vdd))
         (width 1)
         (value 0b1)))))
     (inputs ())
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in 0b1))))
     (phantom_inputs ())
     (signal_graph ((
       wire
       (names (output))
       (width   1)
       (data_in 0b1))))
     (fan_out ((2 (1))))
     (fan_in ((1 (2)) (2 ())))) |}]
;;

let%expect_test "[sexp_of_t] with an input" =
  let output = wire 1 in
  output <== input "input" 1;
  show (Circuit.create_exn ~name:"name" [ output -- "output" ]);
  [%expect {|
    ((name name)
     (signal_by_uid (
       (0 empty)
       (1 (
         wire
         (names (input))
         (width   1)
         (data_in empty)))
       (2 (
         wire
         (names (output))
         (width   1)
         (data_in input)))))
     (inputs ((
       wire
       (names (input))
       (width   1)
       (data_in empty))))
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in input))))
     (phantom_inputs ())
     (signal_graph ((
       wire
       (names (output))
       (width   1)
       (data_in input))))
     (fan_out (
       (0 (1))
       (1 (2))))
     (fan_in (
       (0 ())
       (1 (0))
       (2 (1))))) |}]
;;

let%expect_test "[sexp_of_t] with an operator" =
  let output = wire 1 in
  output <== ~: (input "input" 1);
  show (Circuit.create_exn ~name:"name" [ output -- "output" ]);
  [%expect {|
    ((name name)
     (signal_by_uid (
       (0 empty)
       (1 (
         wire
         (names (input))
         (width   1)
         (data_in empty)))
       (2 (
         wire
         (names (output))
         (width   1)
         (data_in not)))
       (3 (not (width 1) (arguments (input))))))
     (inputs ((
       wire
       (names (input))
       (width   1)
       (data_in empty))))
     (outputs ((
       wire
       (names (output))
       (width   1)
       (data_in not))))
     (phantom_inputs ())
     (signal_graph ((
       wire
       (names (output))
       (width   1)
       (data_in not))))
     (fan_out (
       (0 (1))
       (1 (3))
       (3 (2))))
     (fan_in (
       (0 ())
       (1 (0))
       (2 (3))
       (3 (1))))) |}];
;;

let%expect_test "Output signal not driven" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ wire 1 ]);
  [%expect {|
    ("circuit output signal is not driven" (
      output_signal (
        wire
        (width   1)
        (data_in empty)))) |}]

let%expect_test "Output signal with no name" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ wireof vdd ]);
  [%expect {|
    ("circuit output signal must have a port name"
     (output_signal (
       wire
       (width   1)
       (data_in 0b1)))) |}]

let%expect_test "Output signal with multiple names" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ wireof vdd -- "a" -- "b" ]);
  [%expect {|
    ("circuit output signal should only have one port name"
     (output_signal (
       wire
       (names (b a))
       (width   1)
       (data_in 0b1)))) |}]

let%expect_test "Output signal must be a wire" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ gnd ]);
  [%expect {|
    ("circuit output signal must be a wire" (
      output_signal (
        const
        (names (gnd))
        (width 1)
        (value 0b0)))) |}]

let%expect_test "Port names must be unique" =
  require_does_raise [%here] ~cr:CR_someday (fun () ->
    Circuit.create_exn ~name:"test" [ output "a" (input "a" 1) ]);
  [%expect {|
    "did not raise" |}]

(* This probably shouldn't, otherwise we would have to check all reserved identifiers for
   VHDL, Verilog and C (and other backends). *)
let%expect_test "Port name clashes with reserved name" =
  require_does_not_raise [%here] (fun () ->
    ignore (Circuit.create_exn ~name:"test" [ output "module" (input "x" 1) ] : Circuit.t));
  [%expect {| |}]

let%expect_test "input with no name" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ output "o" (wire 1) ]);
  [%expect {|
    ("circuit input signal must have a port name (unassigned wire?)"
     (input_signal (
       wire
       (width   1)
       (data_in empty)))) |}]

let%expect_test "input with multiple names" =
  require_does_raise [%here] (fun () ->
    Circuit.create_exn ~name:"test" [ output "o" (wire 1 -- "a" -- "b") ]);
  [%expect {|
    ("circuit input signal should only have one port name"
     (input_signal (
       wire
       (names (b a))
       (width   1)
       (data_in empty)))) |}]

let%expect_test "phantom inputs" =
  let circuit = Circuit.create_exn ~name:"test" [ output "b" (input "a" 1)] in
  (* Add a single phantom inputs *)
  Circuit.set_phantom_inputs circuit [ "c", 1] |> show;
  [%expect {|
    ((name test)
     (signal_by_uid (
       (0 empty)
       (1 (
         wire
         (names (a))
         (width   1)
         (data_in empty)))
       (2 (
         wire
         (names (b))
         (width   1)
         (data_in a)))))
     (inputs ((
       wire
       (names (a))
       (width   1)
       (data_in empty))))
     (outputs ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (phantom_inputs ((c 1)))
     (signal_graph ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (fan_out (
       (0 (1))
       (1 (2))))
     (fan_in (
       (0 ())
       (1 (0))
       (2 (1))))) |}];
  (* Add 2, one of which is already an input (and will be removed) *)
  Circuit.set_phantom_inputs circuit [ "a", 1; "c", 1] |> show;
  [%expect {|
    ((name test)
     (signal_by_uid (
       (0 empty)
       (1 (
         wire
         (names (a))
         (width   1)
         (data_in empty)))
       (2 (
         wire
         (names (b))
         (width   1)
         (data_in a)))))
     (inputs ((
       wire
       (names (a))
       (width   1)
       (data_in empty))))
     (outputs ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (phantom_inputs ((c 1)))
     (signal_graph ((
       wire
       (names (b))
       (width   1)
       (data_in a))))
     (fan_out (
       (0 (1))
       (1 (2))))
     (fan_in (
       (0 ())
       (1 (0))
       (2 (1))))) |}];
  (* Add 2, one of which is already an output *)
  require_does_raise [%here] (fun () -> Circuit.set_phantom_inputs circuit [ "b", 1; "c", 1] |> show);
  [%expect {|
    ("Phantom input is also a circuit output"
      (phantom_inputs (
        (b 1)
        (c 1)))
      (outputs ((b 1)))) |}]
;;
