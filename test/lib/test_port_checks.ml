open! Import

module I = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { x : 'a } [@@deriving sexp_of, hardcaml]
end

(* Check input sets.

   Note; although outputs are also checked, there isn't an obvious way to trigger the
   exception.  An empty output, for example, will cause an earlier exception in
   [Circuit]. *)

let%expect_test "too many inputs" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = Signal.(i.a |: i.b |: input "c" 1) } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore (Circuit.create_exn ~port_checks ~name:"foo" create : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {||}];
  require_does_raise [%here] (fun () -> circuit Port_sets);
  [%expect
    {|
    ("Port sets do not match"
      (direction input)
      (expected_ports (a b))
      (actual_ports (a b c))
      (expected_but_not_in_circuit ())
      (in_circuit_but_not_expected (c))
      (circuit ((name foo) (input_ports (c b a)) (output_ports (x))))) |}]
;;

let%expect_test "too few inputs" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = i.a } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore
      (Circuit.create_exn ~port_checks ~add_phantom_inputs:false ~name:"foo" create
       : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {||}];
  require_does_raise [%here] (fun () -> circuit Port_sets);
  [%expect
    {|
    ("Port sets do not match"
      (direction input)
      (expected_ports (a b))
      (actual_ports                (a))
      (expected_but_not_in_circuit (b))
      (in_circuit_but_not_expected ())
      (circuit (
        (name foo)
        (input_ports  (a))
        (output_ports (x))))) |}]
;;

(* Check output port widths.

   As inputs are created by the framework, there isn't an obvious way to make them the
   wrong size. *)

let%expect_test "output width does not match" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = Signal.(i.a |: i.b |> ue) } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore (Circuit.create_exn ~port_checks ~name:"foo" create : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {||}];
  circuit Port_sets;
  [%expect {||}];
  require_does_raise [%here] (fun () -> circuit Port_sets_and_widths);
  [%expect
    {|
    ("Output port widths do not match"
      (expected ((x 1)))
      (got      ((x 2)))
      (circuit ((name foo) (input_ports (b a)) (output_ports (x))))) |}]
;;
