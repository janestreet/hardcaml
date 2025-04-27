open! Import

module I = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { x : 'a } [@@deriving hardcaml]
end

(* Check input sets.

   Note; although outputs are also checked, there isn't an obvious way to trigger the
   exception.  An empty output, for example, will cause an earlier exception in
   [Circuit]. *)

let%expect_test "too many inputs" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = Signal.(i.a |: i.b |: input "c" 1) } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore
      (Circuit.create_exn
         ~config:{ Hardcaml.Circuit.Config.default with port_checks }
         ~name:"foo"
         create
       : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {| |}];
  require_does_raise (fun () -> circuit Port_sets);
  [%expect
    {|
    ("Port sets do not match"
      (direction input)
      (expected_ports (a b))
      (actual_ports (a b c))
      (expected_but_not_in_circuit ())
      (in_circuit_but_not_expected (c))
      (circuit ((name foo) (input_ports (c b a)) (output_ports (x)))))
    |}]
;;

let%expect_test "too few inputs" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = i.a } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore
      (Circuit.create_exn
         ~config:
           { Hardcaml.Circuit.Config.default with
             port_checks
           ; add_phantom_inputs = false
           }
         ~name:"foo"
         create
       : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {| |}];
  require_does_raise (fun () -> circuit Port_sets);
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
        (output_ports (x)))))
    |}]
;;

(* Check output port widths.

   As inputs are created by the framework, there isn't an obvious way to make them the
   wrong size. *)

let%expect_test "output width does not match" =
  let module Circuit = Circuit.With_interface (I) (O) in
  let create (i : _ I.t) = { O.x = Signal.(i.a |: i.b |> ue) } in
  let circuit (port_checks : Hardcaml.Circuit.Port_checks.t) =
    ignore
      (Circuit.create_exn
         ~config:{ Hardcaml.Circuit.Config.default with port_checks }
         ~name:"foo"
         create
       : Hardcaml.Circuit.t)
  in
  circuit Relaxed;
  [%expect {| |}];
  circuit Port_sets;
  [%expect {| |}];
  require_does_raise (fun () -> circuit Port_sets_and_widths);
  [%expect
    {|
    ("Port width of "
      (name       x)
      (port_width 2)
      " was specified as "
      (expected_width 1)
      " in interface")
    |}]
;;

module%test Wrong_length_list = struct
  module O = struct
    type 'a t = { l : 'a list [@length 2] } [@@deriving hardcaml]
  end

  let create _ = { O.l = List.init 3 ~f:(fun _ -> Signal.vdd) }

  let%expect_test "Raises if using a list of wrong length" =
    let module Circuit = Circuit.With_interface (I) (O) in
    require_does_raise (fun () -> Circuit.create_exn ~name:"foo" create);
    [%expect
      {|
      ("Number of ports in output interface"
       2
       "does not match number of provided output signals!"
       3
       "(are you using a wrong length list or array in the interface?)"
       (port_names (l0 l1)))
      |}]
  ;;
end
