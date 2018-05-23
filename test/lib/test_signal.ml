open! Import

open Signal

let%expect_test "[Reg_spec.sexp_of_t]" =
  print_s [%sexp (Reg_spec.create () ~clk:clock : Reg_spec.t)];
  [%expect {|
    ((clock      clock)
     (clock_edge rising)) |}];
;;

let%expect_test "[Ram_spec.sexp_of_t]" =
  print_s [%sexp (Ram_spec.create () ~clk:clock : Ram_spec.t)];
  [%expect {|
    ((clock      clock)
     (clock_edge rising)) |}];
;;

let%expect_test "name of empty" =
  require_does_raise [%here] (fun () ->
    Signal.names empty);
  [%expect {| "cannot get [names] from the empty signal" |}]

let%expect_test "non-const signal" =
  require_does_raise [%here] (fun () ->
    Signal.const_value (wire 1));
  [%expect {|
    ("cannot get the value of a non-constant signal"
     (wire
       (width   1)
       (data_in empty))) |}]

let%expect_test "non-const to_int" =
  require_does_raise [%here] (fun () ->
    to_int (wire 1));
  [%expect {|
    ("cannot use [to_int] on non-constant signal"
     (wire
       (width   1)
       (data_in empty))) |}]

let%expect_test "non-const to_bstr" =
  require_does_raise [%here] (fun () ->
    to_bstr (wire 1));
  [%expect {|
    ("cannot use [to_bstr] on non-constant signal"
     (wire
       (width   1)
       (data_in empty))) |}]

let%expect_test "set name of empty" =
  require_does_raise [%here] (fun () ->
    empty -- "oops");
  [%expect {|
    ("attempt to set the name of the empty signal" (to_ oops)) |}]

let%expect_test "multiple assignment to a wire" =
  require_does_raise [%here] (fun () ->
    let w = wire 1 in
    w <== vdd;
    w <== gnd);
  [%expect {|
    ("attempt to assign wire multiple times"
      (already_assigned_wire (
        wire
        (width   1)
        (data_in 0b1)))
      (expression (
        const
        (names (gnd))
        (width 1)
        (value 0b0)))) |}]

let%expect_test "wire width mismatch" =
  require_does_raise [%here] (fun () ->
    let w = wire 29 in
    w <== consti 17 3);
  [%expect {|
    ("attempt to assign expression to wire of different width"
     (wire_width       29)
     (expression_width 17)
     (wire       (wire  (width 29) (data_in empty)))
     (expression (const (width 17) (value   0x00003)))) |}]

let%expect_test "assignment to a non-wire" =
  require_does_raise [%here] (fun () ->
    let w = vdd in
    w <== gnd);
  [%expect {|
    ("attempt to assign non-wire"
      (assignment_target (
        const
        (names (vdd))
        (width 1)
        (value 0b1)))
      (expression (
        const
        (names (gnd))
        (width 1)
        (value 0b0)))) |}]

let reg_error ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?e here =
  require_does_raise here (fun () ->
    reg (Reg_spec.override (Reg_spec.create () ~clk:(Option.value clk ~default:clock))
           ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv)
      ~e:(Option.value e ~default:enable)
      (input "d" 8));
;;

let%expect_test "invalid clock" =
  reg_error [%here] ~clk:(input "not_a_clock" 2);
  [%expect {|
    ("clock is invalid"
      (info           "signal has unexpected width")
      (expected_width 1)
      (signal (
        wire
        (names (not_a_clock))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid clock_level" =
  reg_error [%here] ~clkl:(input "not_a_clock_level" 2);
  [%expect {|
    ("clock level is invalid"
      (info "signal should be [vdd], [gnd] or [empty].")
      (signal (
        wire
        (names (not_a_clock_level))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid reset" =
  reg_error [%here] ~r:(input "not_a_reset" 2);
  [%expect {|
    ("reset is invalid"
      (info "signal should have expected width or be empty")
      (expected_width 1)
      (signal (
        wire
        (names (not_a_reset))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid reset_level" =
  reg_error [%here] ~rl:(input "not_a_reset_level" 2);
  [%expect {|
    ("reset level is invalid"
      (info "signal should be [vdd], [gnd] or [empty].")
      (signal (
        wire
        (names (not_a_reset_level))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid reset_value" =
  reg_error [%here] ~rv:(input "not_a_reset_value" 2);
  [%expect {|
    ("reset value is invalid"
      (info "signal should have expected width or be empty")
      (expected_width 8)
      (signal (
        wire
        (names (not_a_reset_value))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid clear" =
  reg_error [%here] ~c:(input "not_a_clear" 2);
  [%expect {|
    ("clear signal is invalid"
      (info "signal should have expected width or be empty")
      (expected_width 1)
      (signal (
        wire
        (names (not_a_clear))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid clear_level" =
  reg_error [%here] ~cl:(input "not_a_clear_level" 2);
  [%expect {|
    ("clear level is invalid"
      (info "signal should be [vdd], [gnd] or [empty].")
      (signal (
        wire
        (names (not_a_clear_level))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid clear_value" =
  reg_error [%here] ~cv:(input "not_a_clear_value" 2);
  [%expect {|
    ("clear value is invalid"
      (info "signal should have expected width or be empty")
      (expected_width 8)
      (signal (
        wire
        (names (not_a_clear_value))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "invalid enable" =
  reg_error [%here] ~e:(input "not_an_enable" 2);
  [%expect {|
    ("enable is invalid"
      (info "signal should have expected width or be empty")
      (expected_width 1)
      (signal (
        wire
        (names (not_an_enable))
        (width   2)
        (data_in empty)))) |}]

let%expect_test "insertion" =
  require_does_raise [%here] (fun () -> insert ~t:(constb "111") ~f:(constb "00") (-1));
  [%expect {| ("[insert] below bit 0" -1) |}];
  require_does_raise [%here] (fun () -> insert ~t:(constb "111") ~f:(constb "00") 2);
  [%expect {|
    ("[insert] above msb of target"
      (width_from           2)
      (width_target         3)
      (at_pos               2)
      (highest_inserted_bit 4)) |}];
  require_does_not_raise [%here] (fun () ->
    print_s [%message "valid [insert]"
                        ~_:(insert ~t:(constb "111") ~f:(constb "00") 1 : t)]);
  [%expect {|
    ("valid [insert]" (cat (width 3) (arguments (0b00 select)))) |}]
;;

let%expect_test "mux errors" =
  require_does_raise [%here] (fun () -> mux vdd [ gnd; constb "10" ]);
  [%expect {|
    ("[mux] got inputs of different widths" (
      (const
        (names (gnd))
        (width 1)
        (value 0b0))
      (const
        (width 2)
        (value 0b10)))) |}];
  require_does_raise [%here] (fun () -> mux vdd [ gnd; vdd; gnd ]);
  [%expect {|
    ("[mux] got too many inputs"
      (inputs_provided  3)
      (maximum_expected 2)) |}];
  require_does_raise [%here] (fun () -> mux vdd [ gnd ]);
  [%expect {| ("[mux] got fewer than 2 inputs" (inputs_provided 1)) |}];
  require_does_raise [%here] (fun () -> mux2 (constb "11") gnd vdd);
  [%expect {| "[mux] got select argument that is not one bit" |}]
;;

let%expect_test "shift errors" =
  require_does_raise [%here] (fun () -> sll vdd (-1));
  [%expect {| ("[sll] got negative shift" -1) |}];
  require_does_raise [%here] (fun () -> srl vdd (-1));
  [%expect {| ("[srl] got positive shift" -1) |}];
  require_does_raise [%here] (fun () -> sra vdd (-1));
  [%expect {| ("[sra] got negative shift" -1) |}]
;;

let%expect_test "tree errors" =
  require_does_raise [%here] (fun () -> tree 2 (reduce (+:)) []);
  [%expect {| "[tree] got empty list" |}];
  require_does_raise [%here] (fun () -> tree 1 (reduce (+:)) []);
  [%expect {| "[tree] got [arity <= 1]" |}]
;;
