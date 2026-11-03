open! Core
open Expect_test_helpers_base
open Hardcaml

let%expect_test "encoding of 0, 1, X and Z" =
  List.map [ "0"; "1"; "X"; "Z"; "01XZ"; "1Z0X1" ] ~f:(fun value ->
    let t = Four_state_logic.of_string value in
    [%sexp
      { value = (t : Four_state_logic.t)
      ; bits = (Four_state_logic.bits t : Bits.t)
      ; mask = (Four_state_logic.mask t : Bits.t)
      ; width = (Four_state_logic.width t : int)
      }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬───────┬───────┬───────┐
    │ value │ bits  │ mask  │ width │
    ├───────┼───────┼───────┼───────┤
    │ 0     │     0 │     0 │ 1     │
    │ 1     │     1 │     0 │ 1     │
    │ X     │     1 │     1 │ 1     │
    │ Z     │     0 │     1 │ 1     │
    │ 01XZ  │  0110 │  0011 │ 4     │
    │ 1Z0X1 │ 10011 │ 01010 │ 5     │
    └───────┴───────┴───────┴───────┘
    |}];
  (* The planes are the only state; a value round trips through them. *)
  let t = Four_state_logic.of_string "01XZ" in
  let round_tripped =
    Four_state_logic.create_exn
      ~bits:(Four_state_logic.bits t)
      ~mask:(Four_state_logic.mask t)
  in
  require (Four_state_logic.equal t round_tripped);
  (* Values differing only in the mask are different values. *)
  require
    (not
       (Four_state_logic.equal
          (Four_state_logic.of_string "10")
          (Four_state_logic.of_string "X0")));
  [%expect {| |}]
;;

let%expect_test "invalid character" =
  require_does_raise (fun () -> Four_state_logic.of_string "01?");
  [%expect {| ("[Std_logic.of_char_exn] got invalid char" (char ?)) |}]
;;

let%expect_test "planes must have the same width" =
  let bits = Bits.of_string "0101" in
  let mask = Bits.of_string "01" in
  require_does_raise (fun () -> Four_state_logic.create_exn ~bits ~mask);
  [%expect
    {|
    ("[Four_state_logic.create] planes have different widths"
     (bits_width 4)
     (mask_width 2))
    |}]
;;

let%expect_test "impurity; X wins over Z" =
  List.map [ "0"; "1"; "X"; "Z"; "0110"; "01Z0"; "01X0"; "0XZ0" ] ~f:(fun value ->
    [%sexp
      { value : string
      ; impurity =
          (Four_state_logic.impurity (Four_state_logic.of_string value)
           : Four_state_logic.Impurity.t)
      }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬──────────┐
    │ value │ impurity │
    ├───────┼──────────┤
    │ 0     │ Pure     │
    │ 1     │ Pure     │
    │ X     │ X        │
    │ Z     │ Z        │
    │ 0110  │ Pure     │
    │ 01Z0  │ Z        │
    │ 01X0  │ X        │
    │ 0XZ0  │ X        │
    └───────┴──────────┘
    |}]
;;

let%expect_test "select" =
  let t = Four_state_logic.of_string "01XZ10" in
  let select ~high ~low =
    print_s [%sexp (Four_state_logic.select t ~high ~low : Four_state_logic.t)]
  in
  select ~high:5 ~low:0;
  [%expect {| 01XZ10 |}];
  select ~high:3 ~low:2;
  [%expect {| XZ |}];
  select ~high:1 ~low:1;
  [%expect {| 1 |}]
;;

let%expect_test "plane characters of each 4-state bit" =
  List.map Logic.Four_state.all ~f:(fun logic ->
    let bits, mask = Four_state_logic.plane_chars logic in
    let plane char = Bits.of_string (String.of_char char) in
    (* Decoding the planes again shows [to_string] agrees with [plane_chars]. *)
    let value = Four_state_logic.create_exn ~bits:(plane bits) ~mask:(plane mask) in
    [%sexp
      { logic : Logic.Four_state.t; bits : char; mask : char; value : Four_state_logic.t }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬──────┬──────┬───────┐
    │ logic │ bits │ mask │ value │
    ├───────┼──────┼──────┼───────┤
    │ x     │ 1    │ 1    │ X     │
    │ z     │ 0    │ 1    │ Z     │
    │ 0     │ 0    │ 0    │ 0     │
    │ 1     │ 1    │ 0    │ 1     │
    └───────┴──────┴──────┴───────┘
    |}]
;;

let%expect_test "of_string" =
  (* 2 state - using the fancy verilog style mode *)
  print_s [%message (Four_state_logic.of_string "10'd4" : Four_state_logic.t)];
  [%expect {| ("Four_state_logic.of_string \"10'd4\"" 0000000100) |}];
  (* 4 state *)
  print_s [%message (Four_state_logic.of_string "XZ10" : Four_state_logic.t)];
  [%expect {| ("Four_state_logic.of_string \"XZ10\"" XZ10) |}];
  (* round trip *)
  print_s
    [%message (Four_state_logic.to_string (Four_state_logic.of_string "zX") : string)];
  [%expect {| ("Four_state_logic.to_string (Four_state_logic.of_string \"zX\")" ZX) |}];
  print_s
    [%message (Four_state_logic.to_string (Four_state_logic.don't_care 63) : string)];
  [%expect
    {|
    ("Four_state_logic.to_string (Four_state_logic.don't_care 63)"
     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX)
    |}]
;;

(* The truth tables below pin down the 4-state semantics.

   - Z is a driver state, not a logic level. Any computing operation - gates, arithmetic,
     comparison, or a [mux] with an impure select - turns it into X, which is what both
     Verilog and VHDL simulators do. Only structural operations ([select], [concat], and a
     [mux] with a pure select) pass Z through.

   - X propagation depends on the instance. In [Pessimistic] (the default exported by
     [Four_state_logic]) an impure input always produces X, even when a pure input
     determines the result. In [Optimistic] a controlling pure input decides -
     [0 & X = 0], [1 | X = 1], and [==:] returns 0 when pure bits definitely mismatch -
     which is the optimism Verilog and VHDL simulators implement for gates.

   - Both instances satisfy the same contract, checked exhaustively at the bottom of this
     file: a pure output bit holds under every resolution of the impure input bits. The
     simulators themselves do not (for example an impure select silently picks an arm of a
     Verilog case statement); the cosim reference tables in [test_four_state_reference.ml]
     pin down what they do instead. *)

let all_1_bit_values = Four_state_logic.[ gnd; vdd; don't_care 1; high_impedance 1 ]

let truthtable f =
  let row r =
    List.iter r ~f:(fun r -> printf "%s " (Four_state_logic.to_string r));
    printf "\n"
  in
  printf "  ";
  row all_1_bit_values;
  List.iter all_1_bit_values ~f:(fun a ->
    printf "%s " (Four_state_logic.to_string a);
    List.iter all_1_bit_values ~f:(fun b ->
      printf "%s " (Four_state_logic.to_string (f a b)));
    printf "\n")
;;

let%expect_test "truth tables" =
  let open Four_state_logic in
  truthtable ( |: );
  [%expect
    {|
      0 1 X Z
    0 0 1 X X
    1 1 1 X X
    X X X X X
    Z X X X X
    |}];
  truthtable ( &: );
  [%expect
    {|
      0 1 X Z
    0 0 0 X X
    1 0 1 X X
    X X X X X
    Z X X X X
    |}];
  truthtable ( ^: );
  [%expect
    {|
      0 1 X Z
    0 0 1 X X
    1 1 0 X X
    X X X X X
    Z X X X X
    |}];
  truthtable (fun _ a -> ( ~: ) a);
  [%expect
    {|
      0 1 X Z
    0 1 0 X X
    1 1 0 X X
    X 1 0 X X
    Z 1 0 X X
    |}];
  (* Verilog [==] additionally returns [0] when pure bits definitely mismatch (for example
     [0X == 1X] is [0], not [x]) - that is what the [Optimistic] instance does. *)
  truthtable ( ==: );
  [%expect
    {|
      0 1 X Z
    0 1 0 X X
    1 0 1 X X
    X X X X X
    Z X X X X
    |}]
;;

let%expect_test "mux" =
  let open Four_state_logic in
  (* A pure select indexes the case list and returns the selected case unchanged,
     preserving X and Z in the data (as Verilog's [s ? b : a] does when [s] is known). An
     impure select produces all X in both instances - Verilog optimistically returns [a]
     when both cases agree ([X ? a : a = a]) where we give X. *)
  printf "a b | s=0 s=1 s=X s=Z\n";
  List.iter all_1_bit_values ~f:(fun a ->
    List.iter all_1_bit_values ~f:(fun b ->
      printf "%s %s |" (to_string a) (to_string b);
      List.iter all_1_bit_values ~f:(fun s -> printf "  %s " (to_string (mux s [ a; b ])));
      printf "\n"));
  [%expect
    {|
    a b | s=0 s=1 s=X s=Z
    0 0 |  0   0   X   X
    0 1 |  0   1   X   X
    0 X |  0   X   X   X
    0 Z |  0   Z   X   X
    1 0 |  1   0   X   X
    1 1 |  1   1   X   X
    1 X |  1   X   X   X
    1 Z |  1   Z   X   X
    X 0 |  X   0   X   X
    X 1 |  X   1   X   X
    X X |  X   X   X   X
    X Z |  X   Z   X   X
    Z 0 |  Z   0   X   X
    Z 1 |  Z   1   X   X
    Z X |  Z   X   X   X
    Z Z |  Z   Z   X   X
    |}];
  (* An out of range pure select clamps to the last case, which also passes impure data
     straight through. *)
  print_s
    [%sexp
      (mux (of_string "11") [ of_string "00"; of_string "01"; of_string "1Z" ]
       : Four_state_logic.t)];
  [%expect {| 1Z |}]
;;

let%expect_test "addition" =
  let open Four_state_logic in
  (* Any impure input bit X-fills the whole result, which is what both Verilog and VHDL
     simulators do. Both instances share this behaviour. *)
  List.iter [ "0000"; "1111"; "X000"; "000X"; "Z000"; "000Z" ] ~f:(fun a ->
    let a = of_string a in
    let b = of_string "0001" in
    printf
      !"%{Four_state_logic} + %{Four_state_logic} = %{Four_state_logic}\n"
      a
      b
      (a +: b));
  [%expect
    {|
    0000 + 0001 = 0001
    1111 + 0001 = 0000
    X000 + 0001 = XXXX
    000X + 0001 = XXXX
    Z000 + 0001 = XXXX
    000Z + 0001 = XXXX
    |}]
;;

let%expect_test "structural operations pass X and Z through" =
  let open Four_state_logic in
  let t = of_string "01XZ" in
  print_s [%sexp (concat_msb [ t; select t ~high:1 ~low:0 ] : Four_state_logic.t)];
  [%expect {| 01XZXZ |}]
;;

let%expect_test "optimistic truth tables" =
  let open Four_state_logic.Optimistic in
  (* A controlling pure input decides even when the other input is impure. *)
  truthtable ( |: );
  [%expect
    {|
      0 1 X Z
    0 0 1 X X
    1 1 1 1 1
    X X 1 X X
    Z X 1 X X
    |}];
  truthtable ( &: );
  [%expect
    {|
      0 1 X Z
    0 0 0 0 0
    1 0 1 X X
    X 0 X X X
    Z 0 X X X
    |}];
  (* [xor] has no controlling value, so it matches the pessimistic table. *)
  truthtable ( ^: );
  [%expect
    {|
      0 1 X Z
    0 0 1 X X
    1 1 0 X X
    X X X X X
    Z X X X X
    |}];
  (* On 1 bit [==:] also matches the pessimistic table: a definite mismatch needs both
     bits pure, and then nothing is impure. On wider values a pure-bit mismatch decides
     the comparison even when other bits are impure. *)
  List.iter
    [ "0X00", "1X00"; "X000", "X000"; "01XZ", "0101" ]
    ~f:(fun (a, b) ->
      printf !"%s == %s = %{Four_state_logic}\n" a b (of_string a ==: of_string b));
  [%expect
    {|
    0X00 == 1X00 = 0
    X000 == X000 = X
    01XZ == 0101 = X
    |}];
  (* The remaining operations - arithmetic, comparisons and [mux] - are shared with
     [Pessimistic]. *)
  print_s [%sexp (of_string "X000" +: of_string "0001" : Four_state_logic.t)];
  [%expect {| XXXX |}];
  print_s [%sexp (mux (of_string "X") [ gnd; vdd ] : Four_state_logic.t)];
  [%expect {| X |}]
;;

let%expect_test "cases" =
  (* [cases] is derived by folding [mux (select ==: match_with) [acc; value]] over the
     arms ([Comb.Expert.Gen_cases_from_mux]), so its impure-select behaviour follows the
     instance's [==:]: [Pessimistic] gives all X for any impure select, while [Optimistic]
     skips arms whose pure bits definitely mismatch - an impure select that definitely
     mismatches every arm falls through to [default]. A pure select passes impure data in
     the matching arm straight through. *)
  let test (module C : Comb.S with type t = Four_state_logic.t) select =
    C.cases
      ~default:(C.of_string "1111")
      (C.of_string select)
      [ C.of_string "00", C.of_string "0001"; C.of_string "01", C.of_string "001Z" ]
  in
  List.iter
    [ ( "pessimistic"
      , (module Four_state_logic.Pessimistic : Comb.S with type t = Four_state_logic.t) )
    ; "optimistic", (module Four_state_logic.Optimistic)
    ]
    ~f:(fun (name, instance) ->
      printf "%s\n" name;
      List.iter [ "00"; "01"; "10"; "0X"; "1X"; "ZZ" ] ~f:(fun select ->
        printf !"  %s -> %{Four_state_logic}\n" select (test instance select)));
  [%expect
    {|
    pessimistic
      00 -> 0001
      01 -> 001Z
      10 -> 1111
      0X -> XXXX
      1X -> XXXX
      ZZ -> XXXX
    optimistic
      00 -> 0001
      01 -> 001Z
      10 -> 1111
      0X -> XXXX
      1X -> 1111
      ZZ -> XXXX
    |}]
;;

(* Exhaustively check the contract both instances satisfy: when an operation produces a
   pure output bit, that bit's value holds under every resolution of the impure input bits
   to 0 or 1 (computed with the corresponding pure [Bits] operation). *)
let%expect_test "pure output bits hold under every resolution of impure inputs" =
  let cartesian chars_per_position ~f =
    List.fold_right chars_per_position ~init:[ [] ] ~f:(fun chars acc ->
      List.concat_map chars ~f:(fun char -> List.map acc ~f:(fun tail -> char :: tail)))
    |> List.map ~f:(fun chars -> f (String.of_char_list chars))
  in
  let all_values width =
    cartesian
      (List.init width ~f:(fun _ -> [ '0'; '1'; 'X'; 'Z' ]))
      ~f:Four_state_logic.of_string
  in
  let resolutions t =
    Four_state_logic.to_string t
    |> String.to_list
    |> List.map ~f:(function
      | ('0' | '1') as char -> [ char ]
      | 'X' | 'Z' -> [ '0'; '1' ]
      | _ -> assert false)
    |> cartesian ~f:Bits.of_string
  in
  let rec all_resolutions = function
    | [] -> [ [] ]
    | arg :: args ->
      let rest = all_resolutions args in
      List.concat_map (resolutions arg) ~f:(fun bits ->
        List.map rest ~f:(fun tail -> bits :: tail))
  in
  let check ~instance ~op_name ~args ~ours ~pure_op =
    let ours = Four_state_logic.to_string ours in
    List.iter (all_resolutions args) ~f:(fun resolved ->
      let pure = Bits.to_bstr (pure_op resolved) in
      String.iteri ours ~f:(fun i char ->
        match char with
        | 'X' | 'Z' -> ()
        | _ ->
          if not (Char.equal char pure.[i])
          then
            print_s
              [%message
                "pure output bit does not hold under a resolution of the inputs"
                  (instance : string)
                  (op_name : string)
                  (args : Four_state_logic.t list)
                  (ours : string)
                  (resolved : Bits.t list)
                  (pure : string)]))
  in
  let values = all_values 2 in
  List.iter
    [ ( "Pessimistic"
      , (module Four_state_logic.Pessimistic : Comb.S with type t = Four_state_logic.t) )
    ; "Optimistic", (module Four_state_logic.Optimistic)
    ]
    ~f:(fun (instance, (module C : Comb.S with type t = Four_state_logic.t)) ->
      List.iter values ~f:(fun a ->
        check
          ~instance
          ~op_name:"~:"
          ~args:[ a ]
          ~ours:C.(~:a)
          ~pure_op:(function
            | [ a ] -> Bits.(~:a)
            | _ -> assert false));
      List.iter (List.cartesian_product values values) ~f:(fun (a, b) ->
        List.iter
          [ "&:", C.( &: ), Bits.( &: )
          ; "|:", C.( |: ), Bits.( |: )
          ; "^:", C.( ^: ), Bits.( ^: )
          ; "==:", C.( ==: ), Bits.( ==: )
          ; "<:", C.( <: ), Bits.( <: )
          ; "+:", C.( +: ), Bits.( +: )
          ; "-:", C.( -: ), Bits.( -: )
          ; "*:", C.( *: ), Bits.( *: )
          ]
          ~f:(fun (op_name, ours_op, pure_op) ->
            check ~instance ~op_name ~args:[ a; b ] ~ours:(ours_op a b) ~pure_op:(function
              | [ a; b ] -> pure_op a b
              | _ -> assert false)));
      (* [mux] and [cases] over every 2 bit select, with impure data. *)
      let data = List.map [ "00"; "1Z"; "X1" ] ~f:Four_state_logic.of_string in
      let matches = [ "00"; "01" ] in
      List.iter values ~f:(fun select ->
        check
          ~instance
          ~op_name:"mux"
          ~args:(select :: data)
          ~ours:(C.mux select data)
          ~pure_op:(function
          | select :: data -> Bits.mux select data
          | _ -> assert false);
        let default = Four_state_logic.of_string "Z0" in
        let values = List.take data 2 in
        check
          ~instance
          ~op_name:"cases"
          ~args:(select :: default :: values)
          ~ours:
            (C.cases
               ~default
               select
               (List.zip_exn (List.map matches ~f:C.of_string) values))
          ~pure_op:(function
            | select :: default :: values ->
              Bits.cases
                ~default
                select
                (List.zip_exn (List.map matches ~f:Bits.of_string) values)
            | _ -> assert false)));
  [%expect {| |}]
;;

(* The following demonstrates how we can get more precise X propagation if we wanted it.
   Rebuild the Comb API using gates primitives - now we can see how X's can flow within
   operations. Not exactly sure how this is useful, but you can do it! *)

module Gates = Comb.Make (Comb.Make_primitives (Four_state_logic.Optimistic))

let%expect_test "propagation using gate primitives" =
  let a = Four_state_logic.Optimistic.of_string "1001X01" in
  let b = Four_state_logic.Optimistic.of_string "1000110" in
  let c = Gates.( +: ) a b in
  print_s [%message (c : Gates.t)];
  [%expect {| (c 00XXX11) |}]
;;
