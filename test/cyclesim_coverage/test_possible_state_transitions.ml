open Core
open Hardcaml
open Signal
open Always

let clock = Signal.input "clock" 1
let reg_spec = Reg_spec.create () ~clock

module State = struct
  type t =
    | A
    | B
    | C
    | D
  [@@deriving compare ~localize, enumerate, sexp_of, variants]
end

let create_sm encoding =
  State_machine.create (module State) reg_spec ~encoding ~enable:vdd
;;

let test ?(encodings = State_machine.Encoding.all) ~f ~expect () =
  Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
  List.iter encodings ~f:(fun encoding ->
    let sm = create_sm encoding in
    compile (f sm);
    print_endline "Valid transitions:";
    (match Signal.Type.coverage_metadata sm.current with
     | Some
         { kind =
             Some
               (Variable
                 (State_machine_state { states_by_value; transitions_by_value; _ }))
         ; _
         } ->
       let get_state_exn value = Map.find_exn states_by_value value in
       let transitions =
         Hashtbl.to_alist transitions_by_value
         |> List.concat_map ~f:(fun (from, to_set) ->
           Hash_set.to_list to_set
           |> List.map ~f:(fun to_ ->
             { Coverage_prim.Transition.from = get_state_exn from
             ; to_ = get_state_exn to_
             }))
         |> List.sort ~compare:(fun a b ->
           [%compare: string * string] (a.from.name, a.to_.name) (b.from.name, b.to_.name))
       in
       List.iter transitions ~f:(fun transition ->
         print_endline
           (Coverage_prim.Transition.to_string transition ~f:(fun t -> t.name)))
     | _ -> ());
    expect ())
;;

let%expect_test "no transitions" =
  test ~f:(fun _sm -> []) ~expect:(fun () -> [%expect {| Valid transitions: |}]) ();
  test
    ~f:(fun _sm ->
      let input = input "input" 1 in
      let var = Variable.wire ~default:gnd () in
      [ if_ input [ var <--. 1 ] [ var <--. 2 ] ])
    ~expect:(fun () -> [%expect {| Valid transitions: |}])
    ()
;;

let%expect_test "switch" =
  let f (sm : State.t State_machine.t) =
    [ sm.switch
        [ A, [ sm.set_next B ]
        ; B, [ sm.set_next C ]
        ; C, [ sm.set_next D ]
        ; D, [ sm.set_next A ]
        ]
    ]
  in
  test
    ~f
    ~encodings:[ Binary; Gray ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> B
        B -> C
        C -> D
        D -> A
        |}])
    ();
  (* Onehot encoding is written as list of if statements. We don't do special analysis on
     sequential statements to determine if they cover the full state space, so we will
     always detect state -> state transitions. *)
  test
    ~f
    ~encodings:[ Onehot ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        B -> B
        B -> C
        C -> C
        C -> D
        D -> A
        D -> D
        |}])
    ()
;;

let%expect_test "switch - default " =
  let f (sm : State.t State_machine.t) =
    [ sm.switch
        [ A, [ sm.set_next B ]; B, [ sm.set_next C ]; C, [ sm.set_next D ] ]
        ~default:[]
    ]
  in
  test
    ~f
    ~encodings:[ Binary; Gray ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> B
        B -> C
        C -> D
        D -> D
        |}])
    ();
  test
    ~f
    ~encodings:[ Onehot ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        B -> B
        B -> C
        C -> C
        C -> D
        D -> D
        |}])
    ()
;;

let%expect_test "switch - default impossible state" =
  let f (sm : State.t State_machine.t) =
    [ sm.switch
        [ A, [ sm.set_next B ]; B, [ sm.set_next C ]; C, [ sm.set_next C ] ]
        ~default:[]
    ]
  in
  test
    ~f
    ~encodings:[ Binary; Gray ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> B
        B -> C
        C -> C
        |}])
    ();
  test
    ~f
    ~encodings:[ Onehot ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        B -> B
        B -> C
        C -> C
        |}])
    ()
;;

let%expect_test "switch - nested chained set" =
  let f (sm : State.t State_machine.t) =
    let input = input "input" 1 in
    [ sm.switch
        [ A, [ when_ input [ sm.set_next C ]; when_ input [ sm.set_next B ] ]
        ; B, [ sm.set_next D ]
        ; C, [ sm.set_next D ]
        ; D, [ sm.set_next D ]
        ]
    ]
  in
  test
    ~f
    ~encodings:[ Binary; Gray ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        A -> C
        B -> D
        C -> D
        D -> D
        |}])
    ();
  test
    ~f
    ~encodings:[ Onehot ]
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        A -> C
        B -> B
        B -> D
        C -> C
        C -> D
        D -> D
        |}])
    ()
;;

let%expect_test "if - full coverage" =
  test
    ~f:(fun sm ->
      let input = input "input" 1 in
      [ if_ input [ sm.set_next B ] [ sm.set_next C ] ])
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> B
        A -> C
        B -> B
        B -> C
        C -> B
        C -> C
        |}])
    ()
;;

let%expect_test "if - partial coverage" =
  test
    ~f:(fun sm ->
      let input = input "input" 1 in
      [ if_ input [ sm.set_next B ] [] ])
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        B -> B
        |}])
    ()
;;

let%expect_test "if - with explicit set - full coverage" =
  test
    ~f:(fun sm ->
      let input = input "input" 1 in
      [ sm.set_next C; if_ input [ sm.set_next B ] [ sm.set_next A ] ])
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        B -> A
        B -> B
        |}])
    ()
;;

let%expect_test "if - with explicit set - partial coverage" =
  test
    ~f:(fun sm ->
      let input = input "input" 1 in
      [ sm.set_next C; if_ input [ sm.set_next B ] [] ])
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> B
        A -> C
        B -> B
        B -> C
        C -> B
        C -> C
        |}])
    ()
;;

let%expect_test "full transitions" =
  test
    ~f:(fun sm ->
      let i1 = input "i1" 1 in
      let i2 = input "i2" 1 in
      let i3 = input "i3" 1 in
      [ if_ i1 [ sm.set_next A ]
        @@ elif i2 [ sm.set_next B ]
        @@ elif i3 [ sm.set_next C ] [ sm.set_next D ]
      ])
    ~expect:(fun () ->
      [%expect
        {|
        Valid transitions:
        A -> A
        A -> B
        A -> C
        A -> D
        B -> A
        B -> B
        B -> C
        B -> D
        C -> A
        C -> B
        C -> C
        C -> D
        D -> A
        D -> B
        D -> C
        D -> D
        |}])
    ()
;;
