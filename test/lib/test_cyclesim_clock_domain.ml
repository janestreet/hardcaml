open! Import
open! Core
open Cyclesim_clock_domain

let generate_domains () =
  let open Quickcheck.Generator.Let_syntax in
  let%bind num_domains = Int.gen_uniform_incl 1 10 in
  let%map domain_specs =
    List.init num_domains ~f:(fun i ->
      let%map period = Int.gen_uniform_incl 1 10 in
      Printf.sprintf "clk%d" i, period)
    |> Quickcheck.Generator.all
  in
  Group.create_exn (create_list domain_specs)
;;

let select_indexed group =
  let open Quickcheck.Generator.Let_syntax in
  let elements = Group.elements group in
  let%map idx = Int.gen_uniform_incl 0 (Iarray.length elements - 1) in
  Iarray.get elements idx
;;

let name indexed = (domain indexed).name

module Test_table = struct
  open Quickcheck
  open Generator.Let_syntax

  module Step = struct
    type t =
      | Set of indexed * int
      | Get of indexed
      | Map of int
    [@@deriving sexp_of]
  end

  type t =
    { steps : Step.t list
    ; domains : Group.t
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let%bind domains = generate_domains () in
    let%bind step_len = Int.gen_uniform_incl 10 100 in
    let%bind steps =
      let generate_step =
        Generator.weighted_union
          [ ( 1.0
            , let%bind indexed = select_indexed domains in
              let%map value = Int.quickcheck_generator in
              Step.Set (indexed, value) )
          ; ( 0.5
            , let%map indexed = select_indexed domains in
              Step.Get indexed )
          ; ( 0.3
            , let%map delta = Int.quickcheck_generator in
              Step.Map delta )
          ]
      in
      List.init step_len ~f:(fun _ -> generate_step) |> Generator.all
    in
    return { steps; domains }
  ;;

  let run ?(debug = false) t =
    if debug then print_s [%message "-----TABLE TEST RUN"];
    let table = Table.create t.domains 0 in
    let ref_map =
      Group.elements t.domains
      |> Iarray.to_list
      |> List.map ~f:(fun indexed -> name indexed, 0)
      |> Hashtbl.of_alist_exn (module Name)
    in
    List.iter t.steps ~f:(fun step ->
      if debug then print_s [%message (step : Step.t)];
      match step with
      | Set (indexed, value) ->
        Table.set table ~key:indexed ~data:value;
        Hashtbl.set ref_map ~key:(name indexed) ~data:value
      | Get indexed ->
        let table_value = Table.get table indexed in
        let ref_value = Hashtbl.find_exn ref_map (name indexed) in
        [%test_eq: int] table_value ref_value
      | Map delta ->
        let validate_table_and_ref_map () =
          Iarray.iter (Group.elements t.domains) ~f:(fun indexed ->
            let name = (domain indexed).name in
            let table_value = Table.get table indexed in
            let ref_value = Hashtbl.find_exn ref_map name in
            [%test_eq: int] table_value ref_value)
        in
        validate_table_and_ref_map ();
        Table.map_inplace table ~f:(fun x -> x + delta);
        Hashtbl.map_inplace ref_map ~f:(fun x -> x + delta);
        validate_table_and_ref_map ())
  ;;
end

let%test_unit "Table quickcheck" =
  let open Test_table in
  Quickcheck.test ~sexp_of:sexp_of_t quickcheck_generator ~f:(run ~debug:false)
;;

module Test_set = struct
  open Quickcheck
  open Generator.Let_syntax

  module Step = struct
    type t =
      | Add of indexed
      | Remove of indexed
      | Mem of indexed
      | Iter
    [@@deriving sexp_of]
  end

  type t =
    { steps : Step.t list
    ; domains : Group.t
    ; default : bool
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let%bind domains = generate_domains () in
    let%bind default = Bool.quickcheck_generator in
    let%bind step_len = Int.gen_uniform_incl 10 100 in
    let%bind steps =
      let generate_step =
        Generator.weighted_union
          [ ( 1.0
            , let%map indexed = select_indexed domains in
              Step.Add indexed )
          ; ( 1.0
            , let%map indexed = select_indexed domains in
              Step.Remove indexed )
          ; ( 0.5
            , let%map indexed = select_indexed domains in
              Step.Mem indexed )
          ; 0.3, return Step.Iter
          ]
      in
      List.init step_len ~f:(fun _ -> generate_step) |> Generator.all
    in
    return { steps; domains; default }
  ;;

  let run ?(debug = false) t =
    if debug then print_s [%message "-----SET TEST RUN"];
    let set = Set.create t.domains ~default:t.default in
    let ref_set =
      if t.default
      then
        Group.elements t.domains
        |> Iarray.to_list
        |> List.map ~f:(fun indexed -> name indexed)
        |> Hash_set.of_list (module Name)
      else Hash_set.create (module Name)
    in
    List.iter t.steps ~f:(fun step ->
      if debug then print_s [%message (step : Step.t)];
      match step with
      | Add indexed ->
        Set.add set indexed;
        Hash_set.add ref_set (name indexed)
      | Remove indexed ->
        Set.remove set indexed;
        Hash_set.remove ref_set (name indexed)
      | Mem indexed ->
        let set_value = Set.mem set indexed in
        let ref_value = Hash_set.mem ref_set (name indexed) in
        [%test_eq: bool] set_value ref_value
      | Iter ->
        let set_elements = ref [] in
        Set.iter set ~f:(fun indexed -> set_elements := name indexed :: !set_elements);
        let iter_set = Hash_set.of_list (module Name) !set_elements in
        if not ([%equal: Hash_set.M(Name).t] iter_set ref_set)
        then
          raise_s
            [%message
              "iter_set and ref_set not equal"
                (iter_set : Hash_set.M(Name).t)
                (ref_set : Hash_set.M(Name).t)])
  ;;
end

let%test_unit "Set quickcheck" =
  let open Test_set in
  Quickcheck.test ~sexp_of:sexp_of_t quickcheck_generator ~f:(run ~debug:false)
;;

let get_nth group i = Iarray.get (Group.elements group) i

let iter_and_print_table group table =
  Iarray.iter (Group.elements group) ~f:(fun indexed ->
    let value = Table.get table indexed in
    print_s [%message "" ~name:((domain indexed).name : Name.t) (value : int)])
;;

let%expect_test "Table.create and get" =
  let group = create_list [ "clk1", 1; "clk2", 2; "clk3", 3 ] |> Group.create_exn in
  let table = Table.create group 42 in
  iter_and_print_table group table;
  [%expect
    {|
    ((name clk1) (value 42))
    ((name clk2) (value 42))
    ((name clk3) (value 42))
    |}]
;;

let%expect_test "Table.init" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let table = Table.init group ~f:(fun indexed -> (domain indexed).period) in
  iter_and_print_table group table;
  [%expect
    {|
    ((name clk1) (value 1))
    ((name clk2) (value 2))
    |}]
;;

let%expect_test "Table.set and get" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let table = Table.create group 0 in
  Table.set table ~key:(get_nth group 0) ~data:100;
  Table.set table ~key:(get_nth group 1) ~data:200;
  iter_and_print_table group table;
  [%expect
    {|
    ((name clk1) (value 100))
    ((name clk2) (value 200))
    |}]
;;

let%expect_test "Table.map" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let table = Table.create group 10 in
  let mapped_table = Table.map table ~f:(fun x -> x * 2) in
  iter_and_print_table group mapped_table;
  [%expect
    {|
    ((name clk1) (value 20))
    ((name clk2) (value 20))
    |}]
;;

let iter_and_print_set set =
  Set.iter set ~f:(fun indexed ->
    print_s [%message "" ~name:((domain indexed).name : Name.t)])
;;

let%expect_test "Set.create with default false" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let set = Set.create group ~default:false in
  iter_and_print_set set;
  [%expect {| |}]
;;

let%expect_test "Set.create with default true" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let set = Set.create group ~default:true in
  iter_and_print_set set;
  [%expect
    {|
    (name clk1)
    (name clk2)
    |}]
;;

let%expect_test "Set.add and mem" =
  let group = create_list [ "clk1", 1; "clk2", 2; "clk3", 3 ] |> Group.create_exn in
  let set = Set.create group ~default:false in
  Set.add set (get_nth group 0);
  Set.add set (get_nth group 2);
  iter_and_print_set set;
  [%expect
    {|
    (name clk1)
    (name clk3)
    |}]
;;

let%expect_test "Set.remove" =
  let group = create_list [ "clk1", 1; "clk2", 2 ] |> Group.create_exn in
  let set = Set.create group ~default:true in
  Set.remove set (get_nth group 0);
  iter_and_print_set set;
  [%expect {| (name clk2) |}]
;;

(* Test Group operations *)

let%expect_test "Group.create with duplicate keys" =
  let result = create_list [ "clk1", 1; "clk2", 2; "clk1", 3 ] |> Group.create in
  print_s [%sexp (result : [ `Ok of Group.t | `Duplicate_key of Name.t ])];
  [%expect {| (Duplicate_key clk1) |}]
;;

let%expect_test "Group.get" =
  let group = create_list [ "clk1", 1; "clk2", 2; "clk3z", 3 ] |> Group.create_exn in
  let clk1 = Group.get group (Name.of_string "clk1") in
  let clk2 = Group.get group (Name.of_string "clk2") in
  let clk_missing = Group.get group (Name.of_string "clk_missing") in
  print_s [%message (clk1 : indexed option)];
  print_s [%message (clk2 : indexed option)];
  print_s [%message (clk_missing : indexed option)];
  [%expect
    {|
    (clk1 (((domain ((name clk1) (period 1))) (index 0))))
    (clk2 (((domain ((name clk2) (period 2))) (index 1))))
    (clk_missing ())
    |}]
;;
