open! Core

module Time = struct
  type t = int [@@deriving compare ~localize, sexp_of]

  let zero = 0
end

module Data = struct
  type t = int list [@@deriving sexp_of]

  let none = []
  let merge a b = a @ b
end

module S = Hardcaml.Wave_data_in_events.Make (Time) (Data)

let test_insert ~verbose data =
  let t = S.create () in
  if verbose then print_s [%message (t : S.t)];
  List.iter data ~f:(fun (time, data) ->
    S.insert t time data;
    if verbose then print_s [%message (t : S.t)]);
  t
;;

let%expect_test "simple examples" =
  ignore (test_insert ~verbose:true [ 0, [ 100 ] ] : S.t);
  [%expect
    {|
    (t ((data (())) (time (0)) (length 0)))
    (t ((data ((100))) (time (0)) (length 1)))
    |}];
  ignore (test_insert ~verbose:true [ 0, [ 100 ]; 1, [ 200 ] ] : S.t);
  [%expect
    {|
    (t ((data (())) (time (0)) (length 0)))
    (t ((data ((100))) (time (0)) (length 1)))
    (t ((data ((100) (200))) (time (0 1)) (length 2)))
    |}];
  ignore (test_insert ~verbose:true [ 10, [ 100 ]; 9, [ 200 ] ] : S.t);
  [%expect
    {|
    (t ((data (())) (time (0)) (length 0)))
    (t ((data ((100))) (time (10)) (length 1)))
    (t ((data ((200) (100))) (time (9 10)) (length 2)))
    |}];
  ignore
    (test_insert ~verbose:true [ 10, [ 100 ]; 20, [ 200 ]; 30, [ 300 ]; 10, [ 150 ] ]
     : S.t);
  [%expect
    {|
    (t ((data (())) (time (0)) (length 0)))
    (t ((data ((100))) (time (10)) (length 1)))
    (t ((data ((100) (200))) (time (10 20)) (length 2)))
    (t ((data ((100) (200) (300) ())) (time (10 20 30 0)) (length 3)))
    (t ((data ((100 150) (200) (300) ())) (time (10 20 30 0)) (length 3)))
    |}]
;;

(* Create a map of time -> data list. Merge data lists for equal keys. Finally convert to
   an sorted (by time) array, and also sort the data lists. *)
let reference_implementation data =
  List.fold
    data
    ~init:(Map.empty (module Int))
    ~f:(fun map (time, data) ->
      match Map.find map time with
      | None -> Map.add_exn map ~key:time ~data
      | Some els -> Map.set map ~key:time ~data:(els @ data))
  |> Map.map ~f:(fun data -> List.sort ~compare:Int.compare data)
  |> Map.to_alist
  |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> Array.of_list
;;

let random_insertions ~max_time ~count =
  let data =
    List.init count ~f:(fun _ -> Random.int max_time, [ Random.int 1_000_000 ])
  in
  let t = test_insert ~verbose:false data in
  let reference = reference_implementation data in
  if S.length t <> Array.length reference
  then
    raise_s
      [%message
        "Times are missing or repeated somewhere"
          (S.length t : int)
          (Array.length reference : int)];
  let total = ref 0 in
  let mismatch msg =
    raise_s [%message msg (t : S.t) (reference : (int * int list) array)]
  in
  for i = 0 to S.length t - 1 do
    let time, data = reference.(i) in
    if S.get_time_at_index t i <> time then mismatch "time mismatch";
    let data' = S.get_data_at_index t i |> List.sort ~compare:Int.compare in
    if not ([%compare.equal: int list] data data') then mismatch "data mismatch";
    total := !total + List.length data
  done;
  if !total <> count then mismatch "Total number of elements doesn't match"
;;

let%expect_test "Randomised tests - packed into small time range" =
  for _ = 1 to 100 do
    random_insertions ~max_time:10 ~count:100
  done;
  [%expect {| |}]
;;

let%expect_test "Randomised tests - sparse over time" =
  for _ = 1 to 100 do
    random_insertions ~max_time:10_000 ~count:100
  done;
  [%expect {| |}]
;;

let%expect_test "for simulator" =
  (* create transactions starting at 0 and running onwards *)
  let t = test_insert ~verbose:false (List.init 5 ~f:(fun i -> i * 3, [ i ])) in
  for i = 0 to 17 do
    print_s [%sexp ((i, S.get t i) : int * int list)]
  done;
  [%expect
    {|
    (0 (0))
    (1 (0))
    (2 (0))
    (3 (1))
    (4 (1))
    (5 (1))
    (6 (2))
    (7 (2))
    (8 (2))
    (9 (3))
    (10 (3))
    (11 (3))
    (12 (4))
    (13 (4))
    (14 (4))
    (15 (4))
    (16 (4))
    (17 (4))
    |}]
;;
