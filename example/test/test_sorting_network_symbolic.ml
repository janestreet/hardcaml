open! Import
open! Sorting_network

module Types = struct
  module rec Wire : sig
    type t =
      | Input of string
      | Compare_and_swap_output of { compare_and_swap : Compare_and_swap.t
                                   ; min_or_max       : Min_or_max.t }
  end = Wire
  and Compare_and_swap : sig
    type t =
      { id     : int
      ; depth  : int
      ; input1 : Wire.t
      ; input2 : Wire.t }
  end = Compare_and_swap
end

open Types

let id_counter = ref 0

let reset_id_counter () = id_counter := 0

let next_id () = incr id_counter; !id_counter

module Wire = struct
  include Wire

  let sexp_of_t = function
    | Input string -> [%message "input" string]
    | Compare_and_swap_output { compare_and_swap; min_or_max } ->
      [%message "compare_and_swap"
                  ~_:(compare_and_swap.id : int)
                  ~_:(min_or_max : Min_or_max.t)]

  let depth t =
    match t with
    | Input _ -> 0
    | Compare_and_swap_output o -> o.compare_and_swap.depth

  let create_input string = Input string

  let create_compare_and_swap input1 input2 : t Sorting_network.Min_max.t =
    let compare_and_swap =
      { Compare_and_swap.
        id = next_id ()
      ; depth = 1 + Int.max (depth input1) (depth input2)
      ; input1
      ; input2 } in
    let output min_or_max = Compare_and_swap_output { compare_and_swap; min_or_max } in
    { min = output Min; max = output Max }
end

module Compare_and_swap = struct
  type t = Compare_and_swap.t =
    { id     : int
    ; depth  : int
    ; input1 : Wire.t
    ; input2 : Wire.t }
  [@@deriving fields]

  let sexp_of_t { id; depth = _; input1; input2 } =
    [%message
      "compare_and_swap"
        ~_:(id     : int)
        (input1 : Wire.t)
        (input2 : Wire.t)]
end

let sexp_of_outputs outputs =
  let compare_and_swap_by_id = Hashtbl.create (module Int) in
  let rec loop (wire : Wire.t) =
    match wire with
    | Input _ -> ()
    | Compare_and_swap_output { compare_and_swap; _ } ->
      if not (Hashtbl.mem compare_and_swap_by_id compare_and_swap.id)
      then (
        Hashtbl.add_exn compare_and_swap_by_id ~key:compare_and_swap.id ~data:compare_and_swap;
        loop compare_and_swap.input1;
        loop compare_and_swap.input2) in
  List.iter outputs ~f:loop;
  let compare_and_swap_by_depth =
    Hashtbl.data compare_and_swap_by_id
    |> List.map ~f:(fun compare_and_swap -> compare_and_swap.depth, compare_and_swap)
    |> Map.of_alist_multi (module Int) in
  [%message
    ""
      (outputs : Wire.t list)
      (compare_and_swap_by_depth : Compare_and_swap.t list Map.M(Int).t)]

let%expect_test _ =
  List.iter Config.all ~f:(fun config ->
    List.iter [ 1; 2; 4; 8 ] ~f:(fun num_inputs ->
      reset_id_counter ();
      let inputs = List.init num_inputs ~f:(fun i -> concat [ "i"; Int.to_string i ]) in
      print_s [%message
        ""
          (config : Config.t)
          (inputs : string list)
          ~_:(Or_error.try_with (fun () ->
            create config Wire.create_compare_and_swap
              (List.map inputs ~f:Wire.create_input)) : outputs Or_error.t)]));
  [%expect {|
    ((config Bitonic_sort)
     (inputs (i0))
     (Ok ((outputs ((input i0))) (compare_and_swap_by_depth ()))))
    ((config Bitonic_sort)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         (compare_and_swap 1 Min)
         (compare_and_swap 1 Max)))
       (compare_and_swap_by_depth ((
         1 ((
           compare_and_swap 1
           (input1 (input i0))
           (input2 (input i1))))))))))
    ((config Bitonic_sort)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         (compare_and_swap 5 Min)
         (compare_and_swap 5 Max)
         (compare_and_swap 6 Min)
         (compare_and_swap 6 Max)))
       (compare_and_swap_by_depth (
         (1 (
           (compare_and_swap 2
             (input1 (input i2))
             (input2 (input i3)))
           (compare_and_swap 1
             (input1 (input i0))
             (input2 (input i1)))))
         (2 (
           (compare_and_swap 3
             (input1 (compare_and_swap 1 Min))
             (input2 (compare_and_swap 2 Max)))
           (compare_and_swap 4
             (input1 (compare_and_swap 1 Max))
             (input2 (compare_and_swap 2 Min)))))
         (3 (
           (compare_and_swap 6
             (input1 (compare_and_swap 3 Max))
             (input2 (compare_and_swap 4 Max)))
           (compare_and_swap 5
             (input1 (compare_and_swap 3 Min))
             (input2 (compare_and_swap 4 Min))))))))))
    ((config Bitonic_sort)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         (compare_and_swap 19 Min)
         (compare_and_swap 19 Max)
         (compare_and_swap 20 Min)
         (compare_and_swap 20 Max)
         (compare_and_swap 23 Min)
         (compare_and_swap 23 Max)
         (compare_and_swap 24 Min)
         (compare_and_swap 24 Max)))
       (compare_and_swap_by_depth (
         (1 (
           (compare_and_swap 2
             (input1 (input i2))
             (input2 (input i3)))
           (compare_and_swap 7
             (input1 (input i4))
             (input2 (input i5)))
           (compare_and_swap 1
             (input1 (input i0))
             (input2 (input i1)))
           (compare_and_swap 8
             (input1 (input i6))
             (input2 (input i7)))))
         (2 (
           (compare_and_swap 10
             (input1 (compare_and_swap 7 Max))
             (input2 (compare_and_swap 8 Min)))
           (compare_and_swap 3
             (input1 (compare_and_swap 1 Min))
             (input2 (compare_and_swap 2 Max)))
           (compare_and_swap 4
             (input1 (compare_and_swap 1 Max))
             (input2 (compare_and_swap 2 Min)))
           (compare_and_swap 9
             (input1 (compare_and_swap 7 Min))
             (input2 (compare_and_swap 8 Max)))))
         (3 (
           (compare_and_swap 11
             (input1 (compare_and_swap 9  Max))
             (input2 (compare_and_swap 10 Max)))
           (compare_and_swap 12
             (input1 (compare_and_swap 9  Min))
             (input2 (compare_and_swap 10 Min)))
           (compare_and_swap 6
             (input1 (compare_and_swap 3 Max))
             (input2 (compare_and_swap 4 Max)))
           (compare_and_swap 5
             (input1 (compare_and_swap 3 Min))
             (input2 (compare_and_swap 4 Min)))))
         (4 (
           (compare_and_swap 15
             (input1 (compare_and_swap 6  Min))
             (input2 (compare_and_swap 12 Max)))
           (compare_and_swap 14
             (input1 (compare_and_swap 5  Max))
             (input2 (compare_and_swap 11 Min)))
           (compare_and_swap 13
             (input1 (compare_and_swap 5  Min))
             (input2 (compare_and_swap 11 Max)))
           (compare_and_swap 16
             (input1 (compare_and_swap 6  Max))
             (input2 (compare_and_swap 12 Min)))))
         (5 (
           (compare_and_swap 18
             (input1 (compare_and_swap 14 Min))
             (input2 (compare_and_swap 16 Min)))
           (compare_and_swap 22
             (input1 (compare_and_swap 14 Max))
             (input2 (compare_and_swap 16 Max)))
           (compare_and_swap 17
             (input1 (compare_and_swap 13 Min))
             (input2 (compare_and_swap 15 Min)))
           (compare_and_swap 21
             (input1 (compare_and_swap 13 Max))
             (input2 (compare_and_swap 15 Max)))))
         (6 (
           (compare_and_swap 23
             (input1 (compare_and_swap 21 Min))
             (input2 (compare_and_swap 22 Min)))
           (compare_and_swap 24
             (input1 (compare_and_swap 21 Max))
             (input2 (compare_and_swap 22 Max)))
           (compare_and_swap 19
             (input1 (compare_and_swap 17 Min))
             (input2 (compare_and_swap 18 Min)))
           (compare_and_swap 20
             (input1 (compare_and_swap 17 Max))
             (input2 (compare_and_swap 18 Max))))))))))
    ((config Odd_even_merge_sort)
     (inputs (i0))
     (Ok ((outputs ((input i0))) (compare_and_swap_by_depth ()))))
    ((config Odd_even_merge_sort)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         (compare_and_swap 1 Min)
         (compare_and_swap 1 Max)))
       (compare_and_swap_by_depth ((
         1 ((
           compare_and_swap 1
           (input1 (input i0))
           (input2 (input i1))))))))))
    ((config Odd_even_merge_sort)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         (compare_and_swap 4 Min)
         (compare_and_swap 5 Min)
         (compare_and_swap 5 Max)
         (compare_and_swap 3 Max)))
       (compare_and_swap_by_depth (
         (1 (
           (compare_and_swap 2
             (input1 (input i0))
             (input2 (input i1)))
           (compare_and_swap 1
             (input1 (input i2))
             (input2 (input i3)))))
         (2 (
           (compare_and_swap 3
             (input1 (compare_and_swap 2 Max))
             (input2 (compare_and_swap 1 Max)))
           (compare_and_swap 4
             (input1 (compare_and_swap 2 Min))
             (input2 (compare_and_swap 1 Min)))))
         (3 ((
           compare_and_swap 5
           (input1 (compare_and_swap 3 Min))
           (input2 (compare_and_swap 4 Max))))))))))
    ((config Odd_even_merge_sort)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         (compare_and_swap 15 Min)
         (compare_and_swap 17 Min)
         (compare_and_swap 17 Max)
         (compare_and_swap 18 Min)
         (compare_and_swap 18 Max)
         (compare_and_swap 19 Min)
         (compare_and_swap 19 Max)
         (compare_and_swap 11 Max)))
       (compare_and_swap_by_depth (
         (1 (
           (compare_and_swap 2
             (input1 (input i4))
             (input2 (input i5)))
           (compare_and_swap 7
             (input1 (input i0))
             (input2 (input i1)))
           (compare_and_swap 1
             (input1 (input i6))
             (input2 (input i7)))
           (compare_and_swap 6
             (input1 (input i2))
             (input2 (input i3)))))
         (2 (
           (compare_and_swap 3
             (input1 (compare_and_swap 2 Max))
             (input2 (compare_and_swap 1 Max)))
           (compare_and_swap 8
             (input1 (compare_and_swap 7 Max))
             (input2 (compare_and_swap 6 Max)))
           (compare_and_swap 4
             (input1 (compare_and_swap 2 Min))
             (input2 (compare_and_swap 1 Min)))
           (compare_and_swap 9
             (input1 (compare_and_swap 7 Min))
             (input2 (compare_and_swap 6 Min)))))
         (3 (
           (compare_and_swap 10
             (input1 (compare_and_swap 8 Min))
             (input2 (compare_and_swap 9 Max)))
           (compare_and_swap 15
             (input1 (compare_and_swap 9 Min))
             (input2 (compare_and_swap 4 Min)))
           (compare_and_swap 11
             (input1 (compare_and_swap 8 Max))
             (input2 (compare_and_swap 3 Max)))
           (compare_and_swap 5
             (input1 (compare_and_swap 3 Min))
             (input2 (compare_and_swap 4 Max)))))
         (4 (
           (compare_and_swap 14
             (input1 (compare_and_swap 10 Max))
             (input2 (compare_and_swap 5  Max)))
           (compare_and_swap 12
             (input1 (compare_and_swap 10 Min))
             (input2 (compare_and_swap 5  Min)))))
         (5 (
           (compare_and_swap 13
             (input1 (compare_and_swap 11 Min))
             (input2 (compare_and_swap 12 Max)))
           (compare_and_swap 16
             (input1 (compare_and_swap 14 Min))
             (input2 (compare_and_swap 15 Max)))))
         (6 (
           (compare_and_swap 18
             (input1 (compare_and_swap 13 Min))
             (input2 (compare_and_swap 16 Max)))
           (compare_and_swap 17
             (input1 (compare_and_swap 12 Min))
             (input2 (compare_and_swap 16 Min)))
           (compare_and_swap 19
             (input1 (compare_and_swap 13 Max))
             (input2 (compare_and_swap 14 Max)))))))))) |}]
;;
