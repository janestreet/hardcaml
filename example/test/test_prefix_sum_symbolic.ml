open! Import
open! Prefix_sum

module Wire = struct
  type t =
    | Input of string
    | Adder of { id : int
               ; depth : int
               ; input1 : t
               ; input2 : t }

  let id_counter = ref 0

  let reset_id_counter () = id_counter := 0

  let next_id () = incr id_counter; !id_counter

  let sexp_of_t_name = function
    | Input string -> [%message "input" string]
    | Adder add -> [%message "adder" ~_:(add.id : int)]

  let sexp_of_t = function
    | Input string -> [%message "input" string]
    | Adder { id; input1; input2; _ } ->
      [%message "adder" ~_:(id : int) ~_:(input1 : t_name) ~_:(input2 : t_name)]

  let rec to_inputs = function
    | Input string -> [ string ]
    | Adder { input1; input2; _ } -> to_inputs input1 @ to_inputs input2

  let sexp_of_t t = [%message "" ~expanded_sum:(to_inputs t : string list) ~_:(t : t)]

  let sexp_of_output t =
    [%message "" ~expanded_sum:(to_inputs t : string list) ~_:(t : t_name)]

  let depth t =
    match t with
    | Input _ -> 0
    | Adder add -> add.depth

  let add input1 input2 =
    Adder { id = next_id ()
          ; depth = 1 + Int.max (depth input1) (depth input2)
          ; input1
          ; input2 }
end

let sexp_of_outputs outputs =
  let add_by_id = Hashtbl.create (module Int) in
  let rec loop (wire : Wire.t) =
    match wire with
    | Input _ -> ()
    | Adder { id; input1; input2; _ } ->
      if not (Hashtbl.mem add_by_id id)
      then (
        Hashtbl.add_exn add_by_id ~key:id ~data:wire;
        loop input1;
        loop input2) in
  List.iter outputs ~f:loop;
  let add_by_depth =
    Hashtbl.data add_by_id
    |> List.map ~f:(fun add -> Wire.depth add, add)
    |> Map.of_alist_multi (module Int) in
  [%message
    ""
      (outputs : Wire.output list)
      (add_by_depth : Wire.t list Map.M(Int).t)]

let%expect_test _ =
  List.iter Config.all ~f:(fun config ->
    List.iter [ 1; 2; 4; 8 ] ~f:(fun num_inputs ->
      Wire.reset_id_counter ();
      let inputs = List.init num_inputs ~f:(fun i -> concat [ "i"; Int.to_string i ]) in
      print_s [%message
        ""
          (config : Config.t)
          (inputs : string list)
          ~_:(Or_error.try_with
                (fun () ->
                   eval ~config ~operator:Wire.add
                     (List.map inputs ~f:(fun i -> Wire.Input i)))
              : outputs Or_error.t)]));
  [%expect {|
    ((config Serial)
     (inputs (i0))
     (Ok ((outputs (((expanded_sum (i0)) (input i0)))) (add_by_depth ()))))
    ((config Serial)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0)) ((expanded_sum (i0 i1)) (adder 1))))
       (add_by_depth ((
         1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1))))))))))
    ((config Serial)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 2))
         ((expanded_sum (i0 i1 i2 i3)) (adder 3))))
       (add_by_depth (
         (1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1)))))
         (2 ((
           (expanded_sum (i0 i1 i2))
           (adder 2
             (adder 1)
             (input i2)))))
         (3 ((
           (expanded_sum (i0 i1 i2 i3))
           (adder 3
             (adder 2)
             (input i3))))))))))
    ((config Serial)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 2))
         ((expanded_sum (i0 i1 i2 i3)) (adder 3))
         ((expanded_sum (i0 i1 i2 i3 i4)) (adder 4))
         ((expanded_sum (i0 i1 i2 i3 i4 i5)) (adder 5))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6)) (adder 6))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7)) (adder 7))))
       (add_by_depth (
         (1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1)))))
         (2 ((
           (expanded_sum (i0 i1 i2))
           (adder 2
             (adder 1)
             (input i2)))))
         (3 ((
           (expanded_sum (i0 i1 i2 i3))
           (adder 3
             (adder 2)
             (input i3)))))
         (4 ((
           (expanded_sum (i0 i1 i2 i3 i4))
           (adder 4
             (adder 3)
             (input i4)))))
         (5 ((
           (expanded_sum (i0 i1 i2 i3 i4 i5))
           (adder 5
             (adder 4)
             (input i5)))))
         (6 ((
           (expanded_sum (i0 i1 i2 i3 i4 i5 i6))
           (adder 6
             (adder 5)
             (input i6)))))
         (7 ((
           (expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7))
           (adder 7
             (adder 6)
             (input i7))))))))))
    ((config Sklansky)
     (inputs (i0))
     (Ok ((outputs (((expanded_sum (i0)) (input i0)))) (add_by_depth ()))))
    ((config Sklansky)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0)) ((expanded_sum (i0 i1)) (adder 1))))
       (add_by_depth ((
         1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1))))))))))
    ((config Sklansky)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 3))
         ((expanded_sum (i0 i1 i2 i3)) (adder 4))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i2 i3))
            (adder 2
              (input i2)
              (input i3)))))
         (2 (
           ((expanded_sum (i0 i1 i2 i3))
            (adder 4
              (adder 1)
              (adder 2)))
           ((expanded_sum (i0 i1 i2))
            (adder 3
              (adder 1)
              (input i2))))))))))
    ((config Sklansky)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 3))
         ((expanded_sum (i0 i1 i2 i3)) (adder 4))
         ((expanded_sum (i0 i1 i2 i3 i4)) (adder 9))
         ((expanded_sum (i0 i1 i2 i3 i4 i5)) (adder 10))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6)) (adder 11))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7)) (adder 12))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i2 i3))
            (adder 2
              (input i2)
              (input i3)))
           ((expanded_sum (i6 i7))
            (adder 6
              (input i6)
              (input i7)))
           ((expanded_sum (i4 i5))
            (adder 5
              (input i4)
              (input i5)))))
         (2 (
           ((expanded_sum (i0 i1 i2))
            (adder 3
              (adder 1)
              (input i2)))
           ((expanded_sum (i4 i5 i6 i7))
            (adder 8
              (adder 5)
              (adder 6)))
           ((expanded_sum (i4 i5 i6))
            (adder 7
              (adder 5)
              (input i6)))
           ((expanded_sum (i0 i1 i2 i3))
            (adder 4
              (adder 1)
              (adder 2)))))
         (3 (
           ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7))
            (adder 12
              (adder 4)
              (adder 8)))
           ((expanded_sum (i0 i1 i2 i3 i4))
            (adder 9
              (adder 4)
              (input i4)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5))
            (adder 10
              (adder 4)
              (adder 5)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5 i6))
            (adder 11
              (adder 4)
              (adder 7))))))))))
    ((config Brent_kung)
     (inputs (i0))
     (Ok ((outputs (((expanded_sum (i0)) (input i0)))) (add_by_depth ()))))
    ((config Brent_kung)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0)) ((expanded_sum (i0 i1)) (adder 1))))
       (add_by_depth ((
         1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1))))))))))
    ((config Brent_kung)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 4))
         ((expanded_sum (i0 i1 i2 i3)) (adder 3))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i2 i3))
            (adder 2
              (input i2)
              (input i3)))))
         (2 (
           ((expanded_sum (i0 i1 i2))
            (adder 4
              (adder 1)
              (input i2)))
           ((expanded_sum (i0 i1 i2 i3))
            (adder 3
              (adder 1)
              (adder 2))))))))))
    ((config Brent_kung)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 9))
         ((expanded_sum (i0 i1 i2 i3)) (adder 5))
         ((expanded_sum (i0 i1 i2 i3 i4)) (adder 10))
         ((expanded_sum (i0 i1 i2 i3 i4 i5)) (adder 8))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6)) (adder 11))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7)) (adder 7))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i4 i5))
            (adder 3
              (input i4)
              (input i5)))
           ((expanded_sum (i2 i3))
            (adder 2
              (input i2)
              (input i3)))
           ((expanded_sum (i6 i7))
            (adder 4
              (input i6)
              (input i7)))))
         (2 (
           ((expanded_sum (i0 i1 i2))
            (adder 9
              (adder 1)
              (input i2)))
           ((expanded_sum (i4 i5 i6 i7))
            (adder 6
              (adder 3)
              (adder 4)))
           ((expanded_sum (i0 i1 i2 i3))
            (adder 5
              (adder 1)
              (adder 2)))))
         (3 (
           ((expanded_sum (i0 i1 i2 i3 i4 i5))
            (adder 8
              (adder 5)
              (adder 3)))
           ((expanded_sum (i0 i1 i2 i3 i4))
            (adder 10
              (adder 5)
              (input i4)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7))
            (adder 7
              (adder 5)
              (adder 6)))))
         (4 ((
           (expanded_sum (i0 i1 i2 i3 i4 i5 i6))
           (adder 11
             (adder 8)
             (input i6))))))))))
    ((config Kogge_stone)
     (inputs (i0))
     (Ok ((outputs (((expanded_sum (i0)) (input i0)))) (add_by_depth ()))))
    ((config Kogge_stone)
     (inputs (i0 i1))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0)) ((expanded_sum (i0 i1)) (adder 1))))
       (add_by_depth ((
         1 ((
           (expanded_sum (i0 i1))
           (adder 1
             (input i0)
             (input i1))))))))))
    ((config Kogge_stone)
     (inputs (i0 i1 i2 i3))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 4))
         ((expanded_sum (i0 i1 i2 i3)) (adder 5))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i2 i3))
            (adder 3
              (input i2)
              (input i3)))
           ((expanded_sum (i1 i2))
            (adder 2
              (input i1)
              (input i2)))))
         (2 (
           ((expanded_sum (i0 i1 i2))
            (adder 4
              (input i0)
              (adder 2)))
           ((expanded_sum (i0 i1 i2 i3))
            (adder 5
              (adder 1)
              (adder 3))))))))))
    ((config Kogge_stone)
     (inputs (i0 i1 i2 i3 i4 i5 i6 i7))
     (Ok (
       (outputs (
         ((expanded_sum (i0)) (input i0))
         ((expanded_sum (i0 i1)) (adder 1))
         ((expanded_sum (i0 i1 i2)) (adder 8))
         ((expanded_sum (i0 i1 i2 i3)) (adder 9))
         ((expanded_sum (i0 i1 i2 i3 i4)) (adder 14))
         ((expanded_sum (i0 i1 i2 i3 i4 i5)) (adder 15))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6)) (adder 16))
         ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7)) (adder 17))))
       (add_by_depth (
         (1 (
           ((expanded_sum (i2 i3))
            (adder 3
              (input i2)
              (input i3)))
           ((expanded_sum (i1 i2))
            (adder 2
              (input i1)
              (input i2)))
           ((expanded_sum (i5 i6))
            (adder 6
              (input i5)
              (input i6)))
           ((expanded_sum (i3 i4))
            (adder 4
              (input i3)
              (input i4)))
           ((expanded_sum (i0 i1))
            (adder 1
              (input i0)
              (input i1)))
           ((expanded_sum (i6 i7))
            (adder 7
              (input i6)
              (input i7)))
           ((expanded_sum (i4 i5))
            (adder 5
              (input i4)
              (input i5)))))
         (2 (
           ((expanded_sum (i0 i1 i2 i3))
            (adder 9
              (adder 1)
              (adder 3)))
           ((expanded_sum (i1 i2 i3 i4))
            (adder 10
              (adder 2)
              (adder 4)))
           ((expanded_sum (i2 i3 i4 i5))
            (adder 11
              (adder 3)
              (adder 5)))
           ((expanded_sum (i4 i5 i6 i7))
            (adder 13
              (adder 5)
              (adder 7)))
           ((expanded_sum (i0 i1 i2))
            (adder 8
              (input i0)
              (adder 2)))
           ((expanded_sum (i3 i4 i5 i6))
            (adder 12
              (adder 4)
              (adder 6)))))
         (3 (
           ((expanded_sum (i0 i1 i2 i3 i4))
            (adder 14
              (input i0)
              (adder 10)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5))
            (adder 15
              (adder 1)
              (adder 11)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5 i6 i7))
            (adder 17
              (adder 9)
              (adder 13)))
           ((expanded_sum (i0 i1 i2 i3 i4 i5 i6))
            (adder 16
              (adder 8)
              (adder 12)))))))))) |}];
;;
