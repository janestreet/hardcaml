open! Import
open! Mul

module Signal = struct
  module Types = struct
    module rec Bit : sig
      type t =
        | And of t * t
        | Bit of { input : Bits.t; index : int }
        | Gnd
        | Not of t
        | Or of t * t
        | Xor of t * t
    end = Bit
    and Bits : sig
      type t =
        | Add of t * t
        | Concat of Bit.t list
        | Input of { name : string; width : int }
        | Uresize of { input : t; width : int }
    end = Bits
  end

  open Types
  open Bit
  open Bits

  type t = Bits.t
  type bit = Bit.t

  let rec sexp_of_t (t : t) =
    let binary op t1 t2 = [%message "" ~_:(t1 : t) op ~_:(t2 : t)] in
    match t with
    | Add (t1, t2) -> binary "+:" t1 t2
    | Concat bits -> [%message "concat" ~_:(bits : bit list)]
    | Input { name ; width = _ } -> [%message name]
    | Uresize { input; width } -> [%message "uresize" ~_:(width : int) ~_:(input : t)]
  and sexp_of_bit (bit : Bit.t) =
    let binary op t1 t2 = [%message "" ~_:(t1 : bit) op ~_:(t2 : bit)] in
    match bit with
    | And (t1, t2) -> binary "&:" t1 t2
    | Bit { input = Input { name; _ }; index } ->
      [%message "" ~_:(concat [ name; index |> Int.to_string ])]
    | Bit { input; index } -> [%message "bit" ~_:(index : int) ~_:(input : t)]
    | Gnd -> [%message "gnd"]
    | Not t -> [%message "~:" (t : bit)]
    | Or (t1, t2) -> binary "|:" t1 t2
    | Xor (t1, t2) -> binary "^:" t1 t2

  let rec width = function
    | Add (t, _) -> width t
    | Concat bits -> List.length bits
    | Input t -> t.width
    | Uresize t -> t.width

  let gnd = Gnd

  let ( +: ) t1 t2 = Add (t1, t2)
  let ( &: ) t1 t2 = And (t1, t2)
  let ( |: ) t1 t2 = Or (t1, t2)
  let ( ^: ) t1 t2 = Xor (t1, t2)
  let ( ~: ) t = Not t

  let concat bits = Concat bits

  let bit input index = Bit { input; index }

  let uresize input to_width =
    if width input = to_width
    then input
    else Uresize { input; width = to_width }

  let bits t =
    let w = width t in
    List.init w ~f:(fun i -> Bit { input = t; index = w - i - 1 })
end

let%expect_test _ =
  for a_width = 1 to 4 do
    for b_width = a_width to 4 do
      List.iter Config.all ~f:(fun config ->
        print_s [%message
          ""
            (config : Config.t)
            (a_width : int)
            (b_width : int)
            ~result:(
              create_gen ~config (module Signal)
                (Input { name = "a"; width = a_width })
                (Input { name = "b"; width = b_width })
              : Signal.t)]);
    done;
  done;
  [%expect {|
    ((config  Dadda)
     (a_width 1)
     (b_width 1)
     (result ((uresize 2 (concat ((a0 &: b0)))) +: (uresize 2 (concat (gnd))))))
    ((config  Wallace)
     (a_width 1)
     (b_width 1)
     (result ((uresize 2 (concat ((a0 &: b0)))) +: (uresize 2 (concat (gnd))))))
    ((config  Dadda)
     (a_width 1)
     (b_width 2)
     (result (
       (uresize 3 (
         concat (
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 3 (concat (gnd gnd))))))
    ((config  Wallace)
     (a_width 1)
     (b_width 2)
     (result (
       (uresize 3 (
         concat (
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 3 (concat (gnd gnd))))))
    ((config  Dadda)
     (a_width 1)
     (b_width 3)
     (result (
       (uresize 4 (
         concat (
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 4 (concat (gnd gnd gnd))))))
    ((config  Wallace)
     (a_width 1)
     (b_width 3)
     (result (
       (uresize 4 (
         concat (
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 4 (concat (gnd gnd gnd))))))
    ((config  Dadda)
     (a_width 1)
     (b_width 4)
     (result (
       (uresize 5 (
         concat (
           (a0 &: b3)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 5 (concat (gnd gnd gnd gnd))))))
    ((config  Wallace)
     (a_width 1)
     (b_width 4)
     (result (
       (uresize 5 (
         concat (
           (a0 &: b3)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 5 (concat (gnd gnd gnd gnd))))))
    ((config  Dadda)
     (a_width 2)
     (b_width 2)
     (result (
       (uresize 4 (
         concat (
           (a1 &: b1)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 4 (concat (gnd (a1 &: b0) gnd))))))
    ((config  Wallace)
     (a_width 2)
     (b_width 2)
     (result (
       (uresize 4 (
         concat (
           (a1 &: b1)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 4 (concat (gnd (a1 &: b0) gnd))))))
    ((config  Dadda)
     (a_width 2)
     (b_width 3)
     (result (
       (uresize 5 (
         concat (
           (a1 &: b2)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 5 (
         concat (
           gnd
           (a1 &: b1)
           (a1 &: b0)
           gnd))))))
    ((config  Wallace)
     (a_width 2)
     (b_width 3)
     (result (
       (uresize 5 (
         concat (
           (a1 &: b2)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 5 (
         concat (
           gnd
           (a1 &: b1)
           (a1 &: b0)
           gnd))))))
    ((config  Dadda)
     (a_width 2)
     (b_width 4)
     (result (
       (uresize 6 (
         concat (
           (a1 &: b3)
           (a0 &: b3)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 6 (
         concat (
           gnd
           (a1 &: b2)
           (a1 &: b1)
           (a1 &: b0)
           gnd))))))
    ((config  Wallace)
     (a_width 2)
     (b_width 4)
     (result (
       (uresize 6 (
         concat (
           (a1 &: b3)
           (a0 &: b3)
           (a0 &: b2)
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 6 (
         concat (
           gnd
           (a1 &: b2)
           (a1 &: b1)
           (a1 &: b0)
           gnd))))))
    ((config  Dadda)
     (a_width 3)
     (b_width 3)
     (result (
       (concat (
         gnd
         (a2 &: b2)
         ((a1 &: b2) ^: (a2 &: b1))
         ((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
         (a1 &: b0)
         (a0 &: b0)))
       +:
       (concat (
         gnd
         ((a1 &: b2) &: (a2 &: b1))
         ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
          |:
          ((a1 &: b1) &: (a2 &: b0)))
         gnd
         (a0 &: b1)
         gnd)))))
    ((config  Wallace)
     (a_width 3)
     (b_width 3)
     (result (
       (concat (
         gnd
         (a2 &: b2)
         ((a1 &: b2) ^: (a2 &: b1))
         ((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
         ((a0 &: b1) ^: (a1 &: b0))
         (a0 &: b0)))
       +:
       (concat (
         gnd
         ((a1 &: b2) &: (a2 &: b1))
         ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
          |:
          ((a1 &: b1) &: (a2 &: b0)))
         ((a0 &: b1) &: (a1 &: b0))
         gnd
         gnd)))))
    ((config  Dadda)
     (a_width 3)
     (b_width 4)
     (result (
       (concat (
         gnd
         (a2 &: b3)
         ((a1 &: b3) ^: (a2 &: b2))
         ((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
         ((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
         (a1 &: b0)
         (a0 &: b0)))
       +:
       (concat (
         gnd
         ((a1 &: b3) &: (a2 &: b2))
         ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
          |:
          ((a1 &: b2) &: (a2 &: b1)))
         ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
          |:
          ((a1 &: b1) &: (a2 &: b0)))
         gnd
         (a0 &: b1)
         gnd)))))
    ((config  Wallace)
     (a_width 3)
     (b_width 4)
     (result (
       (concat (
         gnd
         (a2 &: b3)
         ((a1 &: b3) ^: (a2 &: b2))
         ((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
         ((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
         ((a0 &: b1) ^: (a1 &: b0))
         (a0 &: b0)))
       +:
       (concat (
         gnd
         ((a1 &: b3) &: (a2 &: b2))
         ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
          |:
          ((a1 &: b2) &: (a2 &: b1)))
         ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
          |:
          ((a1 &: b1) &: (a2 &: b0)))
         ((a0 &: b1) &: (a1 &: b0))
         gnd
         gnd)))))
    ((config  Dadda)
     (a_width 4)
     (b_width 4)
     (result (
       (uresize 8 (
         concat (
           gnd gnd
           (a3 &: b3)
           ((a3 &: b2)
            ^:
            ((a2 &: b3)
             ^:
             ((((a1 &: b3) &: (a2 &: b2)) |: ((a1 &: b3) &: (a3 &: b1)))
              |:
              ((a2 &: b2) &: (a3 &: b1)))))
           (((a1 &: b3) ^: ((a2 &: b2) ^: (a3 &: b1)))
            ^:
            ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
             |:
             ((a1 &: b2) &: (a2 &: b1))))
           ((a3 &: b0)
            ^:
            (((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
             ^:
             ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
              |:
              ((a1 &: b1) &: (a2 &: b0)))))
           ((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
           (a0 &: b1)
           (a0 &: b0))))
       +:
       (uresize 8 (
         concat (
           gnd gnd
           ((((a3 &: b2) &: (a2 &: b3))
             |:
             ((a3 &: b2)
              &:
              ((((a1 &: b3) &: (a2 &: b2)) |: ((a1 &: b3) &: (a3 &: b1)))
               |:
               ((a2 &: b2) &: (a3 &: b1)))))
            |:
            ((a2 &: b3)
             &:
             ((((a1 &: b3) &: (a2 &: b2)) |: ((a1 &: b3) &: (a3 &: b1)))
              |:
              ((a2 &: b2) &: (a3 &: b1)))))
           (((a1 &: b3) ^: ((a2 &: b2) ^: (a3 &: b1)))
            &:
            ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
             |:
             ((a1 &: b2) &: (a2 &: b1))))
           ((((a3 &: b0) &: ((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1))))
             |:
             ((a3 &: b0)
              &:
              ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
               |:
               ((a1 &: b1) &: (a2 &: b0)))))
            |:
            (((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
             &:
             ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
              |:
              ((a1 &: b1) &: (a2 &: b0)))))
           gnd
           gnd
           (a1 &: b0)
           gnd))))))
    ((config  Wallace)
     (a_width 4)
     (b_width 4)
     (result (
       (uresize 8 (
         concat (
           gnd
           ((a3 &: b3) &: ((a2 &: b3) &: (a3 &: b2)))
           ((a3 &: b3) ^: ((a2 &: b3) &: (a3 &: b2)))
           (((a2 &: b3) ^: (a3 &: b2))
            ^:
            ((((a1 &: b3) &: (a2 &: b2)) |: ((a1 &: b3) &: (a3 &: b1)))
             |:
             ((a2 &: b2) &: (a3 &: b1))))
           (((a1 &: b3) ^: ((a2 &: b2) ^: (a3 &: b1)))
            ^:
            ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
             |:
             ((a1 &: b2) &: (a2 &: b1))))
           ((a3 &: b0)
            ^:
            (((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
             ^:
             ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
              |:
              ((a1 &: b1) &: (a2 &: b0)))))
           (((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
            ^:
            ((a0 &: b1) &: (a1 &: b0)))
           ((a0 &: b1) ^: (a1 &: b0))
           (a0 &: b0))))
       +:
       (uresize 8 (
         concat (
           gnd gnd
           (((a2 &: b3) ^: (a3 &: b2))
            &:
            ((((a1 &: b3) &: (a2 &: b2)) |: ((a1 &: b3) &: (a3 &: b1)))
             |:
             ((a2 &: b2) &: (a3 &: b1))))
           (((a1 &: b3) ^: ((a2 &: b2) ^: (a3 &: b1)))
            &:
            ((((a0 &: b3) &: (a1 &: b2)) |: ((a0 &: b3) &: (a2 &: b1)))
             |:
             ((a1 &: b2) &: (a2 &: b1))))
           ((((a3 &: b0) &: ((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1))))
             |:
             ((a3 &: b0)
              &:
              ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
               |:
               ((a1 &: b1) &: (a2 &: b0)))))
            |:
            (((a0 &: b3) ^: ((a1 &: b2) ^: (a2 &: b1)))
             &:
             ((((a0 &: b2) &: (a1 &: b1)) |: ((a0 &: b2) &: (a2 &: b0)))
              |:
              ((a1 &: b1) &: (a2 &: b0)))))
           (((a0 &: b2) ^: ((a1 &: b1) ^: (a2 &: b0)))
            &:
            ((a0 &: b1) &: (a1 &: b0)))
           gnd
           gnd
           gnd)))))) |}]
;;
