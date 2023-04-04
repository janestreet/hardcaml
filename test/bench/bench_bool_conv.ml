open! Import
open! Bits
module Bigstring = Base_bigstring

let int64_of_bool_obj_magic b = Stdlib.Obj.magic b |> Int64.of_int
let int64_of_bool_branching b = if b then 1L else 0L
let test_values = [| 1L; 4L; 8L |]
let result = Bigstring.init 8 ~f:(fun _ -> '\000')

(* just about complex enough to stump the inliner from just evaluating the functions at
   compile time. *)
let test f =
  for i = 0 to Array.length test_values - 1 do
    for j = 0 to Array.length test_values - 1 do
      f test_values.(i) test_values.(j)
    done
  done
;;

let equal_obj_magic a b =
  Int64.equal a b |> int64_of_bool_obj_magic |> Bigstring.set_int64_t_le result ~pos:0
;;

let equal_branching a b =
  Int64.equal a b |> int64_of_bool_branching |> Bigstring.set_int64_t_le result ~pos:0
;;

let equal_bit_fiddling a b =
  lnot (Int64.compare a b) land 1
  |> Int64.of_int
  |> Bigstring.set_int64_t_le result ~pos:0
;;

let%bench_fun "equal - magic" = fun () -> test equal_obj_magic
let%bench_fun "equal - branching" = fun () -> test equal_branching
let%bench_fun "equal - bit_fiddling" = fun () -> test equal_bit_fiddling

let lt_obj_magic a b =
  Int64.( < ) a b |> int64_of_bool_obj_magic |> Bigstring.set_int64_t_le result ~pos:0
;;

let lt_branching a b =
  Int64.( < ) a b |> int64_of_bool_branching |> Bigstring.set_int64_t_le result ~pos:0
;;

let lt_bit_fiddling a b =
  (Int64.compare a b lsr 1) land 1
  |> Int64.of_int
  |> Bigstring.set_int64_t_le result ~pos:0
;;

let%bench_fun "lt - magic" = fun () -> test lt_obj_magic
let%bench_fun "lt - branching" = fun () -> test lt_branching
let%bench_fun "lt - bit_fiddling" = fun () -> test lt_bit_fiddling
