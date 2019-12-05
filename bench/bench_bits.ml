open! Import
open! Bits

let%bench_fun "[floor_log2]" =
  let t = of_bit_string "1" in
  fun () -> floor_log2 t
;;

(* {[
     let%bench_fun "[floor_log2_int]" =
       let t = of_bit_string "1" in
       fun () -> floor_log2_int t
     ;;
   ]} *)
