open Base
open Cyclesim0

module Combine_error = struct
  type t =
    { cycle_no : int
    ; clock_edge : Side.t
    ; port_name : string
    ; value0 : Bits.t
    ; value1 : Bits.t
    }
  [@@deriving sexp_of]
end

let default_on_error error =
  raise_s
    [%message "[Cyclesim.combine] output port values differ" (error : Combine_error.t)]
;;

let combine
  ?(port_sets_may_differ = false)
  ?(on_error = default_on_error)
  (s0 : _ t)
  (s1 : _ t)
  =
  let si =
    Set.to_list
      (List.fold
         (s0.in_ports @ s1.in_ports)
         ~init:(Set.empty (module String))
         ~f:(fun s (n, _) -> Set.add s n))
  in
  let out_port_set =
    Set.to_list
      (List.fold
         (s0.out_ports_before_clock_edge @ s1.out_ports_before_clock_edge)
         ~init:(Set.empty (module String))
         ~f:(fun s (n, _) -> Set.add s n))
  in
  let try_find n s =
    try Some (List.Assoc.find_exn s ~equal:String.equal n) with
    | _ -> None
  in
  let _, copy_in_ports =
    let l =
      List.map si ~f:(fun n ->
        match try_find n s0.in_ports, try_find n s1.in_ports with
        | Some x, Some y -> (n, x), fun () -> y := !x
        | Some x, None when port_sets_may_differ -> (n, x), fun () -> ()
        | None, Some y when port_sets_may_differ -> (n, y), fun () -> ()
        | _ -> raise_s [%message "Input port was not found" ~name:(n : string)])
    in
    List.map l ~f:fst, List.map l ~f:snd
  in
  let cycle_no = ref 0 in
  let check edge n x y =
    if not (Bits.equal !x !y)
    then
      on_error
        { Combine_error.cycle_no = !cycle_no
        ; clock_edge = edge
        ; port_name = n
        ; value0 = !x
        ; value1 = !y
        }
  in
  let associate_and_check_out_ports edge out_ports =
    let l =
      List.map out_port_set ~f:(fun n ->
        match try_find n (out_ports s0), try_find n (out_ports s1) with
        | Some x, Some y -> (n, x), fun () -> check edge n x y
        | Some x, None when port_sets_may_differ -> (n, x), fun () -> ()
        | None, Some y when port_sets_may_differ -> (n, y), fun () -> ()
        | _ -> raise_s [%message "Output port was not found" ~name:(n : string)])
    in
    List.map l ~f:fst, List.map l ~f:snd
  in
  let _, check_out_ports_after_clock_edge =
    associate_and_check_out_ports After Cyclesim0.out_ports_after_clock_edge
  and _, check_out_ports_before_clock_edge =
    associate_and_check_out_ports Before Cyclesim0.out_ports_before_clock_edge
  in
  let copy_in_ports () = List.iter copy_in_ports ~f:(fun f -> f ()) in
  let check_out_ports () =
    List.iter check_out_ports_before_clock_edge ~f:(fun f -> f ());
    List.iter check_out_ports_after_clock_edge ~f:(fun f -> f ())
  in
  let incr_cycle () = Int.incr cycle_no in
  let cycle_check () =
    s0.cycle_check ();
    copy_in_ports ();
    s1.cycle_check ()
  in
  let cycle_before_clock_edge () =
    s0.cycle_before_clock_edge ();
    s1.cycle_before_clock_edge ()
  in
  let cycle_at_clock_edge () =
    s0.cycle_at_clock_edge ();
    s1.cycle_at_clock_edge ()
  in
  let cycle_after_clock_edge () =
    s0.cycle_after_clock_edge ();
    s1.cycle_after_clock_edge ();
    check_out_ports ();
    incr_cycle ()
  in
  let reset () =
    s0.reset ();
    s1.reset ()
  in
  { s0 with
    reset
  ; cycle_check
  ; cycle_before_clock_edge
  ; cycle_at_clock_edge
  ; cycle_after_clock_edge
  }
;;
