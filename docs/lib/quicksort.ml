open Base
open Hardcaml

module Functional = struct
  (* Functional quicksort - Upto O(N^2) memory use *)

  (* $MDX part-begin=functional *)
  let rec qsort a =
    match a with
    | [] -> []
    | x :: xs ->
      let left = List.filter xs ~f:(fun y -> y <= x) in
      let right = List.filter xs ~f:(fun y -> y > x) in
      List.concat [ qsort left; [ x ]; qsort right ]
  ;;

  (* $MDX part-end *)

  let qsort a =
    let b = qsort (Array.to_list a) in
    List.iteri b ~f:(fun i v -> a.(i) <- v)
  ;;
end

module Lomuto_in_place = struct
  (* In place quicksort - upto O(N) stack space *)

  (* $MDX part-begin=lomuto *)
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  ;;

  let partition a ~low ~high =
    let pivot = a.(high) in
    let i = ref low in
    for j = low to high - 1 do
      if a.(j) < pivot
      then (
        swap a !i j;
        Int.incr i)
    done;
    swap a !i high;
    !i
  ;;

  let rec qsort a ~low ~high =
    if not (low >= high || low < 0)
    then (
      let p = partition a ~high ~low in
      qsort a ~low ~high:(p - 1);
      qsort a ~low:(p + 1) ~high)
  ;;

  let qsort a = qsort a ~low:0 ~high:(Array.length a - 1)

  (* $MDX part-end *)
end

module Sedgewick_log_stack = struct
  (* O(Log N) stack space *)

  let partition = Lomuto_in_place.partition

  (* $MDX part-begin=sedgewick *)
  let rec qsort a ~low ~high =
    let low, high = ref low, ref high in
    while !low < !high do
      let pivot = partition a ~low:!low ~high:!high in
      if pivot - !low < !high - pivot
      then (
        qsort a ~low:!low ~high:(pivot - 1);
        low := pivot + 1)
      else (
        qsort a ~low:(pivot + 1) ~high:!high;
        high := pivot - 1)
    done
  ;;

  let qsort a = qsort a ~low:0 ~high:(Array.length a - 1)
  (* $MDX part-end *)
end

module%test Debug = struct
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  ;;

  let partition a ~low ~high =
    Stdio.print_s
      [%message
        "partition"
          (low : int)
          (high : int)
          ~_:(Array.sub a ~pos:low ~len:(high - low + 1) : int array)];
    let pivot = a.(high) in
    let i = ref low in
    for j = low to high - 1 do
      if a.(j) < pivot
      then (
        swap a !i j;
        Int.incr i)
    done;
    swap a !i high;
    Stdio.print_s
      [%message
        "-->" (!i : int) ~_:(Array.sub a ~pos:low ~len:(high - low + 1) : int array)];
    !i
  ;;

  let rec qsort a ~low ~high =
    Stdio.print_s [%message "REC" (low : int) (high : int)];
    let low, high = ref low, ref high in
    while !low < !high do
      let pivot = partition a ~low:!low ~high:!high in
      if pivot - !low < !high - pivot
      then (
        qsort a ~low:!low ~high:(pivot - 1);
        low := pivot + 1)
      else (
        qsort a ~low:(pivot + 1) ~high:!high;
        high := pivot - 1);
      Stdio.print_s [%message "NXT" (!low : int) (!high : int)]
    done
  ;;

  let qsort a = qsort a ~low:0 ~high:(Array.length a - 1)

  let%expect_test "" =
    let a = [| 9; 4; 7; 6; 5; 5; 0; 3; 1; 4; 8; 6; 4; 0; 2; 3 |] in
    qsort a;
    [%expect
      {|
      (REC (low 0) (high 15))
      (partition (low 0) (high 15) (9 4 7 6 5 5 0 3 1 4 8 6 4 0 2 3))
      (--> (!i 4) (0 1 0 2 3 5 9 3 4 4 8 6 4 7 6 5))
      (REC (low 0) (high 3))
      (partition (low 0) (high 3) (0 1 0 2))
      (--> (!i 3) (0 1 0 2))
      (REC (low 4) (high 3))
      (NXT (!low 0) (!high 2))
      (partition (low 0) (high 2) (0 1 0))
      (--> (!i 0) (0 1 0))
      (REC (low 0) (high -1))
      (NXT (!low 1) (!high 2))
      (partition (low 1) (high 2) (1 0))
      (--> (!i 1) (0 1))
      (REC (low 1) (high 0))
      (NXT (!low 2) (!high 2))
      (NXT (!low 5) (!high 15))
      (partition (low 5) (high 15) (5 9 3 4 4 8 6 4 7 6 5))
      (--> (!i 9) (3 4 4 4 5 8 6 9 7 6 5))
      (REC (low 5) (high 8))
      (partition (low 5) (high 8) (3 4 4 4))
      (--> (!i 6) (3 4 4 4))
      (REC (low 5) (high 5))
      (NXT (!low 7) (!high 8))
      (partition (low 7) (high 8) (4 4))
      (--> (!i 7) (4 4))
      (REC (low 7) (high 6))
      (NXT (!low 8) (!high 8))
      (NXT (!low 10) (!high 15))
      (partition (low 10) (high 15) (8 6 9 7 6 5))
      (--> (!i 10) (5 6 9 7 6 8))
      (REC (low 10) (high 9))
      (NXT (!low 11) (!high 15))
      (partition (low 11) (high 15) (6 9 7 6 8))
      (--> (!i 14) (6 7 6 8 9))
      (REC (low 15) (high 15))
      (NXT (!low 11) (!high 13))
      (partition (low 11) (high 13) (6 7 6))
      (--> (!i 11) (6 7 6))
      (REC (low 11) (high 10))
      (NXT (!low 12) (!high 13))
      (partition (low 12) (high 13) (7 6))
      (--> (!i 12) (6 7))
      (REC (low 12) (high 11))
      (NXT (!low 13) (!high 13))
      |}]
  ;;
end

module Explicit_stack = struct
  (* Convert to stack *)

  (* $MDX part-begin=call_stack_type *)
  module type Call_stack = sig
    (* Each entry on the stack contains the high and low indices of the current partition,
       and the pivot point. *)
    type t

    (* Create an empty stack *)
    val create : unit -> t

    (* Push a new entry to the stack. pivot is initially [0]. *)
    val push : t -> low:int -> high:int -> unit

    (* Pop the top entry of the stack. *)
    val pop : t -> unit

    (* Modify the top entry of the stack. *)
    val set_low : t -> int -> unit
    val set_high : t -> int -> unit
    val set_pivot : t -> int -> unit

    (* Read the top entry of the stack. *)
    val low : t -> int
    val high : t -> int
    val pivot : t -> int
  end
  (* $MDX part-end *)

  module Call_stack : Call_stack = struct
    type e =
      { mutable low : int
      ; mutable high : int
      ; mutable pivot : int
      }

    type t = e Stack.t

    let create () = Stack.create ()
    let push stack ~low ~high = Stack.push stack { low; high; pivot = 0 }
    let pop stack = ignore (Stack.pop_exn stack : e)
    let set_low stack low = (Stack.top_exn stack).low <- low
    let set_high stack high = (Stack.top_exn stack).high <- high
    let set_pivot stack pivot = (Stack.top_exn stack).pivot <- pivot
    let low stack = (Stack.top_exn stack).low
    let high stack = (Stack.top_exn stack).high
    let pivot stack = (Stack.top_exn stack).pivot
  end

  let partition = Lomuto_in_place.partition

  (* $MDX part-begin=explicit_stack *)
  let rec qsort a stack =
    let open Call_stack in
    while low stack < high stack do
      set_pivot stack (partition a ~low:(low stack) ~high:(high stack));
      if pivot stack - low stack < high stack - pivot stack
      then (
        push stack ~low:(low stack) ~high:(pivot stack - 1);
        qsort a stack;
        set_low stack (pivot stack + 1))
      else (
        push stack ~low:(pivot stack + 1) ~high:(high stack);
        qsort a stack;
        set_high stack (pivot stack - 1))
    done;
    pop stack
  ;;

  let qsort a =
    let stack = Call_stack.create () in
    (* push the initial stack entry *)
    Call_stack.push stack ~low:0 ~high:(Array.length a - 1);
    qsort a stack
  ;;
  (* $MDX part-end *)
end

let%expect_test "test software implementations" =
  let call f a =
    let b = Array.copy a in
    f b;
    b
  in
  let ref_sort a = Array.sort a ~compare:Int.compare in
  for _ = 0 to 100 do
    let len = 1 + Random.int 20 in
    let a = Array.init len ~f:(fun _ -> Random.int 100) in
    let r = call ref_sort a in
    let q = call Lomuto_in_place.qsort a in
    let q' = call Functional.qsort a in
    let q'' = call Sedgewick_log_stack.qsort a in
    let q''' = call Explicit_stack.qsort a in
    if Array.compare Int.compare r q <> 0
       || Array.compare Int.compare r q' <> 0
       || Array.compare Int.compare r q'' <> 0
       || Array.compare Int.compare r q''' <> 0
    then
      raise_s
        [%message
          (a : int array)
            (r : int array)
            (q : int array)
            (q' : int array)
            (q'' : int array)
            (q''' : int array)]
  done
;;

open Signal
module Clocking = Clocking

(* $MDX part-begin=config *)
module type Config = sig
  val log_size : int
  val data_size : int
end
(* $MDX part-end *)

module Make (Config : Config) = struct
  open Config

  (* $MDX part-begin=partition *)
  module Partition = struct
    type 'a t =
      { low : 'a [@bits log_size]
      ; high : 'a [@bits log_size]
      ; pivot : 'a [@bits log_size]
      }
    [@@deriving hardcaml]
  end

  module Partition_with_valids = With_valid.Fields.Make (Partition)
  (* $MDX part-end *)

  (* $MDX part-begin=callstack *)
  module Call_stack = struct
    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; partition : 'a Partition_with_valids.t
        ; push : 'a
        ; pop : 'a
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    module O = struct
      type 'a t =
        { partition : 'a Partition.t
        ; is_empty : 'a
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    let create scope (i : _ I.t) : _ O.t =
      let addr_bits = Int.ceil_log2 (log_size + 1) in
      let%hw top = wire addr_bits in
      let top_next = top +:. 1 in
      let top_prev = top -:. 1 in
      top
      <-- Clocking.reg
            i.clocking
            ~enable:(i.push |: i.pop)
            (mux2 i.push top_next top_prev);
      let create_stack { With_valid.valid; value } =
        (multiport_memory
           (log_size + 1)
           ~write_ports:
             [| { write_clock = i.clocking.clock
                ; write_enable = i.push |: valid
                ; write_address = mux2 i.push top_next top
                ; write_data = value
                }
             |]
           ~read_addresses:[| top |]).(0)
      in
      { O.partition = Partition.map i.partition ~f:create_stack; is_empty = top ==:. 0 }
    ;;
  end
  (* $MDX part-end *)

  (* $MDX part-begin=interface *)
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; read_data : 'a [@bits data_size]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; write_enable : 'a
      ; write_address : 'a [@bits log_size]
      ; write_data : 'a [@bits data_size]
      ; read_address : 'a [@bits log_size]
      ; read_enable : 'a
      }
    [@@deriving hardcaml]
  end
  (* $MDX part-end *)

  (* $MDX part-begin=states *)
  module State = struct
    type t =
      | Start
      | Qsort
      | Pivot
      | Partition
      | Swap
      | Swap_pivot
      | Update_range
      | Recurse
    [@@deriving sexp_of, compare ~localize, enumerate]
  end
  (* $MDX part-end *)

  (* $MDX part-begin=core *)
  let create scope (i : _ I.t) =
    let%hw.Always.State_machine sm =
      Always.State_machine.create (module State) (Clocking.to_spec i.clocking)
    in
    let%hw.Partition_with_valids.Of_always write_partition =
      Partition_with_valids.Of_always.wire zero
    in
    let%hw_var push = Always.Variable.wire ~default:gnd () in
    let%hw_var pop = Always.Variable.wire ~default:gnd () in
    let%hw.Call_stack.O.Of_signal stack =
      Call_stack.create
        scope
        { Call_stack.I.clocking = i.clocking
        ; partition = Partition_with_valids.Of_always.value write_partition
        ; push = push.value
        ; pop = pop.value
        }
    in
    let read_address = Always.Variable.wire ~default:(zero log_size) () in
    let write_address = Always.Variable.wire ~default:(zero log_size) () in
    let write_enable = Always.Variable.wire ~default:gnd () in
    let write_data = Always.Variable.wire ~default:(zero data_size) () in
    let%hw_var i_idx = Clocking.Var.reg i.clocking ~width:log_size in
    let%hw_var j_idx = Clocking.Var.reg i.clocking ~width:log_size in
    let%hw_var j_idx_prev = Clocking.Var.reg i.clocking ~width:log_size in
    let%hw_var pivot_at = Clocking.Var.reg i.clocking ~width:log_size in
    let%hw_var tmp = Clocking.Var.reg i.clocking ~width:log_size in
    let init_j_idx =
      Always.(
        proc [ j_idx <-- stack.partition.low +:. 1; j_idx_prev <-- stack.partition.low ])
    in
    let incr_j_idx =
      Always.(proc [ j_idx <-- j_idx.value +:. 1; j_idx_prev <-- j_idx.value ])
    in
    let swap_write address data =
      Always.(
        proc [ write_address <-- address; write_data <-- data; write_enable <-- vdd ])
    in
    let partition_search_update raddr =
      Always.(
        proc
          [ read_address <-- j_idx.value
          ; incr_j_idx
          ; when_
              (j_idx_prev.value ==: stack.partition.high)
              [ read_address <-- raddr
              ; swap_write i_idx.value pivot_at.value
              ; sm.set_next Swap_pivot
              ]
          ])
    in
    let left_partition_is_smaller =
      stack.partition.pivot -: stack.partition.low
      <: stack.partition.high -: stack.partition.pivot
    in
    let left_partition_is_smaller_reg = Clocking.Var.reg i.clocking ~width:1 in
    let read_enable = Clocking.Var.reg i.clocking ~width:1 in
    Always.(
      compile
        [ proc
            (Partition.map write_partition ~f:(fun { valid; value = _ } -> valid <-- gnd)
             |> Partition.to_list)
        ; pop <-- gnd
        ; sm.switch
            [ ( Start
              , [ when_
                    i.start
                    [ Partition_with_valids.Of_always.assign
                        write_partition
                        { low = { valid = vdd; value = zero log_size }
                        ; high = { valid = vdd; value = ones log_size }
                        ; pivot = { valid = vdd; value = zero log_size }
                        }
                    ; read_enable <-- vdd
                    ; sm.set_next Qsort
                    ]
                ] )
            ; ( Qsort
              , [ if_
                    (stack.partition.low <: stack.partition.high)
                    [ i_idx <-- stack.partition.low
                    ; init_j_idx
                    ; read_address <-- stack.partition.high
                    ; sm.set_next Pivot
                    ]
                    [ if_
                        stack.is_empty
                        [ read_enable <-- gnd; sm.set_next Start ]
                        [ pop <-- vdd ]
                    ]
                ] )
            ; ( Pivot
              , [ read_address <-- stack.partition.low
                ; pivot_at <-- i.read_data
                ; sm.set_next Partition
                ] )
            ; ( Partition
              , [ if_
                    (i.read_data <: pivot_at.value)
                    [ swap_write i_idx.value i.read_data
                    ; read_address <-- i_idx.value
                    ; i_idx <-- i_idx.value +:. 1
                    ; sm.set_next Swap
                    ]
                    [ partition_search_update i_idx.value ]
                ] )
            ; ( Swap
              , [ swap_write j_idx_prev.value i.read_data
                ; read_address <-- j_idx.value
                ; sm.set_next Partition
                ; partition_search_update i_idx.value
                ] )
            ; ( Swap_pivot
              , [ swap_write stack.partition.high i.read_data
                ; write_partition.pivot.value <-- i_idx.value
                ; write_partition.pivot.valid <-- vdd
                ; sm.set_next Update_range
                ] )
            ; ( Update_range
              , [ left_partition_is_smaller_reg <-- left_partition_is_smaller
                ; if_
                    left_partition_is_smaller
                    [ tmp <-- stack.partition.low
                    ; write_partition.low.value <-- stack.partition.pivot +:. 1
                    ; write_partition.low.valid <-- vdd
                    ]
                    [ tmp <-- stack.partition.high
                    ; write_partition.high.value <-- stack.partition.pivot -:. 1
                    ; write_partition.high.valid <-- vdd
                    ]
                ; sm.set_next Recurse
                ] )
            ; ( Recurse
              , [ if_
                    left_partition_is_smaller_reg.value
                    [ write_partition.low.value <-- tmp.value
                    ; write_partition.high.value <-- stack.partition.pivot -:. 1
                    ]
                    [ write_partition.low.value <-- stack.partition.pivot +:. 1
                    ; write_partition.high.value <-- tmp.value
                    ]
                ; push
                  <-- mux2
                        left_partition_is_smaller_reg.value
                        (stack.partition.pivot <>:. 0)
                        (stack.partition.pivot <>+. -1)
                ; sm.set_next Qsort
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; write_enable = write_enable.value
    ; write_address = write_address.value
    ; write_data = write_data.value
    ; read_address = read_address.value
    ; read_enable = read_enable.value
    }
  ;;
  (* $MDX part-end *)
end

(* $MDX part-begin=toplevel *)

module Make_with_memory (Config : Config) = struct
  open Config
  module Qsort = Make (Config)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; write_enable : 'a
      ; write_address : 'a [@bits log_size]
      ; write_data : 'a [@bits data_size]
      ; read_address : 'a [@bits log_size]
      ; read_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; data_out : 'a [@bits data_size]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let%hw.Qsort.O.Of_signal qsort = Qsort.O.Of_signal.wires () in
    let%hw ram =
      (Ram.create
         ~name:"my_ram"
         ~collision_mode:Read_before_write
         ~size:(Int.pow 2 log_size)
         ~write_ports:
           [| { write_clock = i.clocking.clock
              ; write_enable = i.write_enable |: qsort.write_enable
              ; write_address = mux2 i.write_enable i.write_address qsort.write_address
              ; write_data = mux2 i.write_enable i.write_data qsort.write_data
              }
           |]
         ~read_ports:
           [| { read_clock = i.clocking.clock
              ; read_enable = i.read_enable |: qsort.read_enable
              ; read_address = mux2 i.read_enable i.read_address qsort.read_address
              }
           |]
         ()).(0)
    in
    Qsort.O.Of_signal.assign
      qsort
      (Qsort.create
         scope
         { Qsort.I.clocking = i.clocking; start = i.start; read_data = ram });
    { O.done_ = qsort.done_; data_out = ram }
  ;;
end
(* $MDX part-end *)

module Config = struct
  let log_size = 4
  let data_size = 4
end

module Qsort = Make_with_memory (Config)
module Sim = Cyclesim.With_interface (Qsort.I) (Qsort.O)

(* let () = Caller_id.set_mode Top_of_stack *)
open Bits

let display_rules =
  let open Hardcaml_waveterm in
  let bit name = Display_rule.port_name_is name ~wave_format:Bit in
  let uint name = Display_rule.port_name_is name ~wave_format:Unsigned_int in
  [ [ bit "clock"; bit "clear"; bit "start"; bit "done_" ]
  ; [ uint "i_idx"
    ; uint "j_idx"
    ; uint "j_idx_prev"
    ; uint "pivot_at"
    ; uint "qsort$read_address"
    ; uint "ram"
    ]
  ; [ Display_rule.port_name_is "sm" ]
  ; [ uint "stack$low"
    ; uint "stack$pivot"
    ; uint "stack$high"
    ; uint "top"
    ; bit "push"
    ; bit "pop"
    ; bit "stack$is_empty"
    ]
  ; [ uint "qsort$write_address"; uint "qsort$write_data"; bit "qsort$write_enable" ]
  ; [ uint "write_partition$low$value"
    ; bit "write_partition$low$valid"
    ; uint "write_partition$pivot$value"
    ; bit "write_partition$pivot$valid"
    ; uint "write_partition$high$value"
    ; bit "write_partition$high$valid"
    ]
  ]
  |> List.concat
;;

let clear_core (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := gnd
;;

let load_data (sim : Sim.t) data =
  let inputs = Cyclesim.inputs sim in
  inputs.write_enable := vdd;
  for i = 0 to Int.pow 2 Config.log_size - 1 do
    inputs.write_address <--. i;
    inputs.write_data <--. data.(i);
    Cyclesim.cycle sim
  done;
  inputs.write_enable := gnd
;;

(* $MDX part-begin=debug *)
module Debug = struct
  type t =
    { ram : Cyclesim.Memory.t
    ; low : Cyclesim.Node.t
    ; pivot : Cyclesim.Node.t
    ; high : Cyclesim.Node.t
    ; sm : Cyclesim.Reg.t
    ; pivot_state : int
    }

  let create sim =
    let ram = Cyclesim.lookup_mem_by_name sim "my_ram" |> Option.value_exn in
    let low = Cyclesim.lookup_node_by_name sim "stack$low" |> Option.value_exn in
    let pivot = Cyclesim.lookup_node_by_name sim "stack$pivot" |> Option.value_exn in
    let high = Cyclesim.lookup_node_by_name sim "stack$high" |> Option.value_exn in
    let sm = Cyclesim.lookup_reg_by_name sim "sm" |> Option.value_exn in
    let pivot_state, _ =
      List.findi_exn Qsort.Qsort.State.all ~f:(fun _ -> function
        | Pivot -> true
        | _ -> false)
    in
    { ram; low; pivot; high; sm; pivot_state }
  ;;

  let update_on_cycle t =
    if Cyclesim.Reg.to_int t.sm = t.pivot_state
    then (
      let ram = Cyclesim.Memory.read_all t.ram in
      let ram = Array.map ram ~f:Bits.to_unsigned_int in
      let low = Cyclesim.Node.to_int t.low in
      let pivot = Cyclesim.Node.to_int t.pivot in
      let high = Cyclesim.Node.to_int t.high in
      Stdio.print_s
        [%message "" ~_:((low, pivot, high) : int * int * int) (ram : int array)])
  ;;
end

let run_qsort (sim : Sim.t) debug =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.start := vdd;
  Cyclesim.cycle sim;
  inputs.start := gnd;
  let timeout = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !timeout < 300 do
    Option.iter debug ~f:Debug.update_on_cycle;
    Cyclesim.cycle sim;
    Int.incr timeout
  done
;;

(* $MDX part-end *)

let read_results (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.read_enable := Bits.vdd;
  let result = Array.init (Int.pow 2 Config.log_size) ~f:(Fn.const 0) in
  for i = 0 to Int.pow 2 Config.log_size - 1 do
    inputs.read_address <--. i;
    Cyclesim.cycle sim;
    result.(i) <- to_unsigned_int !(outputs.data_out)
  done;
  inputs.read_enable := Bits.gnd;
  result
;;

let check_result data_in result =
  let expected = Array.copy data_in in
  Array.sort expected ~compare:Int.compare;
  if not ([%equal: int array] expected result)
  then
    Stdio.print_s
      [%message (data_in : int array) (expected : int array) (result : int array)]
;;

let setup_test ?(waves = false) ?(debug = false) () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Qsort.create scope) in
  let waves, sim =
    if waves
    then (
      let waves, sim = Hardcaml_waveterm.Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let debug = if debug then Some (Debug.create sim) else None in
  (* reset registers *)
  clear_core sim;
  sim, waves, debug
;;

let run_test sim debug data =
  load_data sim data;
  (* run qsort *)
  run_qsort sim debug;
  (* read results *)
  let result = read_results sim in
  check_result data result
;;

let%expect_test "debugging" =
  let sim, waves, debug = setup_test ~waves:true ~debug:true () in
  let data = [| 9; 4; 7; 6; 5; 5; 0; 3; 1; 4; 8; 6; 4; 0; 2; 3 |] in
  (* let data = [| 0; 4; 7; 0; 7; 7; 7; 7; 1; 3; 8; 3; 1; 3; 0; 9 |]; in *)
  run_test sim debug data;
  Option.iter
    waves
    ~f:
      (Hardcaml_waveterm.Waveform.expect_exact
         ~signals_alignment:Right
         ~display_rules
         ~start_cycle:125
         ~wave_width:1);
  [%expect_exact
    {|((0 0 15) (ram (9 4 7 6 5 5 0 3 1 4 8 6 4 0 2 3)))
((0 0 3) (ram (0 1 0 2 3 5 9 3 4 4 8 6 4 7 6 5)))
((0 3 2) (ram (0 1 0 2 3 5 9 3 4 4 8 6 4 7 6 5)))
((1 0 2) (ram (0 1 0 2 3 5 9 3 4 4 8 6 4 7 6 5)))
((5 4 15) (ram (0 0 1 2 3 5 9 3 4 4 8 6 4 7 6 5)))
((5 0 8) (ram (0 0 1 2 3 3 4 4 4 5 8 6 9 7 6 5)))
((7 6 8) (ram (0 0 1 2 3 3 4 4 4 5 8 6 9 7 6 5)))
((10 9 15) (ram (0 0 1 2 3 3 4 4 4 5 8 6 9 7 6 5)))
((11 10 15) (ram (0 0 1 2 3 3 4 4 4 5 5 6 9 7 6 8)))
((11 14 13) (ram (0 0 1 2 3 3 4 4 4 5 5 6 7 6 8 9)))
((12 11 13) (ram (0 0 1 2 3 3 4 4 4 5 5 6 7 6 8 9)))

┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│             clock││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│             clear││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│             start││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│             done_││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────┬───────┬───────────┬───────┬───────────────────────────┬───────│
│             i_idx││ 10 │11     │12         │13     │14                         │11     │
│                  ││────┴───────┴───────────┴───────┴───────────────────────────┴───────│
│                  ││────┬───────────┬───┬───────┬───────┬───┬───────────────────┬───────│
│             j_idx││ 1  │12         │13 │14     │15     │0  │1                  │12     │
│                  ││────┴───────────┴───┴───────┴───────┴───┴───────────────────┴───────│
│                  ││────┬───────────┬───┬───────┬───────┬───┬───────────────────┬───────│
│        j_idx_prev││ 0  │11         │12 │13     │14     │15 │0                  │11     │
│                  ││────┴───────────┴───┴───────┴───────┴───┴───────────────────┴───────│
│                  ││────────┬───────────────────────────────────────────────────────┬───│
│          pivot_at││ 5      │8                                                      │6  │
│                  ││────────┴───────────────────────────────────────────────────────┴───│
│                  ││────┬───────┬───┬───┬───┬───┬───┬───┬───┬───────────────┬───┬───┬───│
│qsort$read_address││ 15 │11     │12 │13 │12 │14 │13 │15 │14 │0              │13 │11 │12 │
│                  ││────┴───────┴───┴───┴───┴───┴───┴───┴───┴───────────────┴───┴───┴───│
│                  ││────┬───┬───────┬───┬───┬───┬───┬───┬───┬───┬───────────────┬───────│
│               ram││ 0  │8  │6      │9  │7  │9  │6  │9  │8  │9  │0              │6      │
│                  ││────┴───┴───────┴───┴───┴───┴───┴───┴───┴───┴───────────────┴───────│
│                  ││────┬───┬───┬───┬───────┬───┬───┬───┬───┬───┬───┬───┬───────┬───┬───│
│                sm││ Qs.│Pi.│Pa.│Sw.│Partit.│Sw.│Pa.│Sw.│Pa.│Sw.│Up.│Re.│Qsort  │Pi.│Pa.│
│                  ││────┴───┴───┴───┴───────┴───┴───┴───┴───┴───┴───┴───┴───────┴───┴───│
│                  ││────────────────────────────────────────────────────┬───┬───────────│
│         stack$low││ 11                                                 │15 │11         │
│                  ││────────────────────────────────────────────────────┴───┴───────────│
│                  ││────────────────────────────────────────────┬───────┬───┬───────────│
│       stack$pivot││ 10                                         │14     │0  │14         │
│                  ││────────────────────────────────────────────┴───────┴───┴───────────│
│                  ││────────────────────────────────────────────────┬───┬───┬───────────│
│        stack$high││ 15                                             │13 │15 │13         │
│                  ││────────────────────────────────────────────────┴───┴───┴───────────│
│                  ││────────────────────────────────────────────────────┬───┬───────────│
│               top││ 0                                                  │1  │0          │
│                  ││────────────────────────────────────────────────────┴───┴───────────│
│              push││                                                ┌───┐               │
│                  ││────────────────────────────────────────────────┘   └───────────────│
│               pop││                                                    ┌───┐           │
│                  ││────────────────────────────────────────────────────┘   └───────────│
│    stack$is_empty││────────────────────────────────────────────────────┐   ┌───────────│
│                  ││                                                    └───┘           │
│                  ││────────┬───────┬───┬───┬───────┬───────┬───┬───────────────────────│
│sort$write_address││ 0      │11     │0  │12 │13     │14     │15 │0                      │
│                  ││────────┴───────┴───┴───┴───────┴───────┴───┴───────────────────────│
│                  ││────────┬───────┬───┬───┬───┬───┬───┬───┬───┬───────────────────────│
│  qsort$write_data││ 0      │6      │0  │7  │9  │6  │9  │8  │9  │0                      │
│                  ││────────┴───────┴───┴───┴───┴───┴───┴───┴───┴───────────────────────│
│qsort$write_enable││        ┌───────┐   ┌───────────────────────┐                       │
│                  ││────────┘       └───┘                       └───────────────────────│
│                  ││────────────────────────────────────────────────┬───┬───────────────│
│artition$low$value││ 0                                              │15 │0              │
│                  ││────────────────────────────────────────────────┴───┴───────────────│
│artition$low$valid││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────────────────────────────────────────┬───┬───────────────────────│
│tition$pivot$value││ 0                                      │14 │0                      │
│                  ││────────────────────────────────────────┴───┴───────────────────────│
│tition$pivot$valid││                                        ┌───┐                       │
│                  ││────────────────────────────────────────┘   └───────────────────────│
│                  ││────────────────────────────────────────────┬───┬───┬───────────────│
│rtition$high$value││ 0                                          │13 │15 │0              │
│                  ││────────────────────────────────────────────┴───┴───┴───────────────│
│rtition$high$valid││                                            ┌───┐                   │
│                  ││────────────────────────────────────────────┘   └───────────────────│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
71b1dfcf58bbd1d8a5c5fba17d4635c6
|}]
;;

let%expect_test "random tests" =
  let sim, _, debug = setup_test ~waves:false ~debug:false () in
  for _ = 1 to 100 do
    run_test
      sim
      debug
      (Array.init (Int.pow 2 Config.log_size) ~f:(fun _ -> Random.int 10))
  done;
  [%expect {| |}]
;;
