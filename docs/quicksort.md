# 6.6 Quicksort

# Quicksort

Quicksort is a sorting algorithm that takes `O(N log N)` operations in the average case
and `O(N^2)` in the worst case. It is a divide and conquer algorithm that recursively sorts
the input in two halves. It works as follows:

1. Pick some element from the array.  Call this the pivot.
2. Partition the array such into three parts.

```
[ <= pivot][pivot][ >= pivot]
```

All values less than or equal to the pivot are placed before the pivot and all elements
greater than the pivot are placed after it. Given this we can be sure pivot is now
actually in the correct position in the array.

3. Recursively apply quicksort to the left and right partitions.

Quicksort describes a family of algorithms with some different trade offs - in particular
the actual partitioning algorithm and how to choose the pivot can affect its performance on
different input arrays.

Lets start with the most basic implementation.

<!-- $MDX file=./lib/quicksort.ml,part=functional -->
```ocaml
  let rec qsort a =
    match a with
    | [] -> []
    | x :: xs ->
      let left = List.filter xs ~f:(fun y -> y <= x) in
      let right = List.filter xs ~f:(fun y -> y > x) in
      List.concat [ qsort left; [ x ]; qsort right ]
  ;;
```

It is a very elegant implementation and indeed runs with the expected average and worst
case complexities. However, it also allocates lots of intermediate lists and the amount of
space needed will be proportional to the runtime i.e. in worst case it could require `O(N^2)`
space.

We want to implement this on in hardware so we'd like to get a better bound on the space
requirements.

## In place sorting

Lomuto described a partitioning scheme that is both simple and can run in place i.e. it
modifies the input array rather than allocating new arrays.

<!-- $MDX file=./lib/quicksort.ml,part=lomuto -->
```ocaml
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
```

So we've fixed the space problem ... except we haven't quite. Although we no longer
allocate new data arrays, we still use stack space for the recursive calls. The stack
usage will follow the average and worst case complexity of the algorithm depending on the
input data ie it could be up to `O(N)`.

Why does this happen? It's because of the choice of pivot. For best performance the pivot
should evenly split the input array but we cannot guarantee that. If the input was already
sorted we hit a degenerate case where the left partition is all the elements except the
pivot and the right partition is empty. Thus we need to recurse `N` times to quicksort
it.

## Sedgewick's log stack space trick

Now we come to the implementation we want to use for hardware. This reorganizes the
recursion so it only uses stack space on the smaller partition. It doesn't change the
amount of computation we do - it will still be `O(N^2)` - but we can ensure we use only
`log N` stack space.

<!-- $MDX file=./lib/quicksort.ml,part=sedgewick -->
```ocaml
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
```

# Hardware Design

## Top level architecture

```
      |\   _______   |\
______||__|       |__||_____
     _||  |  RAM  |  ||_
    | |/  |_______|  |/ |
    |      _______      |
    |     |       |     |
    |_____| QSORT |_____|
          |       |
          |_______|
```

The top level design consists of a RAM and the sorting core.  The RAM is implementation 
with a Simple Dual Port RAM.  This means it has one read and one write port.  

The sorting core requires read/write access to the RAM as does the external interface in
order to load the data and read back the result. Thus the ports are multiplexed. This
basically means the RAM should not be accessed by the external interface while the core is
running.

<!-- $MDX file=./lib/quicksort.ml,part=config -->
```ocaml
module type Config = sig
  val log_size : int
  val data_size : int
end
```

<!-- $MDX file=./lib/quicksort.ml,part=toplevel -->
```ocaml
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
```

We show a little trick for designs like this where the outputs of both modules are also
required as inputs to the modules. You can predefine the outputs of one model as wires and
then use them in the other module. We can then assign the wires when we create the second
module. This trick extends to as many modules as needed.

# Quicksort Design

There are three main things the core must implement:

1. The partitioning process.
2. Control of recursion and iterations.
3. Management of the stack.

In addition there is a complexity with regard to the RAM. Because we are instantiating a
proper synthesizable RAM the read is synchronous - this means we get the read data one
cycle after providing the read address. We will need to account for this latency in our
state machine.

## Call stack
<!-- $MDX file=./lib/quicksort.ml,part=partition -->
```ocaml
  module Partition = struct
    type 'a t =
      { low : 'a [@bits log_size]
      ; high : 'a [@bits log_size]
      ; pivot : 'a [@bits log_size]
      }
    [@@deriving hardcaml]
  end

  module Partition_with_valids = With_valid.Fields.Make (Partition)
```

<!-- $MDX file=./lib/quicksort.ml,part=callstack -->
```ocaml
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
```

The hardware outputs the current top most entry of the stack via `O.partition`. This is of
type `_ Partition.t` and contains the low and high indices of the current range along with
the pivot index.

We allow writes to the stack in two different ways. `I.partition` has a valid associated
with each field. If the `valid` is high then we overwrite that field in the current `top`
entry. Alternatively, if `I.push` is set then we write all fields (regardless of `valid`)
to the next entry on the stack and increment the `top` pointer. Note `mux2` when setting
the `write_address` of the memory which implements this.

If `I.pop` is set we decrement the `top` pointer effectively popping the top entry
of the stack.

Lastly we provide `O.is_empty` which indicates if the stack is empty.

## Sorting core

### Interface

The interface to the core is very simple. Apart from `clock` and `clear` it just consists of a
`start` signal, a `done_` indicator and the connections to the read and write ports of the
RAM.

<!-- $MDX file=./lib/quicksort.ml,part=interface -->
```ocaml
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
```

### States

The sorting algorithm is implemented as state machine. The states `Qsort`, `Update_range`
and `Recurse` are involved with recursion and iteration control. `Pivot`, `Partition`,
`Swap` and `Swap_pivot` perform the partitioning process. The state machine begins in the
`Start` state.

<!-- $MDX file=./lib/quicksort.ml,part=states -->
```ocaml
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
```

We'll discuss each state in detail below. Where appropriate we shall refer to the
Sedgewick implementation of qsort along with the Lomuto partitioning code shown again
below with line numbers.

```
 1  let swap a i j =
 2    let tmp = a.(i) in
 3    a.(i) <- a.(j);
 4    a.(j) <- tmp
 5  ;;
 6
 7  let partition a ~low ~high =
 8    let pivot = a.(high) in
 9    let i = ref low in
10    for j = low to high - 1 do
11      if a.(j) < pivot
12      then (
13        swap a !i j;
14        int.incr i)
15    done;
16    swap a !i high;
17    !i
18  ;;
19
20  let rec qsort a ~low ~high =
21    let low, high = ref low, ref high in
22    while !low < !high do
23      let pivot = partition a ~low:!low ~high:!high in
24      if pivot - !low < !high - pivot
25      then (
26        qsort a ~low:!low ~high:(pivot - 1);
27        low := pivot + 1)
28      else (
29        qsort a ~low:(pivot + 1) ~high:!high;
30        high := pivot - 1)
31    done
32  ;;
```

#### Start

When `start` is raised the initial search range is set at the top of the call stack and we
transition to `Qsort`.

#### Qsort

This controls the main while loop in the algorithm at line 22.  

If the test passes we move state `Pivot` and start the partitioning process. Note we also set
the RAM read address to the pivot point. This is done so the pivot is available to be read
in the next state.

When the test fails we end the qsort function - but only at this level of the stack. Hence
the state machine will pop the `Call_stack` and repeat the test. If the `Call_stack`
becomes empty we are done and go back to the `Start` state.

#### Pivot

In this state we simply read the pivot value we will use for partitioning. It corresponds
to line 8. We move directly to state `Partition`. We set the read address to the start of
the partition so we can begin streaming through it in the following states.

#### Partition, Swap, and Swap_pivot.

The states `Partition` and `Swap` implement the for loop on lines 10 to 15. If the test
against the pivot at line 11 passes we immediately write the value at `j` into `i` while
setting the read address to `i` and transition to `Swap`. `Swap` writes the value at
`i` into `j` completing the swap. It transitions back immediately to state `Partition`.

Note we read and write the value at address `i` in the RAM on the same cycle. This will
write the new value while reading the old value (which is what we require for the swap
operation). This behavior is due to setting the collision mode of the RAM to
`Read_before_write`.

On line 10 we see the iteration goes up to `high-1`. The state machine actually goes up to
`high`. The reason for this is we need to swap the values at high and `i` as on line 16. A
key point here is the test on line 11 - it will never be true for the value at `high` as
it is the pivot value. When we reach high we will transition to `Swap_pivot` to complete
the final swap operation.

Within `Swap_pivot` we also now know the correct index of the pivot so write it into the
current `Call_stack` entry. It will be used for later iterations.

#### Update_range and recurse

These states implement lines 23 to 30. They find the smaller of the two partitions and
recurse into it.  In terms of hardware we need to do two things, hence the two states.

In `Update_range` we are dealing with line 27 or 30. This is effectively replacing the
current `Call_stack` entry with the larger range. A small subtlety here is by updating the
`Call_stack` entry we actually change the test for the smaller partition - hence we must
store it in a register for use in `Recurse`.

In `Recurse` we push the smaller partition into the `Call_stack`. There is a little extra
logic to do with setting the `push` signal. We avoid doing so if the pivot would become
negative or larger than the array size. This is because our register values (high, low,
pivot etc) cannot represent numbers outside the range `0 .. array size-1`. It is safe to
do this - if we did recurse (and extended the size of the registers) we would immediately
fail the test on line 22 anyway.

### Implementation

Below is the complete implementation for reference.

<!-- $MDX file=./lib/quicksort.ml,part=core -->
```ocaml
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
```

## Testing

We will not go through the testbench in detail for this example - suffice it to say it
looks very similar to previous examples - perform a reset, load the input data, start the
core and wait for it to complete and then read back the results and check they are
correct.

There is an interesting bit of code to do with debugging though.

<!-- $MDX file=./lib/quicksort.ml,part=debug -->
```ocaml
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
```

While the core is running we (optionally) execute a function called
`Debug.update_on_cycle` on every clock cycle. This uses a feature of `Cyclesim` that
allows us to lookup internal values within a simulation. In particular, we look up the
high, low and pivot values and the complete memory contents every time the state machine
enters the `Pivot state`.

We then dump those values. When debugging the state machine this was very useful in
tracking down problems.  The output looks likes this:

```
((0 0 15) (ram (9 4 7 6 5 5 0 3 1 4 8 6 4 0 2 3)))
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
```

This data made it much, much easier to find points where things started to go awry.
