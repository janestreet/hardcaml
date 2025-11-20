# 6.5 Binary Search

# Binary Search

Binary search allows us to find an element in a sorted array of length `N` in `log2(N)`
steps.

It starts by considering the full range of the array `0..N-1` and testing the mid-point at
`N/2`. If the element is found we are done. Otherwise if the element at the midpoint is
greater than the element we are looking for we repeat with the range set to `0..N/2`. If
it is less we use the range `N/2..N`. This continues until the element is found, or the
range becomes empty.

# Designing the Hardware Interface

The elements of the array we search must contain a `key` value by which it is sorted and
that the binary search algorithm can use.
 
In Ocaml we could represent each element as:

```ocaml
module type Data = sig 
    type t
    
    val key : t -> int
end
```

In Hardcaml we can take a similar approach.

<!-- $MDX file=./lib/binary_search.ml,part=data -->
```ocaml
module type Data = sig
  include Interface.S

  val key : Signal.t t -> Signal.t
end
```

The hardware design also needs to know the size of the array we are searching and the
number of bits in the key. Thus we define the following `Config` type.

<!-- $MDX file=./lib/binary_search.ml,part=config -->
```ocaml
module type Config = sig
  module Data : Data

  val log_size : int
  val key_size : int
end
```

The hardware design will be fully parameterized by `Config` by implementing it within a functor.

<!--
```ocaml
module type Config = sig end
```
-->

```ocaml
module Make(Config : Config) = struct
 (* ... hardware implemented here ... *)
end
```

The interface to the design is specified as follows

<!-- $MDX file=./lib/binary_search.ml,part=interface -->
```ocaml
  module Data_with_valid = With_valid.Wrap.Make (Config.Data)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t (* Clock and clear. *)
      ; start : 'a (* Pulsed to start search. *)
      ; d : 'a Config.Data.t (* Data read at `address`. *)
      ; find_key : 'a [@bits Config.key_size] (* Key we are looking for. *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a (* Low while searching. *)
      ; index : 'a [@bits Config.log_size] (* Index where key was found. *)
      ; q : 'a Data_with_valid.t (* Data where key was found. *)
      ; address : 'a [@bits Config.log_size] (* Address into search array. *)
      }
    [@@deriving hardcaml]
  end
```

The interface consists of the following logical parts.

### Clocking 

`Clocking` is an interface which groups together the `clock` and `clear` signals.

### Control

`start` is used to begin the search and `done_` indicates when a search is in progress.

`find_key` is the key we are searching for and must be provided coincident with `start`
and be held during the search operation.

### Array access 

`address` is the current index we are searching the array at and `d` is the data at that
address.

### Output result

`index` is the address at which the element was found.

`q` is the data we found.  The type of `q` is `Data_with_valid` which was defined as:

```
  module Data_with_valid = With_valid.Wrap.Make (Config.Data)
```

`With_valid` is common pattern in Hardcaml code. It wraps a signal or, as in this case, an
`Interface` with an additional `valid` bit. If `valid` is high the search succeeded and if
it is low the search failed (the element we were looking for did not exist in the input
array).

# Implementing the Search

<!-- $MDX file=./lib/binary_search.ml,part=implementation -->
```ocaml
  module State = struct
    type t =
      | Start
      | Loop
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) =
    let%hw.Always.State_machine sm =
      Always.State_machine.create (module State) (Clocking.to_spec i.clocking)
    in
    (* search range *)
    let%hw_var low = Clocking.Var.reg i.clocking ~width:(Config.log_size + 1) in
    let%hw_var mid = Clocking.Var.reg i.clocking ~width:(Config.log_size + 1) in
    let%hw_var high = Clocking.Var.reg i.clocking ~width:(Config.log_size + 1) in
    (* results *)
    let index = Clocking.Var.reg i.clocking ~width:Config.log_size in
    let q = Data_with_valid.Of_always.reg (Clocking.to_spec i.clocking) in
    let%hw key = Config.Data.key i.d in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ low <--. 0
                ; mid <--. Int.pow 2 Config.log_size / 2
                ; high <--. Int.pow 2 Config.log_size
                ; when_ i.start [ q.valid <-- gnd; sm.set_next Loop ]
                ] )
            ; ( Loop
              , [ (* success *)
                  if_
                    (i.find_key ==: key)
                    [ q.valid <-- vdd
                    ; index <-- lsbs mid.value
                    ; Config.Data.Of_always.assign q.value i.d
                    ; sm.set_next Start
                    ]
                  (* failure *)
                  @@ elif (mid.value ==: low.value) [ q.valid <-- gnd; sm.set_next Start ]
                  (* search left half *)
                  @@ elif
                       (i.find_key <: key)
                       [ high <-- mid.value
                       ; mid <-- low.value +: srl (mid.value -: low.value) ~by:1
                       ]
                  @@ (* search right half *)
                  [ low <-- mid.value
                  ; mid <-- mid.value +: srl (high.value -: mid.value) ~by:1
                  ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; index = index.value
    ; q = Data_with_valid.Of_always.value q
    ; address = lsbs mid.value
    }
  ;;
```

When start is applied we set the initial range and the state machine transitions to the
`Loop` state. The `mid` point is output as `address` and the current element is checked:

- if we found the key the search succeeds. `q.valid` is set high and the index and data
  are output.
- otherwise, if the mid point equals the low point, the search range is 0 and the search
  fails. `q.valid` is set low.
- otherwise, if the search key is less than the current key, search the left half.
- otherwise, (if the search key is greater then the current key) search the right half.

# Testing

## Adding the search array

To make the testbench simpler to implement we are going to create a top level architecture
which include the search array memory.

The interface to this design has a write port for loading the search data along with the
`start` and `find_key` values. The output is the same as before except we don't output
`address` and wire this directly into the memory.

The implementation just instantiates the search array memory and binary search module.

<!-- $MDX file=./lib/binary_search.ml,part=with_memory -->
```ocaml
module Make_with_memory (Config : Config) = struct
  module Binary_search = Make (Config)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; write_enable : 'a
      ; write_data : 'a Config.Data.t
      ; write_address : 'a [@bits Config.log_size]
      ; start : 'a
      ; find_key : 'a [@bits Config.key_size]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; index : 'a [@bits Config.log_size]
      ; q : 'a Binary_search.Data_with_valid.t
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let read_address = wire Config.log_size in
    let q =
      multiport_memory
        (Int.pow 2 Config.log_size)
        ~write_ports:
          [| { write_clock = i.clocking.clock
             ; write_address = i.write_address
             ; write_enable = i.write_enable
             ; write_data = Config.Data.Of_signal.pack i.write_data
             }
          |]
        ~read_addresses:[| read_address |]
    in
    let binary_search =
      Binary_search.create
        scope
        { Binary_search.I.clocking = i.clocking
        ; start = i.start
        ; d = Config.Data.Of_signal.unpack q.(0)
        ; find_key = i.find_key
        }
    in
    read_address <-- binary_search.address;
    { O.done_ = binary_search.done_; index = binary_search.index; q = binary_search.q }
  ;;
end
```

## Writing the testbench

### Configuring the testbench

Define the `Data` interface we will search over and the size of the array to search.

<!-- $MDX file=./lib/binary_search.ml,part=test_config -->
```ocaml
module Data = struct
  let key_size = 6

  type 'a t =
    { key : 'a [@bits key_size]
    ; data : 'a [@bits 32]
    }
  [@@deriving hardcaml]

  let key t = t.key
end

module Config = struct
  module Data = Data

  let key_size = Data.key_size
  let log_size = 4
end
```

The array size will be 16 elements. The key is 6 bits so ranges from 0 to 63. 

### Test functions

The testbench is implemented by the following functions.

- clear_core - clear (reset) the registers.
- load_inputs - load the array to search.
- run_core - start the search, wait for it to complete then return the results.

<!-- $MDX file=./lib/binary_search.ml,part=test_fns -->
```ocaml
let clear_core (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear <--. 1;
  Cyclesim.cycle sim;
  inputs.clocking.clear <--. 0
;;

let load_inputs (sim : Sim.t) (input_data : int Data.t array) =
  let inputs = Cyclesim.inputs sim in
  inputs.write_enable <--. 1;
  for i = 0 to Int.pow 2 Config.log_size - 1 do
    inputs.write_address <--. i;
    inputs.write_data.key <--. input_data.(i).key;
    inputs.write_data.data <--. input_data.(i).data;
    Cyclesim.cycle sim
  done
;;

let run_core (sim : Sim.t) find_key =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  (* Start the search. *)
  inputs.start <--. 1;
  Cyclesim.cycle sim;
  inputs.start <--. 0;
  inputs.find_key <--. find_key;
  (* Wait for it to finish. Don't let it take more than 20 cycles. *)
  let timeout = ref 0 in
  while (not (Bits.to_bool !(outputs.done_))) && !timeout < 20 do
    Cyclesim.cycle sim;
    Int.incr timeout
  done;
  Cyclesim.cycle sim;
  (* Collect and return results. *)
  let found = Bits.to_bool !(outputs.q.valid) in
  if found
  then (
    let index = Bits.to_unsigned_int !(outputs.index) in
    let data = Bits.to_unsigned_int !(outputs.q.value.data) in
    let key = Bits.to_unsigned_int !(outputs.q.value.key) in
    (* Sanity check - make sure the key we found was actually the key we were searching
       for. *)
    if key <> find_key
    then
      raise_s
        [%message
          "Found wrong key" (find_key : int) (key : int) (index : int) (data : int)];
    Some (index, data))
  else None
;;
```

### Testbench

<!-- $MDX file=./lib/binary_search.ml,part=testbench -->
```ocaml
let validate ~(input_data : int Data.t array) ~find_key ~result =
  let key_is_in_input_data =
    match Array.find input_data ~f:(fun { key; _ } -> key = find_key) with
    | None -> false
    | _ -> true
  in
  let raise_search_failed () =
    raise_s
      [%message
        "Search failed"
          (find_key : int)
          (key_is_in_input_data : bool)
          (result : (int * int) option)
          (input_data : int Config.Data.t array)]
  in
  match result, key_is_in_input_data with
  | None, true ->
    (* The hardware didn't find the element but it was there. *)
    raise_search_failed ()
  | Some _, false ->
    (* The hardware found an element but it wasn't there. *)
    raise_search_failed ()
  | None, false ->
    (* Nothing to find. *)
    ()
  | Some (index, data), true ->
    (* We found the element - check the key and data are as expected. *)
    if input_data.(index).key <> find_key || input_data.(index).data <> data
    then raise_search_failed ()
;;

let test ~waves ~input_data ~find_key =
  let open Hardcaml_waveterm in
  (* Create the simulator. *)
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Binary_search.create scope) in
  let waves, sim =
    (* Optionally trace a waveform. *)
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  (* Run the testbench. *)
  clear_core sim;
  load_inputs sim input_data;
  let result = run_core sim find_key in
  (* Optionally show the waveform. *)
  Option.iter
    waves
    ~f:(Waveform.expect_exact ~wave_width:3 ~start_cycle:16 ~display_rules);
  (* Check the results are valid. *)
  validate ~input_data ~find_key ~result
;;
```

### Randomized testing

We can now perform some randomized testing. We create an array of random `Data` elements
and sort it. We then search for some random keys in the array. If it doesn't raise all is
well.

<!-- $MDX file=./lib/binary_search.ml,part=random_tests -->
```ocaml
let%expect_test "random tests" =
  for _ = 1 to 10 do
    let input_data =
      Array.init (Int.pow 2 Config.log_size) ~f:(fun _ ->
        { Config.Data.key = random_key (); data = random_data () })
    in
    Array.sort input_data ~compare:(fun { key; _ } { key = key1; _ } ->
      Int.compare key key1);
    for _ = 1 to 20 do
      let find_key = random_key () in
      test ~waves:false ~input_data ~find_key
    done
  done;
  [%expect {| |}]
;;
```

### Waveform

<!-- $MDX file=./lib/binary_search.ml,part=waveform -->
```ocaml
let%expect_test "debug waveform" =
  let input_data =
    Array.init (Int.pow 2 Config.log_size) ~f:(fun i -> { Config.Data.key = i; data = i })
  in
  let find_key = 3 in
  test ~waves:true ~input_data ~find_key;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clocking$clock    ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
│clocking$clear    ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│write_enable      ││────────────────────────────────────────────────────────            │
│                  ││                                                                    │
│                  ││────────────────────────────────────────────────────────            │
│write_data$key    ││ 15                                                                 │
│                  ││────────────────────────────────────────────────────────            │
│                  ││────────────────────────────────────────────────────────            │
│write_data$data   ││ 15                                                                 │
│                  ││────────────────────────────────────────────────────────            │
│                  ││────────────────────────────────────────────────────────            │
│write_address     ││ 15                                                                 │
│                  ││────────────────────────────────────────────────────────            │
│start             ││        ┌───────┐                                                   │
│                  ││────────┘       └───────────────────────────────────────            │
│                  ││────────────────┬───────────────────────────────────────            │
│find_key          ││ 0              │3                                                  │
│                  ││────────────────┴───────────────────────────────────────            │
│done_             ││────────────────┐                               ┌───────            │
│                  ││                └───────────────────────────────┘                   │
│                  ││────────────────────────────────────────────────┬───────            │
│index             ││ 0                                              │3                  │
│                  ││────────────────────────────────────────────────┴───────            │
│q$valid           ││                                                ┌───────            │
│                  ││────────────────────────────────────────────────┘                   │
│                  ││────────────────────────────────────────────────┬───────            │
│q$value$key       ││ 0                                              │3                  │
│                  ││────────────────────────────────────────────────┴───────            │
│                  ││────────────────────────────────────────────────┬───────            │
│q$value$data      ││ 0                                              │3                  │
│                  ││────────────────────────────────────────────────┴───────            │
│                  ││────────────────────────────────────────┬───────────────            │
│low               ││ 0                                      │2                          │
│                  ││────────────────────────────────────────┴───────────────            │
│                  ││────────────────────────┬───────┬───────┬───────────────            │
│mid               ││ 8                      │4      │2      │3                          │
│                  ││────────────────────────┴───────┴───────┴───────────────            │
│                  ││────────────────────────┬───────┬───────────────────────            │
│high              ││ 16                     │8      │4                                  │
│                  ││────────────────────────┴───────┴───────────────────────            │
│                  ││────────────────┬───────────────────────────────┬───────            │
│sm                ││ Start          │Loop                           │Start              │
│                  ││────────────────┴───────────────────────────────┴───────            │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
c55f6d1c162be347d7d0654ae22a7310
|}]
;;
```
