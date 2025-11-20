open Base
open Hardcaml
open Signal
module Clocking = Clocking

(* $MDX part-begin=data *)
module type Data = sig
  include Interface.S

  val key : Signal.t t -> Signal.t
end
(* $MDX part-end *)

(* $MDX part-begin=config *)
module type Config = sig
  module Data : Data

  val log_size : int
  val key_size : int
end
(* $MDX part-end *)

module Make (Config : Config) = struct
  (* $MDX part-begin=interface *)
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
  (* $MDX part-end *)

  (* $MDX part-begin=implementation *)
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

  (* $MDX part-end *)

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"binary_search" create input
  ;;
end

(* $MDX part-begin=with_memory *)
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
(* $MDX part-end *)

(* $MDX part-begin=test_config *)
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
(* $MDX part-end *)

module Binary_search = Make_with_memory (Config)
module Sim = Cyclesim.With_interface (Binary_search.I) (Binary_search.O)
open Bits

(* $MDX part-begin=test_fns *)
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

(* $MDX part-end *)

let display_rules =
  let open Hardcaml_waveterm.Display_rule in
  let uint name = port_name_is name ~wave_format:(Bit_or Unsigned_int) in
  let module I = Hardcaml_waveterm.Display_rules.With_interface (Binary_search.I) in
  let module O = Hardcaml_waveterm.Display_rules.With_interface (Binary_search.O) in
  [ I.default ~wave_format:(Bit_or Unsigned_int) ()
  ; O.default ~wave_format:(Bit_or Unsigned_int) ()
  ; [ uint "low"; uint "mid"; uint "high"; port_name_is "sm" ]
  ]
  |> List.concat
;;

(* $MDX part-begin=testbench *)

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

(* $MDX part-end *)

(* $MDX part-begin=waveform *)
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

(* $MDX part-end *)

let random_key () = Random.int (Int.pow 2 Config.key_size)
let random_data () = Random.int (Int.pow 2 32)

let%expect_test "randomized example waveform" =
  let input_data =
    Array.init (Int.pow 2 Config.log_size) ~f:(fun _ ->
      { Config.Data.key = random_key (); data = random_data () })
  in
  let find_key = 3 in
  test ~waves:true ~input_data ~find_key;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clocking$clock    ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
│clocking$clear    ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────    │
│write_enable      ││────────────────────────────────────────────────────────────────    │
│                  ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────    │
│write_data$key    ││ 20                                                                 │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────────────────────────────────────────────    │
│write_data$data   ││ 869180822                                                          │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────────────────────────────────────────────    │
│write_address     ││ 15                                                                 │
│                  ││────────────────────────────────────────────────────────────────    │
│start             ││        ┌───────┐                                                   │
│                  ││────────┘       └───────────────────────────────────────────────    │
│                  ││────────────────┬───────────────────────────────────────────────    │
│find_key          ││ 0              │3                                                  │
│                  ││────────────────┴───────────────────────────────────────────────    │
│done_             ││────────────────┐                                       ┌───────    │
│                  ││                └───────────────────────────────────────┘           │
│                  ││────────────────────────────────────────────────────────────────    │
│index             ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────    │
│q$valid           ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────────────────────────────────────────────    │
│q$value$key       ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────────────────────────────────────────────    │
│q$value$data      ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────────────────────────────────────────────    │
│low               ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────    │
│                  ││────────────────────────┬───────┬───────┬───────┬───────────────    │
│mid               ││ 8                      │4      │2      │1      │0                  │
│                  ││────────────────────────┴───────┴───────┴───────┴───────────────    │
│                  ││────────────────────────┬───────┬───────┬───────┬───────────────    │
│high              ││ 16                     │8      │4      │2      │1                  │
│                  ││────────────────────────┴───────┴───────┴───────┴───────────────    │
│                  ││────────────────┬───────────────────────────────────────┬───────    │
│sm                ││ Start          │Loop                                   │Start      │
│                  ││────────────────┴───────────────────────────────────────┴───────    │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
6ab756f19a31ebf5b7241090f8405d77
|}]
;;

(* $MDX part-begin=random_tests *)
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
(* $MDX part-end *)
