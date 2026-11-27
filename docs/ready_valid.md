# 5.9 Ready/Valid Interfaces

<!--
```ocaml
# open Base
# open Hardcaml
# open Signal
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

# Ready/Valid Interfaces

A common way to move data between hardware blocks is the ready/valid handshake (also
called a "decoupled" interface, or an AXI-Stream style interface).

- The producer drives `valid` and `data`.
- The consumer drives `ready`.
- A transfer happens on every clock edge where both `valid` and `ready` are high.

`valid` and `data` flow forward from the producer to the consumer. `ready` flows backward.
A Hardcaml interface has no direction of its own: whether a field is an input or an output
depends only on whether it is in a module's `I` or `O` record. So a ready/valid port is
split in two: the forward half and the backward half are defined as separate interfaces,
and each module puts them in `I` or `O` depending on which end of the stream it sits on.

## Defining the stream interfaces

We define the two halves once. `Source` holds the signals driven by the producer, and
`Dest` holds the signal driven by the consumer.

```ocaml
module Stream = struct
  module Source = struct
    type 'a t =
      { valid : 'a
      ; data : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module Dest = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end
```

A module that receives a stream (on its upstream, `up`, side) takes `Source` as an input
and drives `Dest` as an output. A module that sends a stream (on its downstream, `dn`,
side) does the reverse. A module that does both, such as the pipeline stage below, has a
`Source` and a `Dest` in each of its `I` and `O` records.

```ocaml
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; up : 'a Stream.Source.t (* From the producer. *)
    ; dn : 'a Stream.Dest.t (* From the consumer. *)
    }
  [@@deriving hardcaml ~rtlmangle:"_"]
end

module O = struct
  type 'a t =
    { up : 'a Stream.Dest.t (* To the producer. *)
    ; dn : 'a Stream.Source.t (* To the consumer. *)
    }
  [@@deriving hardcaml ~rtlmangle:"_"]
end
```

`~rtlmangle:"_"` joins the nested field names with `_` rather than the default `$`, so
the generated port names are `up_valid`, `up_data`, `up_ready`, `dn_valid`, `dn_data` and
`dn_ready`. These are legal in both Verilog and VHDL.

```ocaml
# Stdio.print_s [%sexp (I.port_names : string I.t)]
((clock clock) (clear clear) (up ((valid up_valid) (data up_data)))
 (dn ((ready dn_ready))))
- : unit = ()
# Stdio.print_s [%sexp (O.port_names : string O.t)]
((up ((ready up_ready))) (dn ((valid dn_valid) (data dn_data))))
- : unit = ()
```

## A pipeline stage

A pipeline stage registers `valid` and `data` between a producer and a consumer. We could
pass `dn.ready` straight back as `up.ready`, but then in a chain of stages the first
stage's `up.ready` depends combinationally on the last stage's `dn.ready`, and that path
gets longer with every stage we add. If we register `ready` instead, the producer only
sees a stall one cycle late and may send one more item, so the stage needs room to hold
it.

A stage built this way is called a skid buffer. It has a main register that drives the
output and a skid register that catches the extra item when the output is stalled.
`up.ready` is high whenever the skid register is empty, so it comes straight from a
register.

```ocaml
let create (i : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let main_valid = Always.Variable.reg spec ~width:1 in
  let main_data = Always.Variable.reg spec ~width:8 in
  let skid_valid = Always.Variable.reg spec ~width:1 in
  let skid_data = Always.Variable.reg spec ~width:8 in
  let up_ready = ~:(skid_valid.value) in
  (* The main register can load a new item if it is empty, or if its current item is
     being transferred this cycle. *)
  let main_can_load = i.dn.ready |: ~:(main_valid.value) in
  Always.(
    compile
      [ if_
          main_can_load
          [ if_
              skid_valid.value
              [ (* Drain the skid register first, so the order is kept. *)
                main_valid <-- vdd
              ; main_data <-- skid_data.value
              ; skid_valid <-- gnd
              ]
              [ (* [up_ready] is high here, so this is a transfer if [up.valid]. *)
                main_valid <-- i.up.valid
              ; main_data <-- i.up.data
              ]
          ]
          [ (* The output is stalled. Catch an incoming item in the skid register. *)
            when_
              (i.up.valid &: up_ready)
              [ skid_valid <-- vdd; skid_data <-- i.up.data ]
          ]
      ]);
  { O.up = { ready = up_ready }
  ; dn = { valid = main_valid.value; data = main_data.value }
  }
;;
```

## Connecting stages together

Because `ready` flows backward, chaining two stages creates a loop: the first stage needs
the second stage's `up.ready` before the second stage has been created. As with any
feedback in Hardcaml, we break the loop with a `wire` and assign it afterwards.

```ocaml
let create_two_stages (i : _ I.t) : _ O.t =
  let stage1_dn_ready = wire 1 in
  let stage1 = create { i with dn = { ready = stage1_dn_ready } } in
  let stage2 = create { i with up = stage1.dn } in
  stage1_dn_ready <-- stage2.up.ready;
  { O.up = stage1.up; dn = stage2.dn }
;;
```

The result has the same `I` and `O` as a single stage, so it can be used anywhere a single
stage can, including in a larger chain.

## Simulating

The testbench below plays both the producer and the consumer. The producer sends an
incrementing count but has nothing to send on every fourth cycle. The consumer accepts on
only 2 cycles out of every 5. If the handshake is right, the consumer receives 0, 1, 2,
... with no gaps or repeats.

```ocaml
# let run_sim () =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create create_two_stages in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let next_to_send = ref 0 in
    let received = ref [] in
    for cycle = 0 to 29 do
      let producer_valid = cycle % 4 <> 3 in
      let consumer_ready = cycle % 5 < 2 in
      i.up.valid := Bits.of_bool producer_valid;
      i.up.data := Bits.of_unsigned_int ~width:8 !next_to_send;
      i.dn.ready := Bits.of_bool consumer_ready;
      Cyclesim.cycle sim;
      (* Transfers happen on the clock edge, so read the values from just before it. *)
      if producer_valid && Bits.to_bool !(o.up.ready)
      then Int.incr next_to_send;
      if consumer_ready && Bits.to_bool !(o.dn.valid)
      then received := Bits.to_unsigned_int !(o.dn.data) :: !received
    done;
    Stdio.printf "sent %d, received %d\n" !next_to_send (List.length !received);
    Stdio.print_s [%sexp (List.rev !received : int list)]
  ;;
val run_sim : unit -> unit = <fun>
# run_sim ()
sent 14, received 10
(0 1 2 3 4 5 6 7 8 9)
- : unit = ()
```

The consumer is the slower side, so it sets the pace. The 4 items sent but not received
are still in the pipeline when the simulation stops: each stage holds at most 2, one in
its main register and one in its skid register. Once both stages are full, `up.ready`
stays low and the producer waits.
