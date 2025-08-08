# 6.4 FFT

<!--
```ocaml
# open Base
# open Hardcaml
```
-->

# Fast Fourier Transform

In this example we will implement a 16-point fast Fourier transform (FFT).

We'll not spend much time going through the theory and will focus on translating a
software reference model to hardware. For more background you can look up wikipedia.

# Software Implementation

<!-- $MDX file=./lib/fft.ml,part=swfft -->
```ocaml
let make_twiddle_factors len =
  let pi = 3.14159265358979 in
  let c = ~-.2.0 *. pi /. Float.of_int len in
  Array.init (len / 2) ~f:(fun i -> Complex.exp { re = 0.; im = c *. Float.of_int i })
;;

let fft x =
  let len = Array.length x in
  let n_bits = Int.ceil_log2 len in
  let w = make_twiddle_factors len in
  let y = Array.init len ~f:(fun i -> x.(bitrev n_bits i)) in
  for nb = 1 to n_bits do
    let n = 1 lsl nb in
    let m = 1 lsl (n_bits - nb) in
    for h = 0 to m - 1 do
      let ofs = n * h in
      for i = 0 to (n / 2) - 1 do
        let j, k = ofs + i, ofs + i + (n / 2) in
        let a, b = y.(j), y.(k) in
        let wm = w.(i * m) in
        let wb = Complex.mul wm b in
        y.(j) <- Complex.add a wb;
        y.(k) <- Complex.sub a wb
      done
    done
  done;
  y
;;
```

At the heart of the loops we perform a complex multiply and a complex add/subtract which
is called a butterfly operation.

We read 2 values from `y` at indices `j` and `k`. In addition we look up a `w` table of
twiddle factors up at index `wm`. The butterfly operation is performed and the results are
written back to the `y` array.

The outer most loop runs `log(16) = 4` times. The two innermost loops together will perform
`16/2=8` iterations.

To generate hardware for this we need to consider the following:

1. State machine to generate the `j`, `k` and `m` values and control the iterations.
2. A ROM to store the twiddle factor coefficients.
3. RAMs to store the input, output and intermediate values during the outer iterations.
4. Logic to implement the butterfly operation.

# Hardware Implementation

The follows shows the architecture we are aiming for.

```
write input       read output
       ---->[MEM]---->
              |
            [FFT]
```

We will load FFT coefficients into MEM through the write input. The FFT will read and
write MEM multiple times to perform the transform. Then we will read the result back
through the read output.

## Controller state machine

Lets look at the values we need to generate. We can do that by modifying the FFT code and
printing the values we need to generate.

```ocaml
# let n_bits = 4
val n_bits : int = 4
# Stdio.printf " j |  k |  m\n";
  for nb = 1 to n_bits do
    Stdio.printf "------------\n";
    let n = 1 lsl nb in
    let m = 1 lsl (n_bits - nb) in
    for h = 0 to m - 1 do
      let ofs = n * h in
      for i = 0 to (n / 2) - 1 do
        let j, k = ofs + i, ofs + i + (n / 2) in
        let m = i*m in
        Stdio.printf "%2d | %2d | %2d\n" j k m
      done
    done
  done
 j |  k |  m
------------
 0 |  1 |  0
 2 |  3 |  0
 4 |  5 |  0
 6 |  7 |  0
 8 |  9 |  0
10 | 11 |  0
12 | 13 |  0
14 | 15 |  0
------------
 0 |  2 |  0
 1 |  3 |  4
 4 |  6 |  0
 5 |  7 |  4
 8 | 10 |  0
 9 | 11 |  4
12 | 14 |  0
13 | 15 |  4
------------
 0 |  4 |  0
 1 |  5 |  2
 2 |  6 |  4
 3 |  7 |  6
 8 | 12 |  0
 9 | 13 |  2
10 | 14 |  4
11 | 15 |  6
------------
 0 |  8 |  0
 1 |  9 |  1
 2 | 10 |  2
 3 | 11 |  3
 4 | 12 |  4
 5 | 13 |  5
 6 | 14 |  6
 7 | 15 |  7
- : unit = ()
```

As stated before, the outer loop runs 4 times, and the inner two loops combine to run 8
iterations.

We can encode this into a state machine is follows:

<!-- $MDX file=./lib/fft.ml,part=loop_controller -->
```ocaml
module State = struct
  type t =
    | Start
    | Loop
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let loop_controller (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let pass = Always.Variable.reg spec ~width:(Int.ceil_log2 4) in
  let count = Always.Variable.reg spec ~width:3 in
  let sm = Always.State_machine.create (module State) spec in
  Always.(
    compile
      [ sm.switch
          [ Start, [ pass <--. 0; count <--. 0; when_ i.start [ sm.set_next Loop ] ]
          ; ( Loop
            , [ count <-- count.value +:. 1
              ; when_
                  (count.value ==:. 7)
                  [ pass <-- pass.value +:. 1
                  ; when_ (pass.value ==:. 3) [ sm.set_next Start ]
                  ]
              ] )
          ]
      ]);
  pass.value, count.value, sm.is Start
;;
```

We now need a way to convert the `pass` and `count` values into `j`, `k` and `m`.

Looking the at the addresses we need to generate when `pass=0` we just need to add a bit
to the bottom of count.

```ocaml
# open Bits
# Array.init 8 ~f:(fun i ->
    let count = of_unsigned_int ~width:3 i in
    let j = count @: gnd in
    let k = count @: vdd in
    Bits.to_unsigned_int j, Bits.to_unsigned_int k)
- : (int * int) array =
[|(0, 1); (2, 3); (4, 5); (6, 7); (8, 9); (10, 11); (12, 13); (14, 15)|]
```

Similarly, when `pass=1` we insert a bit between bits 0 and 1 of `count`.

```ocaml
# Array.init 8 ~f:(fun i ->
    let count = of_unsigned_int ~width:3 i in
    let j = count.:[2,1] @: gnd @: count.:(0) in
    let k = count.:[2,1] @: vdd @: count.:(0) in
    Bits.to_unsigned_int j, Bits.to_unsigned_int k)
- : (int * int) array =
[|(0, 2); (1, 3); (4, 6); (5, 7); (8, 10); (9, 11); (12, 14); (13, 15)|]
```

For `pass=2` and `pass=3` we do the same thing at the next bit position up.

For `m` we shift `count` left by `3-pass`.

There is one final thing - in the FFT code the first thing we do is reorganize the input
array as follows:

```
let y = Array.init len ~f:(fun i -> x.(bitrev n_bits i)) in
```

We can do this by reversing the input address when we load the input FFT coefficients into
the design.

Here's the final code:

<!-- $MDX file=./lib/fft.ml,part=index_generation -->
```ocaml
let index_generator ~pass ~count =
  let jk const =
    mux
      pass
      [ count @: const
      ; count.:[2, 1] @: const @: count.:[0, 0]
      ; count.:[2, 2] @: const @: count.:[1, 0]
      ; const @: count.:[2, 0]
      ]
  in
  let j = jk gnd in
  let k = jk vdd in
  let m =
    mux pass [ sll count ~by:3; sll count ~by:2; sll count ~by:1; sll count ~by:0 ]
  in
  j, k, m
;;
```

## Butterfly operation

To compute the butterfly operation we need to decide how we are going to represent complex
numbers.

Typically we would consider a fixed point format designed to suit our dynamic range and
required precision. Instead we are going to cheat and use a feature of Hardcaml that lets
us represent full precision floating point numbers within hardware simulations.

<!-- $MDX file=./lib/fft.ml,part=dcomplex -->
```ocaml
module Dcomplex = struct
  type 'a t =
    { re : 'a [@bits 64]
    ; im : 'a [@bits 64]
    }
  [@@deriving hardcaml]

  open Cyclesim_float_ops.Double

  let add a b = { re = a.re +: b.re; im = a.im +: b.im }
  let sub a b = { re = a.re -: b.re; im = a.im -: b.im }

  let mul a b =
    { re = (a.re *: b.re) -: (a.im *: b.im); im = (a.re *: b.im) +: (a.im *: b.re) }
  ;;
end
```

The `Cyclcesim_float_ops.Double` module provides the operators which take and return 64
bit signals and implement various floating point operations over them. `Dcomplex`
implements complex multiply and add using it.

The butterfly implementation is now as follows:

<!-- $MDX file=./lib/fft.ml,part=butterfly -->
```ocaml
let butterfly a b w =
  let wb = Dcomplex.mul w b in
  Dcomplex.add a wb, Dcomplex.sub a wb
;;
```

## Twiddle factors

This is straight forward - we just create a ROM of `Dcomplex` values from the floating
point twiddle factors computed for the software version.

<!-- $MDX file=./lib/fft.ml,part=twiddle_rom -->
```ocaml
let bits_of_float (type a) (module Comb : Comb.S with type t = a) f : a =
  Int64.bits_of_float f |> Comb.of_int64_trunc ~width:64
;;

let float_of_bits b = Bits.to_int64_trunc b |> Int64.float_of_bits

let twiddle_factor_rom m =
  let w = make_twiddle_factors 16 in
  Array.to_list w
  |> List.map ~f:(fun (w : Complex.t) ->
    { Dcomplex.re = bits_of_float (module Signal) w.re
    ; im = bits_of_float (module Signal) w.im
    })
  |> Dcomplex.Of_signal.mux m
;;
```

## Memory buffer

For simplicity, the memory is implemented with 3 read and 3 write ports. The FFT algorithm
needs to read and write two complex numbers per cycle so that accounts for two of the
read/write ports. In addition we use 1 write port to load the FFT coefficients and 1 read
port to get the result.

The memory will store 16 complex numbers in total.

It is also set up with asynchronous read ports - yet another small simplification for
this example.

This is not a terribly realistic memory structure for efficient implementation, but it
allows us to complete the design and test it.

<!-- $MDX file=./lib/fft.ml,part=memory_buffer -->
```ocaml
let memory_buffer
  ~clock
  ~(ext_write : _ Write_port.t)
  ~(int_write_a : _ Write_port.t)
  ~(int_write_b : _ Write_port.t)
  ~int_read_a
  ~int_read_b
  ~ext_read
  =
  let q =
    multiport_memory
      16
      ~write_ports:
        [| { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack ext_write.data
           ; write_enable = ext_write.enable
           ; write_address = reverse ext_write.address
           }
         ; { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack int_write_a.data
           ; write_enable = int_write_a.enable
           ; write_address = int_write_a.address
           }
         ; { write_clock = clock
           ; write_data = Dcomplex.Of_signal.pack int_write_b.data
           ; write_enable = int_write_b.enable
           ; write_address = int_write_b.address
           }
        |]
      ~read_addresses:[| ext_read; int_read_a; int_read_b |]
  in
  Array.map q ~f:Dcomplex.Of_signal.unpack
;;
```

Note that the write address on the first port is reversed as we said would be required.

## Sticking it all together


<!-- $MDX file=./lib/fft.ml,part=core -->
```ocaml
let create (i : _ I.t) =
  let scope = Scope.create () in
  let%hw pass, count, done_ = loop_controller i in
  let%hw j, k, m = index_generator ~pass ~count in
  let%hw.Dcomplex.Of_signal w = twiddle_factor_rom m in
  let%hw.Dcomplex.Of_signal a = Dcomplex.Of_signal.wires () in
  let%hw.Dcomplex.Of_signal b = Dcomplex.Of_signal.wires () in
  let%hw.Dcomplex.Of_signal a_next, b_next = butterfly a b w in
  let memory_buffer =
    memory_buffer
      ~clock:i.clock
      ~ext_write:i.write
      ~int_write_a:{ enable = ~:done_; address = j; data = a_next }
      ~int_write_b:{ enable = ~:done_; address = k; data = b_next }
      ~int_read_a:j
      ~int_read_b:k
      ~ext_read:i.read_address
  in
  Dcomplex.Of_signal.assign a memory_buffer.(1);
  Dcomplex.Of_signal.assign b memory_buffer.(2);
  { O.done_; data_out = memory_buffer.(0) }
;;
```

This instantiates the loop controller, index generator, twiddle factor rom, butterfly and
memory buffer.

There is a loop through the butterfly and memory buffer logic which requires us to predefine the
`a` and `b` complex values as `wires`.

# Testing the Design

To test the FFT design we will need 4 functions

1. `clear_core` simple clear (reset) of the registers in the design.
2. `load_fft_coefficients` load the input test data.
3. `run_fft` start the core and wait for it to complete.
4. `read_results` read back the final results from the design.

<!-- $MDX file=./lib/fft.ml,part=testbench_fns -->
```ocaml
let clear_core (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd
;;

let load_fft_coefficients (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.write.enable := Bits.vdd;
  for i = 0 to 15 do
    inputs.write.address <--. i;
    inputs.write.data.re := bits_of_float (module Bits) (Float.of_int (i + 1));
    inputs.write.data.im := bits_of_float (module Bits) (Float.of_int 0);
    Cyclesim.cycle sim
  done;
  inputs.write.enable := Bits.gnd
;;

let run_fft (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  while not (Bits.to_bool !(outputs.done_)) do
    Cyclesim.cycle sim
  done
;;

let read_results (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  for i = 0 to 15 do
    inputs.read_address <--. i;
    Cyclesim.cycle sim;
    let result =
      { Complex.re = float_of_bits !(outputs.data_out.re)
      ; im = float_of_bits !(outputs.data_out.im)
      }
    in
    Stdio.printf "[%d] %f %+fi\n" i result.re result.im
  done
;;
```

Finally, we create our simulator and run a simulation.

Note that we have provided the `Cyclesim_float_ops.Double.database` to `Cyclesim` so it
knows how to implement the floating point operations.

<!-- $MDX file=./lib/fft.ml,part=testbench -->
```ocaml
let hw_test () =
  let sim =
    Sim.create
      ~config:
        { Cyclesim.Config.trace_all with
          combinational_ops_database = Cyclesim_float_ops.Double.database
        }
      create
  in
  let waves, sim = Waveform.create sim in
  clear_core sim;
  load_fft_coefficients sim;
  run_fft sim;
  read_results sim;
  Waveform.expect waves ~wave_width:2 ~display_rules ~start_cycle:38
;;
```

## Waveform

<!-- $MDX file=./lib/fft.ml,part=waveform -->
```ocaml
let%expect_test "" =
  hw_test ();
  [%expect
    {|
    [0] 136.000000 +0.000000i
    [1] -8.000000 +40.218716i
    [2] -8.000000 +19.313708i
    [3] -8.000000 +11.972846i
    [4] -8.000000 +8.000000i
    [5] -8.000000 +5.345429i
    [6] -8.000000 +3.313708i
    [7] -8.000000 +1.591299i
    [8] -8.000000 +0.000000i
    [9] -8.000000 -1.591299i
    [10] -8.000000 -3.313708i
    [11] -8.000000 -5.345429i
    [12] -8.000000 -8.000000i
    [13] -8.000000 -11.972846i
    [14] -8.000000 -19.313708i
    [15] -8.000000 -40.218716i
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │clear          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │start          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │done_          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │i$enable       ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││────────────────────────┬──────────────────────────│
    │pass           ││ 2                      │3                         │
    │               ││────────────────────────┴──────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │count          ││ 4    │5    │6    │7    │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │j              ││ 8    │9    │10   │11   │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │k              ││ 12   │13   │14   │15   │8    │9    │10   │11   │12│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │m              ││ 0    │2    │4    │6    │0    │1    │2    │3    │4 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││───────────────────────────────────────────────────│
    │i$address      ││ 15                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │read_address   ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │i$re           ││ 16.                                               │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │i$im           ││ 0.                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││──────────────────────────────┬────────────────────│
    │o$re           ││ 64.                          │136.                │
    │               ││──────────────────────────────┴────────────────────│
    │               ││───────────────────────────────────────────────────│
    │o$im           ││ 0.                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a$re           ││ 32.  │-8.0.│-8.  │-7.9.│64.  │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a$im           ││ 0.   │8.   │0.   │-8.  │0.   │19.3.│8.   │3.31.│0.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b$re           ││ 40.  │-8.0.│-8.  │-7.9.│72.  │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b$im           ││ 0.   │8.   │0.   │-8.  │0.   │19.3.│8.   │3.31.│0.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │w$re           ││ 1.   │0.70.│1.61.│-0.7.│1.   │0.92.│0.70.│0.38.│1.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │w$im           ││ -0.  │-0.7.│-1.  │-0.7.│-0.  │-0.3.│-0.7.│-0.9.│-1│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a_next$re      ││ 72.  │-8.0.│-8.0.│-8.0.│136. │-8.0.│-8.0.│-8.0.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │a_next$im      ││ 0.   │19.3.│8.   │3.31.│0.   │40.2.│19.3.│11.9.│8.│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b_next$re      ││ -8.  │-7.9.│-7.9.│-7.9.│-8.  │-7.9.│-7.9.│-7.9.│-7│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │b_next$im      ││ 0.   │-3.3.│-8.  │-19..│0.   │-1.5.│-3.3.│-5.3.│-8│
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    7a788a5d3036c98ecb7da5ed09414842
    |}]
;;
```

An interesting point here is we are displaying floating point values within the waveform.
This is done by writing a `Custom` display rule for floating point numbers.

<!-- $MDX file=./lib/fft.ml,part=display_rules -->
```ocaml
let display_rules =
  let float name =
    Display_rule.port_name_is
      name
      ~wave_format:(Custom (fun b -> float_of_bits b |> Float.to_string))
  in
  let complex name = [ float (name ^ "$re"); float (name ^ "$im") ] in
  let bit name = Display_rule.port_name_is name ~wave_format:Bit in
  let uint name = Display_rule.port_name_is name ~wave_format:Unsigned_int in
  [ [ bit "clock"
    ; bit "clear"
    ; bit "start"
    ; bit "done_"
    ; bit "i$enable"
    ; uint "pass"
    ; uint "count"
    ; uint "j"
    ; uint "k"
    ; uint "m"
    ; uint "i$address"
    ; uint "read_address"
    ]
  ; complex "i"
  ; complex "o"
  ; complex "a"
  ; complex "b"
  ; complex "w"
  ; complex "a_next"
  ; complex "b_next"
  ]
  |> List.concat
;;
```

# Improving the Design

To make this design more realistic there are a few things we would need to change.

1. Switching to a fixed point representation of Complex numbers.
2. Pipelining the butterfly module, and synchronizing the read/write processes within the
   core. At the moment it is entirely combinational.
3. Making the memory buffer read ports synchronous so they can be implemented efficiently.
4. Adding ping pong buffers for the input/output stages to reduce the total number of ports required.
5. Making it work for generic power of 2 transform sizes.
