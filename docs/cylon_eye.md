# 4.2.2 Cylon Eye

In this example we will simulate the scanning effect of a Cylons eye as seen in Battlestar
Galactica.

We will assume we have an array of 16 LEDs that we can drive with a 3 bit intensity value
where 0 is off and 7 is full brightness.

# Implementation

<!-- $MDX file=./lib/cylon.ml,part=cylon_eye -->
```ocaml
let cylon_eye ~clock ~enable =
  let spec = Reg_spec.create ~clock () in
  let eye_pos = Always.Variable.reg spec ~enable ~width:4 in
  let eye_dir = Always.Variable.reg spec ~enable ~width:1 in
  let eye_bar = List.init 16 ~f:(fun _ -> Always.Variable.reg spec ~enable ~width:3) in
  Always.(
    compile
      [ (* Decrease intensity. *)
        proc
          (List.map eye_bar ~f:(fun eye ->
             proc [ eye <-- eye.value -:. 1; when_ (eye.value ==:. 0) [ eye <--. 0 ] ]))
      ; (* Set current eye to max intensity. *)
        proc
          (List.mapi eye_bar ~f:(fun idx eye ->
             when_ (eye_pos.value ==:. idx) [ eye <--. 7 ]))
      ; (* Scan left and right. *)
        if_
          eye_dir.value
          [ eye_pos <-- eye_pos.value -:. 1
          ; when_ (eye_pos.value ==:. 1) [ eye_dir <-- gnd ]
          ]
          [ eye_pos <-- eye_pos.value +:. 1
          ; when_ (eye_pos.value ==:. 14) [ eye_dir <-- vdd ]
          ]
      ]);
  List.map eye_bar ~f:(fun eye -> eye.value)
;;
```

The circuit takes 2 inputs - the clock and an enable. The enable is wired into every
register. We will see why later.

The implementation has 3 parts. First, we go through every `eye` and decrease it's intensity
by one to a minimum of 0.

We then find the current eye located at `eye_pos` and set it to maximum intensity. Together
these processes create a eye with a tail.  Because it looks cooler.

Finally we have some logic which updates the eye position. When `eye_dir` is zero we move
left and when it is one we move right. At the boundaries we switch direction.

# Testing

Testing is pretty trivial. The only input is `enable` which we set to 1, and otherwise all
we have to do is print the outputs.

<!-- $MDX file=./lib/cylon.ml,part=testbench -->
```ocaml
let test () =
  let sim =
    Circuit.create_exn
      ~name:"cylon_eye"
      (cylon_eye ~clock:(input "clock" 1) ~enable:(input "enable" 1)
       |> List.mapi ~f:(fun i -> output [%string "eye%{i#Int}"]))
    |> Cyclesim.create
  in
  let eyes = List.init 16 ~f:(fun i -> Cyclesim.out_port sim [%string "eye%{i#Int}"]) in
  Cyclesim.in_port sim "enable" := Bits.vdd;
  for _ = 1 to 40 do
    Cyclesim.cycle sim;
    List.iter eyes ~f:(fun e -> printf "%i" (Bits.to_unsigned_int !e));
    printf "\n"
  done
;;

let%expect_test "" =
  test ();
  [%expect
    {|
    7000000000000000
    6700000000000000
    5670000000000000
    4567000000000000
    3456700000000000
    2345670000000000
    1234567000000000
    0123456700000000
    0012345670000000
    0001234567000000
    0000123456700000
    0000012345670000
    0000001234567000
    0000000123456700
    0000000012345670
    0000000001234567
    0000000000123476
    0000000000012765
    0000000000007654
    0000000000076543
    0000000000765432
    0000000007654321
    0000000076543210
    0000000765432100
    0000007654321000
    0000076543210000
    0000765432100000
    0007654321000000
    0076543210000000
    0765432100000000
    7654321000000000
    6743210000000000
    5672100000000000
    4567000000000000
    3456700000000000
    2345670000000000
    1234567000000000
    0123456700000000
    0012345670000000
    0001234567000000
    |}]
;;
```

# Real World Considerations

Lets say we clock this circuit at 100Mhz. It takes 30 cycles to scan left to right and
back again, so that means we will do something like 3.33 million scans per second. That's
obviously not something we could actually see.

This is why we included the enable signal. We can use it to slow down the scanning process
by only enabling it every so often.

The simplest way to do this is to define another counter and only enable the scan when the
counter equals some value. If we want the scan to take 1 second we want to count to 3.33
million so we will need a 22 bit counter.

The second issue is that LEDs are driven by a single on/off value. So how do we show
intensity? For this we need to use a technique called Pulse Width Modulation (PWM). The
idea is that we will turn the LED on and off much faster than we can perceive and in
proportion to the intensity we want to show.  Here is our final design.


<!-- $MDX file=./lib/cylon.ml,part=cylon_top -->
```ocaml
let cylon_eye_top ~scope ~enable_rate ~clock =
  let spec = Reg_spec.create ~clock () in
  let%hw_var enable = Always.Variable.reg spec ~width:1 in
  let eyes = cylon_eye ~clock ~enable:enable.value in
  let%hw_var count = Always.Variable.reg spec ~width:22 in
  let leds =
    List.init (List.length eyes) ~f:(fun _ -> Always.Variable.reg spec ~width:1)
  in
  Always.(
    compile
      [ (* Control the enable. *)
        enable <-- gnd
      ; count <-- count.value +:. 1
      ; when_ (count.value ==:. enable_rate - 1) [ count <--. 0; enable <-- vdd ]
      ; (* PWM for each LED. *)
        proc
          (List.map2_exn leds eyes ~f:(fun led eye ->
             led <-- (eye >=: count.value.:[2, 0])))
      ]);
  List.map leds ~f:(fun led -> led.value)
;;
```

We made the rate at which the enable toggles a parameter called `enable_rate`. We would
want to set this to `3_333_333` when generating hardware. For testing, we set it to `8` so
that we can see the PWM happening.

<!-- $MDX file=./lib/cylon.ml,part=cylon_top_test -->
```ocaml
let test () =
  let sim =
    let scope = Scope.create ~flatten_design:true () in
    let leds = cylon_eye_top ~scope ~enable_rate:8 ~clock:(input "clock" 1) in
    Circuit.create_exn
      ~name:"cylon_eye"
      (List.mapi leds ~f:(fun i -> output (Printf.sprintf "led%.2i" i)))
    |> Cyclesim.create ~config:Cyclesim.Config.trace_all
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  for _ = 1 to 120 do
    Cyclesim.cycle sim
  done;
  Waveform.expect_exact waves ~wave_width:(-2)
;;

let%expect_test "" =
  test ();
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
│                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
│enable            ││    ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥           │
│                  ││────╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───        │
│led00             ││┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──        │
│led01             ││┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──┘└──┘└──┘└──┘└──        │
│led02             ││┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──┘└──┘└──┘└──        │
│led03             ││┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──┘└──┘└──        │
│led04             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──┘└──        │
│led05             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──┘└──        │
│led06             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──┘└──        │
│led07             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐ ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─┘└──        │
│led08             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐ ┌─┐         │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─┘ └─        │
│led09             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐┌─┐         │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └┘ └─        │
│led10             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐┌──┐        │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └┘  └        │
│led11             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────╥──┐        │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘       ╨  └        │
│led12             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───────        │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘               │
│led13             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌───        │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘           │
│led14             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──        │
│led15             ││┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐  ┌┐          │
│                  ││┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──┘└──        │
│                  ││┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥┬╥        │
│count             │││║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║│║        │
│                  ││┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨┴╨        │
│gnd               ││                                                                    │
│                  ││────────────────────────────────────────────────────────────        │
│vdd               ││────────────────────────────────────────────────────────────        │
│                  ││                                                                    │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
23a1bb8cebaa916241e49f8dd3880407
|}]
;;
```

> 📝 There is sometimes a maximum rate at which we should drive a PWM signal. I am not
> sure it applies to LEDs but it is easily adjustable by changing the PWM code as follows
> `led <-- (eye >=: count.value.:[12, 10])`. This will slow down the toggling but will
> still drive the same average power.
