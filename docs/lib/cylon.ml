open Base
open Hardcaml
open Signal
open Hardcaml_waveterm

(* $MDX part-begin=cylon_eye *)
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

(* $MDX part-end *)

open Stdio

(* $MDX part-begin=testbench *)
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

(* $MDX part-end *)

(* $MDX part-begin=cylon_top *)
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

(* $MDX part-end *)

(* $MDX part-begin=cylon_top_test *)
let test () =
  let sim =
    let scope = Scope.create ~flatten_design:true () in
    let leds = cylon_eye_top ~scope ~enable_rate:8 ~clock:(input "clock" 1) in
    Circuit.create_exn
      ~name:"cylon_eye"
      (List.mapi leds ~f:(fun i -> output (Printf.sprintf "led%.2i" i)))
    |> Cyclesim.create ~config:Cyclesim.Config.trace_all
  in
  let waves, sim = Waveform.create sim in
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
│led00             ││╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥   ╥   ╥   ╥   ╥   ╥           │
│                  ││╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───╨───╨───╨───╨───╨───        │
│led01             ││╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥   ╥   ╥   ╥   ╥           │
│                  ││╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───╨───╨───╨───╨───        │
│led02             ││╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥   ╥   ╥   ╥           │
│                  ││╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───╨───╨───╨───        │
│led03             ││╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥   ╥   ╥           │
│                  ││╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───╨───╨───        │
│led04             ││╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥   ╥           │
│                  ││╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───╨───        │
│led05             ││╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥   ╥           │
│                  ││╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───╨───        │
│led06             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥  ╥           │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──╨───        │
│led07             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐ ╥╥          │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─╨╨──        │
│led08             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥ ╥─┐         │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─╨ └─        │
│led09             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐╥─╥         │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └╨ ╨─        │
│led10             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥╥──┐        │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨╨  └        │
│led11             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────╥──╥        │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨       ╨  ╨        │
│led12             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───────        │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨               │
│led13             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥───        │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨           │
│led14             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥           │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───        │
│led15             ││╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥   ╥           │
│                  ││╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───╨───        │
│                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥        │
│count             ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║        │
│                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨        │
│gnd               ││                                                                    │
│                  ││────────────────────────────────────────────────────────────        │
│vdd               ││────────────────────────────────────────────────────────────        │
│                  ││                                                                    │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
23a1bb8cebaa916241e49f8dd3880407
|}]
;;

(* $MDX part-end *)
