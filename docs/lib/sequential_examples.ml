open Base
open Hardcaml
open Hardcaml_waveterm
open Signal

(* https://www.chipverify.com/verilog/verilog-examples *)

module Jk_flip_flop = struct
  type fn = clock:Signal.t -> j:Signal.t -> k:Signal.t -> Signal.t

  (* $MDX part-begin=jk_flip_flop_1 *)
  let jk_flip_flop_1 ~clock ~j ~k =
    let q = Always.Variable.reg (Reg_spec.create ~clock ()) ~width:1 in
    Always.(
      compile
        [ switch
            (j @: k)
            [ of_string "00", [ q <-- q.value ]
            ; of_string "01", [ q <-- gnd ]
            ; of_string "10", [ q <-- vdd ]
            ; of_string "11", [ q <-- ~:(q.value) ]
            ]
        ]);
    q.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=jk_flip_flop_2 *)
  let jk_flip_flop_2 ~clock ~j ~k =
    reg_fb (Reg_spec.create ~clock ()) ~width:1 ~f:(fun q ->
      mux (j @: k) [ q; gnd; vdd; ~:q ])
  ;;

  (* $MDX part-end *)

  let test f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"jk_flip_flop"
           [ output "q" (f ~clock:(input "clock" 1) ~j:(input "j" 1) ~k:(input "k" 1)) ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let j = Cyclesim.in_port sim "j" in
    let k = Cyclesim.in_port sim "k" in
    List.iter Bool.all ~f:(fun k' ->
      List.iter Bool.all ~f:(fun j' ->
        j := Bits.of_bool j';
        k := Bits.of_bool k';
        Cyclesim.cycle sim));
    Cyclesim.cycle sim;
    waves
  ;;

  let%expect_test "jk_flip_flop" =
    Waveform.expect_exact (test jk_flip_flop_1);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                               │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───                            │
│k                 ││                ┌───────────────────────                            │
│                  ││────────────────┘                                                   │
│j                 ││        ┌───────┐       ┌───────────────                            │
│                  ││────────┘       └───────┘                                           │
│q                 ││                ┌───────┐       ┌───────                            │
│                  ││────────────────┘       └───────┘                                   │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
5e4648d8e6a0904ae644bee3628c50fc
|}];
    Waveform.expect_exact (test jk_flip_flop_2);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                               │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───                            │
│k                 ││                ┌───────────────────────                            │
│                  ││────────────────┘                                                   │
│j                 ││        ┌───────┐       ┌───────────────                            │
│                  ││────────┘       └───────┘                                           │
│q                 ││                ┌───────┐       ┌───────                            │
│                  ││────────────────┘       └───────┘                                   │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
5e4648d8e6a0904ae644bee3628c50fc
|}]
  ;;
end

module T_flip_flop = struct
  type fn = clock:Signal.t -> reset_n:Signal.t -> t:Signal.t -> Signal.t

  (* $MDX part-begin=t_flip_flop_1 *)
  let t_flip_flop_1 ~clock ~reset_n ~t =
    let q =
      Always.Variable.reg
        (Reg_spec.create ~clock ~reset:reset_n ~reset_level:Low ())
        ~width:1
    in
    Always.(compile [ if_ t [ q <-- ~:(q.value) ] [ q <-- q.value ] ]);
    q.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=t_flip_flop_2 *)
  let t_flip_flop_2 ~clock ~reset_n ~t =
    reg_fb
      (Reg_spec.create ~clock ~reset:reset_n ~reset_level:Low ())
      ~width:1
      ~enable:t
      ~f:( ~: )
  ;;

  (* $MDX part-end *)

  let test f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"t_flip_flop"
           [ output
               "q"
               (f ~clock:(input "clock" 1) ~reset_n:(input "reset_n" 1) ~t:(input "t" 1))
           ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let rstn = Cyclesim.in_port sim "reset_n" in
    let t = Cyclesim.in_port sim "t" in
    rstn := Bits.gnd;
    Cyclesim.reset sim;
    rstn := Bits.vdd;
    t := Bits.gnd;
    Cyclesim.cycle sim;
    t := Bits.vdd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    t := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    waves
  ;;

  let%expect_test "t flip flop" =
    Waveform.expect_exact (test t_flip_flop_1);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐               │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───            │
│reset_n           ││        ┌───────────────────────────────────────────────            │
│                  ││────────┘                                                           │
│t                 ││                ┌───────────────────────┐                           │
│                  ││────────────────┘                       └───────────────            │
│q                 ││                        ┌───────┐       ┌───────────────            │
│                  ││────────────────────────┘       └───────┘                           │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
cd246554e2fec7fc55da77924e5ec5e7
|}];
    Waveform.expect_exact (test t_flip_flop_2);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐               │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───            │
│t                 ││                ┌───────────────────────┐                           │
│                  ││────────────────┘                       └───────────────            │
│reset_n           ││        ┌───────────────────────────────────────────────            │
│                  ││────────┘                                                           │
│q                 ││                        ┌───────┐       ┌───────────────            │
│                  ││────────────────────────┘       └───────┘                           │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
cd246554e2fec7fc55da77924e5ec5e7
|}]
  ;;
end

module D_flip_flop = struct
  type fn =
    clock:Signal.t
    -> reset:Signal.t
    -> clear:Signal.t
    -> enable:Signal.t
    -> d:Signal.t
    -> Signal.t

  (* $MDX part-begin=d_flip_flop *)
  let d_flip_flop ~clock ~reset ~clear ~enable ~d =
    reg (Reg_spec.create ~clock ~reset ~clear ()) ~enable d
  ;;
  (* $MDX part-end *)
end

module Ring_counter = struct
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  (* $MDX part-begin=ring_counter *)
  let ring_counter ~n ~clock ~clear =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~clear_to:(one n) ~f:(rotr ~by:1)
  ;;

  (* $MDX part-end *)

  let test () =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"ring_counter"
           [ output
               "q"
               (ring_counter ~n:4 ~clock:(input "clock" 1) ~clear:(input "clear" 1))
           ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let clear = Cyclesim.in_port sim "clear" in
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    Cyclesim.cycle ~n:6 sim;
    waves
  ;;

  let%expect_test "ring counter" =
    Waveform.expect_exact (test ());
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐               │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───            │
│clear             ││────────┐                                                           │
│                  ││        └───────────────────────────────────────────────            │
│                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────            │
│q                 ││ 0      │1      │8      │4      │2      │1      │8                  │
│                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────            │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
9e408b17ac3e9528a19d72f9febe1643
|}]
  ;;
end

module Mobius_counter = struct
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  (* $MDX part-begin=mobius_counter *)
  let mobius_counter ~n ~clock ~clear =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~f:(fun d -> ~:(lsb d) @: msbs d)
  ;;

  (* $MDX part-end *)

  let test () =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"mobius_counter"
           [ output
               "q"
               (mobius_counter ~n:4 ~clock:(input "clk" 1) ~clear:(input "clear" 1))
           ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let clear = Cyclesim.in_port sim "clear" in
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    Cyclesim.cycle ~n:12 sim;
    waves
  ;;

  let%expect_test "mobius counter" =
    Waveform.expect_exact ~wave_scale:2 (test ());
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clk               ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
│clear             ││──────┐                                                             │
│                  ││      └─────────────────────────────────────────────────────────────│
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
│q                 ││ 0          │8    │C    │E    │F    │7    │3    │1    │0    │8    │C│
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
823a02c976824c852f264f110d5f69f5
|}]
  ;;
end

module Modulo_n_counter = struct
  type config =
    { width : int
    ; n : int
    }

  type fn =
    config:config -> clock:Signal.t -> clear:Signal.t -> increment:Signal.t -> Signal.t

  (* $MDX part-begin=modulo_n_counter_1 *)
  let modulo_n_counter_1 ~config:{ width; n } ~clock ~clear ~increment =
    let spec = Reg_spec.create ~clock ~clear () in
    let out = Always.Variable.reg spec ~width in
    Always.(
      compile
        [ when_
            increment
            [ if_ (out.value ==:. n - 1) [ out <--. 0 ] [ out <-- out.value +:. 1 ] ]
        ]);
    out.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=modulo_n_counter_2 *)
  let modulo_n_counter_2 ~config:{ width; n } ~clock ~clear ~increment =
    reg_fb
      (Reg_spec.create ~clock ~clear ())
      ~enable:increment
      ~width
      ~f:(mod_counter ~max:(n - 1))
  ;;

  (* $MDX part-end *)

  let test f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"modulo_n_counter"
           [ output
               "q"
               (f
                  ~config:{ width = 3; n = 5 }
                  ~clock:(input "clock" 1)
                  ~clear:(input "clear" 1)
                  ~increment:(input "increment" 1))
           ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let clear = Cyclesim.in_port sim "clear" in
    let increment = Cyclesim.in_port sim "increment" in
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    for _ = 1 to 30 do
      increment := Bits.random ~width:1;
      Cyclesim.cycle sim
    done;
    waves
  ;;

  let%expect_test "modulo_n counter" =
    Waveform.expect_exact ~wave_scale:0 (test modulo_n_counter_1);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││─┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐      │
│                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└      │
│clear             ││──┐                                                                 │
│                  ││  └───────────────────────────────────────────────────────────      │
│increment         ││  ┌─┐ ┌─┐ ┌───┐ ┌───┐           ┌─┐ ┌─┐ ┌─┐ ┌─┐         ┌─┐         │
│                  ││──┘ └─┘ └─┘   └─┘   └───────────┘ └─┘ └─┘ └─┘ └─────────┘ └───      │
│                  ││────┬───┬───┬─┬───┬─┬─────────────┬───┬───┬───┬───────────┬───      │
│q                 ││ 0  │1  │2  │3│4  │0│1            │2  │3  │4  │0          │1        │
│                  ││────┴───┴───┴─┴───┴─┴─────────────┴───┴───┴───┴───────────┴───      │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
ffc2c380a99aa1168c482797ac867922
|}];
    Waveform.expect_exact ~wave_scale:0 (test modulo_n_counter_2);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││─┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐      │
│                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└      │
│clear             ││──┐                                                                 │
│                  ││  └───────────────────────────────────────────────────────────      │
│increment         ││  ┌─┐     ┌─┐ ┌───────────┐ ┌─────┐ ┌─────┐ ┌─┐   ┌─┐     ┌─┐       │
│                  ││──┘ └─────┘ └─┘           └─┘     └─┘     └─┘ └───┘ └─────┘ └─      │
│                  ││────┬───────┬───┬─┬─┬─┬─┬─┬───┬─┬─┬───┬─┬─┬───┬─────┬───────┬─      │
│q                 ││ 0  │1      │2  │3│4│0│1│2│3  │4│0│1  │2│3│4  │0    │1      │2      │
│                  ││────┴───────┴───┴─┴─┴─┴─┴─┴───┴─┴─┴───┴─┴─┴───┴─────┴───────┴─      │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
ce78dc168aa7d62bfad7ce9d2262b6c1
|}]
  ;;
end

module Gray_counter = struct
  type fn = n:int -> clock:Signal.t -> clear:Signal.t -> Signal.t

  (* $MDX part-begin=gray_counter_1 *)
  let gray_counter_1 ~n ~clock ~clear =
    let spec = Reg_spec.create ~clock ~clear () in
    let q = Always.Variable.reg spec ~width:n in
    let out = Always.Variable.reg spec ~width:n in
    Always.(
      compile
        [ q <-- q.value +:. 1
        ; out <-- q.value.:(n - 1) @: q.value.:[n - 1, 1] ^: q.value.:[n - 2, 0]
        ]);
    out.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=gray_counter_2 *)
  let gray_counter_2 ~n ~clock ~clear =
    let spec = Reg_spec.create ~clock ~clear () in
    let q = reg_fb spec ~width:n ~f:(fun q -> q +:. 1) in
    reg spec (msb q @: msbs q ^: lsbs q)
  ;;

  (* $MDX part-end *)

  let test f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"gray_counter"
           [ output "out" (f ~n:4 ~clock:(input "clock" 1) ~clear:(input "clear" 1)) ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let clear = Cyclesim.in_port sim "clear" in
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    Cyclesim.cycle ~n:18 sim;
    waves
  ;;

  let%expect_test "gray counter" =
    Waveform.expect_exact ~wave_scale:1 (test gray_counter_1);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│clear             ││────┐                                                               │
│                  ││    └───────────────────────────────────────────────────────────────│
│                  ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
│out               ││ 0          │1  │3  │2  │6  │7  │5  │4  │C  │D  │F  │E  │A  │B  │9  │
│                  ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
d50b210748d2c276dfc43f04ad7afc43
|}];
    Waveform.expect_exact ~wave_scale:1 (test gray_counter_2);
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
│                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
│clear             ││────┐                                                               │
│                  ││    └───────────────────────────────────────────────────────────────│
│                  ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
│out               ││ 0          │1  │3  │2  │6  │7  │5  │4  │C  │D  │F  │E  │A  │B  │9  │
│                  ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
d50b210748d2c276dfc43f04ad7afc43
|}]
  ;;
end

module Bidirectional_shift_reg = struct
  type fn =
    n:int
    -> clock:Signal.t
    -> clear:Signal.t
    -> enable:Signal.t
    -> dir:Signal.t
    -> d:Signal.t
    -> Signal.t

  (* $MDX part-begin=bidirectional_shift_reg_1 *)
  let bidirectional_shift_reg_1 ~n ~clock ~clear ~enable ~dir ~d =
    let out = Always.Variable.reg (Reg_spec.create ~clock ~clear ()) ~width:n in
    Always.(
      compile
        [ when_
            enable
            [ if_
                dir
                [ out <-- d @: out.value.:[n - 1, 1] ]
                [ out <-- out.value.:[n - 2, 0] @: d ]
            ]
        ]);
    out.value
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=bidirectional_shift_reg_2 *)
  let bidirectional_shift_reg_2 ~n ~clock ~clear ~enable ~dir ~d =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~enable ~f:(fun out ->
      mux2 dir (d @: msbs out) (lsbs out @: d))
  ;;

  (* $MDX part-end *)

  let test f =
    let sim =
      Cyclesim.create
        (Circuit.create_exn
           ~name:"bidirection_shift_reg"
           [ output
               "out"
               (f
                  ~n:4
                  ~clock:(input "clock" 1)
                  ~clear:(input "clear" 1)
                  ~enable:(input "enable" 1)
                  ~dir:(input "dir" 1)
                  ~d:(input "d" 1))
           ])
    in
    let waves, sim = Cyclesim.Waveform.create sim in
    let d = Cyclesim.in_port sim "d" in
    let en = Cyclesim.in_port sim "enable" in
    let dir = Cyclesim.in_port sim "dir" in
    let clear = Cyclesim.in_port sim "clear" in
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    en := Bits.vdd;
    dir := Bits.gnd;
    d := Bits.gnd;
    for _ = 0 to 3 do
      d := Bits.( ~: ) !d;
      Cyclesim.cycle sim
    done;
    dir := Bits.vdd;
    for _ = 0 to 3 do
      d := Bits.( ~: ) !d;
      Cyclesim.cycle sim
    done;
    en := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    waves
  ;;

  let display_rules = Display_rule.[ port_name_is "out" ~wave_format:Binary; default ]

  let%expect_test "bidirectional shift reg" =
    Waveform.expect_exact ~wave_scale:2 (test bidirectional_shift_reg_1) ~display_rules;
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────  │
│out               ││ 0000       │0001 │0010 │0101 │1010 │1101 │0110 │1011 │0101         │
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────  │
│clear             ││──────┐                                                             │
│                  ││      └───────────────────────────────────────────────────────────  │
│clock             ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐    │
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──  │
│d                 ││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐                   │
│                  ││──────┘     └─────┘     └─────┘     └─────┘     └─────────────────  │
│dir               ││                              ┌───────────────────────────────────  │
│                  ││──────────────────────────────┘                                     │
│enable            ││      ┌───────────────────────────────────────────────┐             │
│                  ││──────┘                                               └───────────  │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
68640203b63ade7f4650e00ec528b9e7
|}];
    Waveform.expect_exact ~wave_scale:2 (test bidirectional_shift_reg_2) ~display_rules;
    [%expect_exact
      {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────  │
│out               ││ 0000       │0001 │0010 │0101 │1010 │1101 │0110 │1011 │0101         │
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────  │
│clear             ││──────┐                                                             │
│                  ││      └───────────────────────────────────────────────────────────  │
│clock             ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐    │
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──  │
│d                 ││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐                   │
│                  ││──────┘     └─────┘     └─────┘     └─────┘     └─────────────────  │
│dir               ││                              ┌───────────────────────────────────  │
│                  ││──────────────────────────────┘                                     │
│enable            ││      ┌───────────────────────────────────────────────┐             │
│                  ││──────┘                                               └───────────  │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
68640203b63ade7f4650e00ec528b9e7
|}]
  ;;
end

module Single_port_ram = struct
  (* $MDX part-begin=single_port_ram *)
  let single_port_ram ~clock ~address ~write_enable ~write_data =
    let spec = Reg_spec.create ~clock () in
    (multiport_memory
       (Int.pow 2 (width address))
       ~write_ports:
         [| { write_clock = clock; write_enable; write_address = address; write_data } |]
       ~read_addresses:[| address |]).(0)
    |> reg spec
  ;;
  (* $MDX part-end *)
end

module Sync_fifo = struct
  let log_depth = 3
  let depth = Int.pow 2 log_depth

  (* $MDX part-begin=sync_fifo *)
  type t =
    { data_out : Signal.t
    ; full : Signal.t
    ; empty : Signal.t
    }

  let sync_fifo ~clock ~clear ~write ~read ~data_in =
    let spec = Reg_spec.create ~clock ~clear () in
    let wptr = wire log_depth in
    let wptr_next = wptr +:. 1 in
    let rptr = wire log_depth in
    let full = wptr_next ==: rptr in
    let empty = wptr ==: rptr in
    let write_enable = write &: ~:full in
    let read_enable = read &: ~:empty in
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:depth
        ~write_ports:
          [| { write_clock = clock
             ; write_data = data_in
             ; write_enable
             ; write_address = wptr
             }
          |]
        ~read_ports:[| { read_clock = clock; read_enable; read_address = rptr } |]
        ()
    in
    wptr <-- reg spec ~enable:write_enable wptr_next;
    rptr <-- reg spec ~enable:read_enable (rptr +:. 1);
    { data_out = mem.(0); full; empty }
  ;;
  (* $MDX part-end *)
end
