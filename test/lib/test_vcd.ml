(* generate a small vcd file as an expect test *)

open! Import

let create () =
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  let a, b = input "a" 8, input "b" 8 in
  let c, d =
    reg reg_spec ~enable:vdd (a +: b), pipeline reg_spec ~enable:vdd ~n:2 (a -: b)
  in
  let c, d = output "c" c, output "d" d in
  let circ = Circuit.create_exn ~name:"test" [ c; d ] in
  let sim = Cyclesim.create circ in
  sim
;;

let run sim =
  let open Cyclesim.Sim_bits in
  let a, b = Cyclesim.in_port sim "a", Cyclesim.in_port sim "b" in
  for i = 0 to 2 do
    for j = 0 to 2 do
      a <-- of_int_trunc ~width:8 (i * 10);
      b <-- of_int_trunc ~width:8 (j * 10);
      Cyclesim.cycle sim
    done
  done
;;

let%expect_test "simple vcd file" =
  let sim = create () |> Vcd.wrap Stdio.stdout in
  run sim;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-cyclesim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! -clock $end
    $var wire 1 " -reset $end
    $var wire 8 & a $end
    $var wire 8 % b $end
    $var wire 1 # clear $end
    $upscope $end
    $scope module outputs $end
    $var wire 8 ' c $end
    $var wire 8 ( d $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxxxxxx &
    bxxxxxxxx %
    x#
    bxxxxxxxx '
    bxxxxxxxx (
    $end
    #0
    1!
    0"
    0#
    b00000000 %
    b00000000 &
    b00000000 '
    b00000000 (
    #5
    0!
    #10
    1!
    0"
    b00001010 %
    #15
    0!
    #20
    1!
    0"
    b00010100 %
    b00001010 '
    #25
    0!
    #30
    1!
    0"
    b00000000 %
    b00001010 &
    b00010100 '
    b11110110 (
    #35
    0!
    #40
    1!
    0"
    b00001010 %
    b00001010 '
    b11101100 (
    #45
    0!
    #50
    1!
    0"
    b00010100 %
    b00010100 '
    b00001010 (
    #55
    0!
    #60
    1!
    0"
    b00000000 %
    b00010100 &
    b00011110 '
    b00000000 (
    #65
    0!
    #70
    1!
    0"
    b00001010 %
    b00010100 '
    b11110110 (
    #75
    0!
    #80
    1!
    0"
    b00010100 %
    b00011110 '
    b00010100 (
    #85
    0!
    |}]
;;

let%expect_test "show generated identifiers" =
  print_s
    [%message
      (Char.of_int_exn Vcd.Var.Generator.min_id_char : char)
        (Char.of_int_exn Vcd.Var.Generator.max_id_char : char)
        (Vcd.Var.Generator.id_char_range : int)];
  let generator = Vcd.Var.Generator.create () in
  let generate_block verbose =
    for i = 0 to Vcd.Var.Generator.id_char_range - 1 do
      let id = Vcd.Var.Generator.next generator in
      if verbose
      then (
        Stdio.printf "%s " id;
        if i > 0 && i % 10 = 0 then Stdio.printf "\n")
    done;
    if verbose then Stdio.printf "\n\n"
  in
  for _ = 0 to 2 do
    generate_block true
  done;
  for _ = 3 to Vcd.Var.Generator.id_char_range - 1 do
    generate_block false
  done;
  generate_block true;
  generate_block true;
  generate_block true;
  [%expect
    {|
    (("Char.of_int_exn Vcd.Var.Generator.min_id_char" !)
     ("Char.of_int_exn Vcd.Var.Generator.max_id_char" ~)
     (Vcd.Var.Generator.id_char_range                 94))
    ! " # $ % & ' ( ) * +
    , - . / 0 1 2 3 4 5
    6 7 8 9 : ; < = > ?
    @ A B C D E F G H I
    J K L M N O P Q R S
    T U V W X Y Z [ \ ]
    ^ _ ` a b c d e f g
    h i j k l m n o p q
    r s t u v w x y z {
    | } ~

    !! "! #! $! %! &! '! (! )! *! +!
    ,! -! .! /! 0! 1! 2! 3! 4! 5!
    6! 7! 8! 9! :! ;! <! =! >! ?!
    @! A! B! C! D! E! F! G! H! I!
    J! K! L! M! N! O! P! Q! R! S!
    T! U! V! W! X! Y! Z! [! \! ]!
    ^! _! `! a! b! c! d! e! f! g!
    h! i! j! k! l! m! n! o! p! q!
    r! s! t! u! v! w! x! y! z! {!
    |! }! ~!

    !" "" #" $" %" &" '" (" )" *" +"
    ," -" ." /" 0" 1" 2" 3" 4" 5"
    6" 7" 8" 9" :" ;" <" =" >" ?"
    @" A" B" C" D" E" F" G" H" I"
    J" K" L" M" N" O" P" Q" R" S"
    T" U" V" W" X" Y" Z" [" \" ]"
    ^" _" `" a" b" c" d" e" f" g"
    h" i" j" k" l" m" n" o" p" q"
    r" s" t" u" v" w" x" y" z" {"
    |" }" ~"

    !~ "~ #~ $~ %~ &~ '~ (~ )~ *~ +~
    ,~ -~ .~ /~ 0~ 1~ 2~ 3~ 4~ 5~
    6~ 7~ 8~ 9~ :~ ;~ <~ =~ >~ ?~
    @~ A~ B~ C~ D~ E~ F~ G~ H~ I~
    J~ K~ L~ M~ N~ O~ P~ Q~ R~ S~
    T~ U~ V~ W~ X~ Y~ Z~ [~ \~ ]~
    ^~ _~ `~ a~ b~ c~ d~ e~ f~ g~
    h~ i~ j~ k~ l~ m~ n~ o~ p~ q~
    r~ s~ t~ u~ v~ w~ x~ y~ z~ {~
    |~ }~ ~~

    !!! "!! #!! $!! %!! &!! '!! (!! )!! *!! +!!
    ,!! -!! .!! /!! 0!! 1!! 2!! 3!! 4!! 5!!
    6!! 7!! 8!! 9!! :!! ;!! <!! =!! >!! ?!!
    @!! A!! B!! C!! D!! E!! F!! G!! H!! I!!
    J!! K!! L!! M!! N!! O!! P!! Q!! R!! S!!
    T!! U!! V!! W!! X!! Y!! Z!! [!! \!! ]!!
    ^!! _!! `!! a!! b!! c!! d!! e!! f!! g!!
    h!! i!! j!! k!! l!! m!! n!! o!! p!! q!!
    r!! s!! t!! u!! v!! w!! x!! y!! z!! {!!
    |!! }!! ~!!

    !"! ""! #"! $"! %"! &"! '"! ("! )"! *"! +"!
    ,"! -"! ."! /"! 0"! 1"! 2"! 3"! 4"! 5"!
    6"! 7"! 8"! 9"! :"! ;"! <"! ="! >"! ?"!
    @"! A"! B"! C"! D"! E"! F"! G"! H"! I"!
    J"! K"! L"! M"! N"! O"! P"! Q"! R"! S"!
    T"! U"! V"! W"! X"! Y"! Z"! ["! \"! ]"!
    ^"! _"! `"! a"! b"! c"! d"! e"! f"! g"!
    h"! i"! j"! k"! l"! m"! n"! o"! p"! q"!
    r"! s"! t"! u"! v"! w"! x"! y"! z"! {"!
    |"! }"! ~"!
    |}]
;;

let%expect_test "validate generated identifiers" =
  let generator = Vcd.Var.Generator.create () in
  let rec gen_ids set n =
    if n = 0
    then set
    else (
      let id = Vcd.Var.Generator.next generator in
      if Set.mem set id then raise_s [%message "idenfifier already in set" (id : string)];
      gen_ids (Set.add set id) (n - 1))
  in
  let num_identifiers = 100_000 in
  let set = gen_ids (Set.empty (module String)) num_identifiers in
  if Set.length set <> num_identifiers
  then raise_s [%message "Created repeated identifiers"]
;;

let%expect_test "test with wide signals to ensure byte comparison works properly" =
  let open Signal in
  let ports = [ "a", 127; "b", 128 ] in
  (* Prevent the input from being optimized out *)
  let output =
    List.map ports ~f:(fun (name, width) -> input name width)
    |> concat_lsb
    |> bits_lsb
    |> reduce ~f:( &: )
    |> output "out"
  in
  let circ = Circuit.create_exn ~name:"test" [ output ] in
  let sim = Cyclesim.create circ in
  let sim = Vcd.wrap Stdio.stdout sim in
  List.iter ports ~f:(fun (name, width) ->
    let port = Cyclesim.in_port sim name in
    let values =
      [ Bits.ones width
      ; Bits.zero width
      ; Bits.concat_lsb [ Bits.vdd; Bits.zero (width - 1) ]
        (* Set one bit at the LSB of the signal *)
      ; Bits.zero width
      ; Bits.concat_lsb [ Bits.zero (width - 1); Bits.vdd ]
        (* Set one bit at the MSB of the signal *)
      ; Bits.zero width
      ; Bits.concat_lsb (List.init width ~f:(fun i -> Bits.of_bool (i = width / 2)))
        (* Set one bit roughly in the middle of the signal *)
      ; Bits.zero width
      ]
    in
    List.iter values ~f:(fun value ->
      port := value;
      (* Cycle twice to make sure the signal is only printed to the VCD when it changes *)
      Cyclesim.cycle sim;
      Cyclesim.cycle sim));
  (* This VCD should demonstrate the following behavior:
     - Signals are initialized to X
     - For the first signal, the following updates are seen (with one cycle of no updates
       in between each update)
       - All ones
       - All zeros
       - Only last bit is one
       - All zeros
       - Only first bit is one
       - All zeros
       - One bit in the middle is one
       - All zeros
     - For the second signal, the same sequence of updates is seen
  *)
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-cyclesim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! -clock $end
    $var wire 1 " -reset $end
    $var wire 127 # a $end
    $var wire 128 $ b $end
    $upscope $end
    $scope module outputs $end
    $var wire 1 % out $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx #
    bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx $
    x%
    $end
    #0
    1!
    0"
    b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 #
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    0%
    #5
    0!
    #10
    1!
    0"
    #15
    0!
    #20
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
    #25
    0!
    #30
    1!
    0"
    #35
    0!
    #40
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001 #
    #45
    0!
    #50
    1!
    0"
    #55
    0!
    #60
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
    #65
    0!
    #70
    1!
    0"
    #75
    0!
    #80
    1!
    0"
    b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
    #85
    0!
    #90
    1!
    0"
    #95
    0!
    #100
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
    #105
    0!
    #110
    1!
    0"
    #115
    0!
    #120
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000 #
    #125
    0!
    #130
    1!
    0"
    #135
    0!
    #140
    1!
    0"
    b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
    #145
    0!
    #150
    1!
    0"
    #155
    0!
    #160
    1!
    0"
    b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 $
    #165
    0!
    #170
    1!
    0"
    #175
    0!
    #180
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    #185
    0!
    #190
    1!
    0"
    #195
    0!
    #200
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001 $
    #205
    0!
    #210
    1!
    0"
    #215
    0!
    #220
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    #225
    0!
    #230
    1!
    0"
    #235
    0!
    #240
    1!
    0"
    b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    #245
    0!
    #250
    1!
    0"
    #255
    0!
    #260
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    #265
    0!
    #270
    1!
    0"
    #275
    0!
    #280
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000 $
    #285
    0!
    #290
    1!
    0"
    #295
    0!
    #300
    1!
    0"
    b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 $
    #305
    0!
    #310
    1!
    0"
    #315
    0!
    |}]
;;

let%expect_test "custom wave format to string" =
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  let a = input "a" 4 in
  (* Create an internal signal to apply the wave format to *)
  let tmp = wireof a in
  let tmp = tmp -- "tmp" in
  let tmp = tmp --$ Wave_format.Index [ "hello"; "world"; "test" ] in
  let b = reg reg_spec tmp in
  let b = output "b" b in
  let circ = Circuit.create_exn ~name:"test" [ b ] in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circ in
  let sim = Vcd.wrap Stdio.stdout sim in
  let a = Cyclesim.in_port sim "a" in
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-cyclesim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! -clock $end
    $var wire 1 " -reset $end
    $var wire 4 % a $end
    $var wire 1 # clear $end
    $upscope $end
    $scope module outputs $end
    $var wire 4 & b $end
    $upscope $end
    $scope module various $end
    $var wire 40 ' tmp $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxx %
    x#
    bxxxx &
    bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx '
    $end
    |}];
  let open Cyclesim.Sim_bits in
  a <-- of_int_trunc ~width:4 0;
  Cyclesim.cycle sim;
  a <-- of_int_trunc ~width:4 2;
  Cyclesim.cycle sim;
  a <-- of_int_trunc ~width:4 4;
  Cyclesim.cycle sim;
  (* Strings are 'hello', 'test', and '?' as expected. They are padded with zeros on the
     left, which is empirically consistent with SystemVerilog and works in GTKwave *)
  [%expect
    {|
    #0
    1!
    0"
    0#
    b0000 %
    b0000 &
    b0110100001100101011011000110110001101111 '
    #5
    0!
    #10
    1!
    0"
    b0010 %
    b0000000001110100011001010111001101110100 '
    #15
    0!
    #20
    1!
    0"
    b0100 %
    b0010 &
    b0000000000000000000000000000000000111111 '
    #25
    0!
    |}]
;;

let vcd =
  {|
  $date June 26, 1989 10:05:41
    $end
    $version VERILOG-SIMULATOR 1.0a
    $end
    $timescale 1 ns
    $end
    $scope module top $end
    $scope module m1  $end
    $var trireg 1 *@ net1 $end
    $var trireg 1 *# net2 $end
    $var trireg 1 *$ net3 $end
    $upscope $end
    $scope task t1 $end
    $var reg 32 (k accumulator[31:0] $end
    $var integer 32 {2 index  $end
    $upscope $end
    $upscope $end
    $enddefinitions $end
    #500
    $dumpvars
    x*@
    x*#
    x*$
    bx (k
    bx {2
    $end
    #505
    0*@
    1*#
    1*$
    b10zx1110x11100 (k
    b1111000101z01x {2
    #510
    0*$
    #520
    1*$
    #530
    0*$
    bz (k
    #535
    $dumpall   0*@   1*#   0*$
    bz (k
    b1111000101z01x {2
    $end
    #540
    1*$
    #1000
    $dumpoff
    x*@
    x*#
    x*$
    bx (k
    bx {2
    $end
    #2000
    $dumpon
    z*@
    1*#
    0*$
    b0 (k
    bx {2
    $end
    #2010
    1*$
    $comment
      sim-time annotation
    $end
    #2020
    0*$

  |}
;;

let%expect_test "parse vcd with empty dump blocks" =
  (* IEEE 1364 allows zero-or-more value changes inside [$dumpvars] / [$dumpall] /
     [$dumpon] / [$dumpoff], so the parser must accept empty bodies. *)
  let vcd =
    {|
    $enddefinitions $end
    #0
    $dumpvars $end
    $dumpall $end
    $dumpon $end
    $dumpoff $end
    |}
  in
  print_s [%message (Hardcaml_vcd.from_string vcd : Hardcaml_vcd.t)];
  [%expect
    {|
    ("Hardcaml_vcd.from_string vcd" (
      (declarations (Enddefinitions))
      (simulation_commands (
        (Sim_time 0)
        (Sim_dumpvars ())
        (Sim_dumpall  ())
        (Sim_dumpon   ())
        (Sim_dumpoff  ())))))
    |}]
;;

let%expect_test "parse vcd with special characters" =
  let vcd =
    {|
    $scope module top $end
    $var wire 1 ! :jasper_formal_reset $end
    $var wire 8 " signal_mux_12[7:0] $end
    $var wire 1 # clocking$clear $end
    $enddefinitions $end
    |}
  in
  print_s [%message (Hardcaml_vcd.from_string vcd : Hardcaml_vcd.t)];
  [%expect
    {|
    ("Hardcaml_vcd.from_string vcd" (
      (declarations (
        (Scope Module top)
        (Var (
          (var_type Wire)
          (var_size 1)
          (var_id   !)
          (var_ref (
            (ref_name :jasper_formal_reset)
            (lindex   -1)
            (rindex   -1)))))
        (Var (
          (var_type Wire)
          (var_size 8)
          (var_id   "\"")
          (var_ref (
            (ref_name signal_mux_12[7:0])
            (lindex   -1)
            (rindex   -1)))))
        (Var (
          (var_type Wire)
          (var_size 1)
          (var_id   #)
          (var_ref (
            (ref_name clocking$clear)
            (lindex   -1)
            (rindex   -1)))))
        Enddefinitions))
      (simulation_commands ())))
    |}]
;;

let%expect_test "parse vcd" =
  print_s [%message (Hardcaml_vcd.from_string vcd : Hardcaml_vcd.t)];
  [%expect
    {|
    ("Hardcaml_vcd.from_string vcd" (
      (declarations (
        (Date    "June 26, 1989 10:05:41\n    ")
        (Version "VERILOG-SIMULATOR 1.0a\n    ")
        (Timescale 1      Ns)
        (Scope     Module top)
        (Scope     Module m1)
        (Var (
          (var_type Trireg)
          (var_size 1)
          (var_id   *@)
          (var_ref (
            (ref_name net1)
            (lindex   -1)
            (rindex   -1)))))
        (Var (
          (var_type Trireg)
          (var_size 1)
          (var_id   *#)
          (var_ref (
            (ref_name net2)
            (lindex   -1)
            (rindex   -1)))))
        (Var (
          (var_type Trireg)
          (var_size 1)
          (var_id   *$)
          (var_ref (
            (ref_name net3)
            (lindex   -1)
            (rindex   -1)))))
        Upscope
        (Scope Task t1)
        (Var (
          (var_type Reg)
          (var_size 32)
          (var_id   "(k")
          (var_ref (
            (ref_name accumulator[31:0])
            (lindex   -1)
            (rindex   -1)))))
        (Var (
          (var_type Integer)
          (var_size 32)
          (var_id   {2)
          (var_ref (
            (ref_name index)
            (lindex   -1)
            (rindex   -1)))))
        Upscope
        Upscope
        Enddefinitions))
      (simulation_commands (
        (Sim_time 500)
        (Sim_dumpvars (
          (Scalar_value Vx *@)
          (Scalar_value Vx *#)
          (Scalar_value Vx *$)
          (Vector_value (Vx) "(k")
          (Vector_value (Vx) {2)))
        (Sim_time 505)
        (Sim_value_change (Scalar_value V0 *@))
        (Sim_value_change (Scalar_value V1 *#))
        (Sim_value_change (Scalar_value V1 *$))
        (Sim_value_change (
          Vector_value (V1 V0 Vz Vx V1 V1 V1 V0 Vx V1 V1 V1 V0 V0) "(k"))
        (Sim_value_change (
          Vector_value (V1 V1 V1 V1 V0 V0 V0 V1 V0 V1 Vz V0 V1 Vx) {2))
        (Sim_time 510)
        (Sim_value_change (Scalar_value V0 *$))
        (Sim_time 520)
        (Sim_value_change (Scalar_value V1 *$))
        (Sim_time 530)
        (Sim_value_change (Scalar_value V0 *$))
        (Sim_value_change (Vector_value (Vz) "(k"))
        (Sim_time 535)
        (Sim_dumpall (
          (Scalar_value V0 *@)
          (Scalar_value V1 *#)
          (Scalar_value V0 *$)
          (Vector_value (Vz) "(k")
          (Vector_value (V1 V1 V1 V1 V0 V0 V0 V1 V0 V1 Vz V0 V1 Vx) {2)))
        (Sim_time 540)
        (Sim_value_change (Scalar_value V1 *$))
        (Sim_time 1000)
        (Sim_dumpoff (
          (Scalar_value Vx *@)
          (Scalar_value Vx *#)
          (Scalar_value Vx *$)
          (Vector_value (Vx) "(k")
          (Vector_value (Vx) {2)))
        (Sim_time 2000)
        (Sim_dumpon (
          (Scalar_value Vz *@)
          (Scalar_value V1 *#)
          (Scalar_value V0 *$)
          (Vector_value (V0) "(k")
          (Vector_value (Vx) {2)))
        (Sim_time 2010)
        (Sim_value_change (Scalar_value V1 *$))
        (Sim_comment "sim-time annotation\n    ")
        (Sim_time    2020)
        (Sim_value_change (Scalar_value V0 *$))))))
    |}]
;;

(* Tests for [Vcd.write_mutable_event_based]. *)

(* Build a mutable-event wave with the given (time, value) pairs.

   Values are interpreted as unsigned ints and converted to [Bits.t] of the requested
   width. *)
let make_event_wave ~name ?(typ = Wave_data.Type.Internal) ?wave_format ~width events =
  let max_time = ref 0 in
  let bits = Wave_data_in_mutable_events.create width max_time in
  List.iter events ~f:(fun (time, value) ->
    Wave_data_in_mutable_events.insert bits time (Bits.of_int_trunc ~width value);
    if time > !max_time then max_time := time);
  { Wave_data.Wave.name
  ; width
  ; typ
  ; wave_format =
      (match wave_format with
       | Some f -> f
       | None -> Wave_format.Binary)
  ; wave_data = bits
  }
;;

let%expect_test "write_mutable_event_based: simple multi-wave example" =
  let waves =
    [| make_event_wave ~name:"clk" ~typ:Input ~width:1 [ 0, 0; 5, 1; 10, 0; 15, 1; 20, 0 ]
     ; make_event_wave ~name:"in_a" ~typ:Input ~width:8 [ 0, 0; 10, 0xab; 20, 0x42 ]
     ; make_event_wave ~name:"out_x" ~typ:Output ~width:8 [ 5, 0; 15, 0xab; 25, 0x42 ]
     ; make_event_wave ~name:"state" ~typ:Internal ~width:2 [ 0, 0; 10, 1; 20, 2 ]
    |]
  in
  Vcd.write_mutable_event_based Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! clk $end
    $var wire 8 " in_a $end
    $upscope $end
    $scope module outputs $end
    $var wire 8 # out_x $end
    $upscope $end
    $scope module various $end
    $var wire 2 $ state $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    bxxxxxxxx "
    bxxxxxxxx #
    bxx $
    $end
    #0
    b00 $
    b00000000 "
    0!
    #5
    1!
    b00000000 #
    #10
    b01 $
    b10101011 "
    0!
    #15
    1!
    b10101011 #
    #20
    b10 $
    b01000010 "
    0!
    #25
    b01000010 #
    |}]
;;

let%expect_test "write_mutable_event_based: empty input" =
  Vcd.write_mutable_event_based Stdio.stdout [||];
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $enddefinitions $end
    $dumpvars
    $end
    |}]
;;

let%expect_test "write_mutable_event_based: empty scopes are omitted" =
  let waves =
    [| make_event_wave ~name:"a" ~typ:Internal ~width:4 [ 0, 1; 3, 2 ]
     ; make_event_wave ~name:"b" ~typ:Internal ~width:4 [ 1, 5 ]
    |]
  in
  Vcd.write_mutable_event_based Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module various $end
    $var wire 4 ! a $end
    $var wire 4 " b $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    bxxxx !
    bxxxx "
    $end
    #0
    b0001 !
    #1
    b0101 "
    #3
    b0010 !
    |}]
;;

let%expect_test "write_mutable_event_based: simultaneous events share a single #time \
                 marker"
  =
  let waves =
    [| make_event_wave ~name:"a" ~typ:Input ~width:1 [ 0, 0; 10, 1 ]
     ; make_event_wave ~name:"b" ~typ:Input ~width:1 [ 0, 1; 10, 0 ]
     ; make_event_wave ~name:"c" ~typ:Output ~width:1 [ 0, 0; 10, 1 ]
    |]
  in
  Vcd.write_mutable_event_based Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! a $end
    $var wire 1 " b $end
    $upscope $end
    $scope module outputs $end
    $var wire 1 # c $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    x#
    $end
    #0
    0#
    1"
    0!
    #10
    1!
    1#
    0"
    |}]
;;

let%expect_test "write_mutable_event_based: hierarchical names are split on $" =
  let waves =
    [| make_event_wave ~name:"top$mod_a$signal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ]
     ; make_event_wave ~name:"top$mod_b$signal" ~typ:Internal ~width:1 [ 0, 1; 5, 0 ]
    |]
  in
  Vcd.write_mutable_event_based Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module various $end
    $scope module top $end
    $scope module mod_a $end
    $var wire 1 ! signal $end
    $upscope $end
    $scope module mod_b $end
    $var wire 1 " signal $end
    $upscope $end
    $upscope $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    $end
    #0
    1"
    0!
    #5
    1!
    0"
    |}]
;;

let%expect_test "write_mutable_event_based: indexed wave format" =
  let waves =
    [| make_event_wave
         ~name:"state"
         ~typ:Internal
         ~width:2
         ~wave_format:(Wave_format.Index [ "IDLE"; "RUN"; "DONE" ])
         [ 0, 0; 5, 1; 10, 2 ]
    |]
  in
  Vcd.write_mutable_event_based Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module various $end
    $var wire 32 ! state $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx !
    $end
    #0
    b01001001010001000100110001000101 !
    #5
    b00000000010100100101010101001110 !
    #10
    b01000100010011110100111001000101 !
    |}]
;;

(* Tests for [Vcd.read_mutable_event_based_four_state], the one VCD reader. *)

(* Write [waves] to a temporary VCD file with [write_fn], then parse it back and return
   the result. Reading a VCD always produces 4-state wave data, whatever was written. *)
let round_trip_via_temp_file ~write_fn waves =
  let filename = Stdlib.Filename.temp_file "hardcaml_vcd_test" ".vcd" in
  Exn.protect
    ~f:(fun () ->
      let chan = Stdio.Out_channel.create filename in
      write_fn chan waves;
      Stdio.Out_channel.close chan;
      Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_file filename))
    ~finally:(fun () -> Stdlib.Sys.remove filename)
;;

(* Print all events in a wave for ease of inspection in expect tests. Values are shown in
   "01XZ" notation. *)
let print_wave (wave : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t) =
  let events =
    List.init
      (Wave_data_in_mutable_events_four_state.num_events wave.wave_data)
      ~f:(fun i ->
        let t =
          Wave_data_in_mutable_events_four_state.get_time_at_index wave.wave_data i
        in
        let bits =
          Wave_data_in_mutable_events_four_state.get_data_at_index wave.wave_data i
        in
        let mask =
          Wave_data_in_mutable_events_four_state.get_mask_at_index wave.wave_data i
        in
        t, Four_state_logic.create_exn ~bits ~mask |> Four_state_logic.to_string)
  in
  print_s
    [%message
      ""
        ~name:(wave.name : string)
        ~width:(wave.width : int)
        ~typ:(wave.typ : Wave_data.Type.t)
        ~wave_format:(wave.wave_format : Wave_format.t)
        (events : (int * string) list)]
;;

let%expect_test "read: all waves use final VCD time as length" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 1 ! a $end
$var wire 1 " b $end
$upscope $end
$enddefinitions $end
#0
0!
0"
#3
1!
#10
1"
#20
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:(fun wave ->
    print_s
      [%message
        (wave.name : string)
          ~length:(Wave_data_in_mutable_events_four_state.length wave.wave_data : int)]);
  [%expect
    {|
    ((wave.name a)
     (length    21))
    ((wave.name b)
     (length    21))
    |}]
;;

let%expect_test "read: round-trip event-based VCD" =
  let waves =
    [| make_event_wave ~name:"clk" ~typ:Input ~width:1 [ 0, 0; 5, 1; 10, 0; 15, 1; 20, 0 ]
     ; make_event_wave ~name:"in_a" ~typ:Input ~width:8 [ 0, 0; 10, 0xab; 20, 0x42 ]
     ; make_event_wave ~name:"out_x" ~typ:Output ~width:8 [ 5, 0; 15, 0xab; 25, 0x42 ]
     ; make_event_wave ~name:"state" ~typ:Internal ~width:2 [ 0, 0; 10, 1; 20, 2 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  clk)
     (width 1)
     (typ   Input)
     (wave_format (Bit_or Hex))
     (events (
       (0  0)
       (5  1)
       (10 0)
       (15 1)
       (20 0))))
    ((name  in_a)
     (width 8)
     (typ   Input)
     (wave_format (Bit_or Hex))
     (events (
       (0  00000000)
       (10 10101011)
       (20 01000010))))
    ((name  out_x)
     (width 8)
     (typ   Output)
     (wave_format (Bit_or Hex))
     (events (
       (0  XXXXXXXX)
       (5  00000000)
       (15 10101011)
       (25 01000010))))
    ((name  state)
     (width 2)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0  00)
       (10 01)
       (20 10))))
    |}]
;;

let%expect_test "read: hierarchical names round-trip via $" =
  let waves =
    [| make_event_wave ~name:"top$mod_a$signal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ]
     ; make_event_wave ~name:"top$mod_b$signal" ~typ:Internal ~width:1 [ 0, 1; 5, 0 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  top$mod_a$signal)
     (width 1)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0)
       (5 1))))
    ((name  top$mod_b$signal)
     (width 1)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 1)
       (5 0))))
    |}]
;;

let%expect_test "read: -inputs / -outputs scope rename is reversed" =
  let waves =
    [| make_event_wave ~name:"i$x" ~typ:Internal ~width:4 [ 0, 0; 5, 0xa ]
     ; make_event_wave ~name:"o$y" ~typ:Internal ~width:4 [ 0, 0; 5, 0xb ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  i$x)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0000)
       (5 1010))))
    ((name  o$y)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0000)
       (5 1011))))
    |}]
;;

let%expect_test "read: empty inputs / outputs / various scopes round-trip" =
  (* Only [Internal] waves; [inputs] / [outputs] scopes are omitted by the writer, so the
     reader has to handle their absence. *)
  let waves =
    [| make_event_wave ~name:"only_internal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ] |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  only_internal)
     (width 1)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0)
       (5 1))))
    |}]
;;

let%expect_test "read: empty VCD" =
  let waves = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based [||] in
  print_s [%message (Array.length waves : int)];
  [%expect {| ("Array.length waves" 0) |}]
;;

let%expect_test "read: signals are X until their first driven value" =
  (* Hardcaml's writers emit an all-X [$dumpvars] block before the first [#time], so a
     signal whose first event is after time 0 reads back as X up to that point. [early],
     which is driven at time 0, has its X overwritten. *)
  let waves =
    [| make_event_wave ~name:"early" ~typ:Internal ~width:4 [ 0, 5; 5, 6 ]
     ; make_event_wave ~name:"late" ~typ:Internal ~width:4 [ 5, 6 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_mutable_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  early)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0101)
       (5 0110))))
    ((name  late)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 XXXX)
       (5 0110))))
    |}]
;;

let%expect_test "read: $dumpoff regions record the values they list" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 1 ! sig $end
$upscope $end
$enddefinitions $end
#0
0!
#5
$dumpoff
x!
$end
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:print_wave;
  [%expect
    {|
    ((name  sig)
     (width 1)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0)
       (5 X))))
    |}]
;;

let%expect_test "read: aliased identifiers share wave data" =
  (* External simulators such as Xcelium dump a signal under every name it is visible as,
     sharing one identifier between the [$var]s. *)
  let vcd_string =
    {|$timescale 1ns $end
$scope module top $end
$var wire 4 ! port $end
$var wire 4 ! net $end
$var wire 4 " other $end
$upscope $end
$enddefinitions $end
#0
b0101 !
b0001 "
#5
b0110 !
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:print_wave;
  print_s
    [%message
      ""
        ~port_and_net_share_wave_data:
          (phys_equal waves.(0).wave_data waves.(1).wave_data : bool)
        ~port_and_other_share_wave_data:
          (phys_equal waves.(0).wave_data waves.(2).wave_data : bool)];
  [%expect
    {|
    ((name  top$port)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0101)
       (5 0110))))
    ((name  top$net)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0101)
       (5 0110))))
    ((name  top$other)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events ((0 0001))))
    ((port_and_net_share_wave_data   true)
     (port_and_other_share_wave_data false))
    |}]
;;

let%expect_test "read: aliased identifiers with different widths raise" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module top $end
$var wire 4 ! port $end
$var wire 8 ! net $end
$upscope $end
$enddefinitions $end
#0
b0101 !
|}
  in
  require_does_raise (fun () ->
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string));
  [%expect
    {|
    ("[Vcd.read_mutable_event_based_four_state]: VCD identifier declared with inconsistent widths"
     (var_id !)
     (widths (4 8)))
    |}]
;;

(* Print a real-valued wave, formatting each event's bits with the wave's [Custom] format. *)
let print_formatted_wave
  (wave : Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t)
  =
  let format =
    match wave.wave_format with
    | Custom f -> f
    | Index strings ->
      fun bits ->
        (match List.nth strings (Bits.to_int_trunc bits) with
         | Some s -> s
         | None -> Bits.to_string bits)
    | _ -> fun bits -> Bits.to_string bits
  in
  let events =
    List.init
      (Wave_data_in_mutable_events_four_state.num_events wave.wave_data)
      ~f:(fun i ->
        ( Wave_data_in_mutable_events_four_state.get_time_at_index wave.wave_data i
        , format
            (Wave_data_in_mutable_events_four_state.get_data_at_index wave.wave_data i) ))
  in
  print_s
    [%message
      ""
        ~name:(wave.name : string)
        ~width:(wave.width : int)
        (events : (int * string) list)]
;;

let%expect_test "read: real-valued signals are displayed" =
  (* Real signals have no natural [Bits.t] encoding, so they are stored as the IEEE bits
     of each double (whatever the declared [$var] size) and displayed with a [%g] format.
     Simulators write reals in the full range of [%g] output: signs, bare or trailing
     decimal points, and exponents. *)
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var real 1 ! temperature $end
$var real 64 " negative $end
$var real 64 # exponent $end
$var real 64 $ trailing_point $end
$var wire 4 % bit_sig $end
$upscope $end
$enddefinitions $end
#0
r1.5 !
r-1.5 "
r1.5e-09 #
r2. $
b0101 %
#5
r-7.044325338467268e+20 !
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:(fun wave ->
    match wave.wave_format with
    | Custom _ -> print_formatted_wave wave
    | _ -> print_wave wave);
  [%expect
    {|
    ((name  temperature)
     (width 64)
     (events (
       (0 1.5)
       (5 -7.04433e+20))))
    ((name  negative)
     (width 64)
     (events ((0 -1.5))))
    ((name  exponent)
     (width 64)
     (events ((0 1.5e-09))))
    ((name  trailing_point)
     (width 64)
     (events ((0 2))))
    ((name  bit_sig)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events ((0 0101))))
    |}]
;;

let%expect_test "read: string-valued signals are interned and displayed" =
  (* VHDL simulators dump enum and string signals as [$var string] with [s...] values,
     escaping awkward characters as [\nnn] octal. Each signal's strings are interned in
     first-seen order, the wave stores indexes, and the completed table becomes the wave's
     [Index] format. *)
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var string 0 ! state $end
$var string 0 " message $end
$upscope $end
$enddefinitions $end
#0
sidle !
sEn\040l\345ng\040r\366d\040r\344v "
#5
srunning !
s "
#10
sidle !
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:(fun wave ->
    print_formatted_wave wave;
    print_s [%sexp (wave.wave_format : Wave_format.t)]);
  [%expect
    {|
    ((name  state)
     (width 32)
     (events (
       (0  idle)
       (5  running)
       (10 idle))))
    (Index (idle running))
    ((name  message)
     (width 32)
     (events (
       (0 "En l\229ng r\246d r\228v")
       (5 ""))))
    (Index ("En l\229ng r\246d r\228v" ""))
    |}]
;;

let%expect_test "read: value changes of the wrong kind raise" =
  (* One VCD per mismatch: a real value on a bit signal, a bit value on a real signal, and
     a string value on a real signal (MyHDL dumps enum state names on [$var real] signals,
     which we reject rather than guess at). *)
  let read_string vcd_string =
    require_does_raise (fun () ->
      Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string))
  in
  read_string
    {|$timescale 1ns $end
$scope module various $end
$var wire 4 ! sig $end
$upscope $end
$enddefinitions $end
#0
r1.5 !
|};
  [%expect
    {|
    ("[Vcd.read_mutable_event_based_four_state]: value change does not match the signal's declared type"
     (vc (Real_value 1.5 !))
     (declared Bits))
    |}];
  read_string
    {|$timescale 1ns $end
$scope module various $end
$var real 1 ! sig $end
$upscope $end
$enddefinitions $end
#0
b0101 !
|};
  [%expect
    {|
    ("[Vcd.read_mutable_event_based_four_state]: value change does not match the signal's declared type"
     (vc (Vector_value (V0 V1 V0 V1) !))
     (declared Real))
    |}];
  read_string
    {|$timescale 1ns $end
$scope module various $end
$var real 1 ! state $end
$upscope $end
$enddefinitions $end
#0
scount !
|};
  [%expect
    {|
    ("[Vcd.read_mutable_event_based_four_state]: value change does not match the signal's declared type"
     (vc (String_value count !))
     (declared Real))
    |}]
;;

let%expect_test "read: simulator quirks" =
  (* A grab-bag of things real simulators write that stray from the VCD spec: an unknown
     [$attrbegin] command (nvc), a non-power-of-ten timescale (gameroy), a scope name with
     brackets (Xcelium), an SV [logic] var type, an escaped identifier containing brackets
     (Vivado, amaranth), an unterminated [$dumpvars] (treadle), VHDL logic values (GHDL:
     L/H are weak 0/1; U, W and - map to X), and a fractional timestamp, which is
     truncated (migen). *)
  let vcd_string =
    {|$timescale 244 ns $end
$attrbegin misc 03 foo 1 $end
$scope module top[3] $end
$var logic 4 ! \counter[0] [3:0] $end
$upscope $end
$enddefinitions $end
$dumpvars
b01HL !
#2.5
bUW-Z !
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:print_wave;
  [%expect
    {|
    ((name  "top[3]$\\counter[0]")
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0110)
       (2 XXXZ))))
    |}]
;;

let%expect_test "read: short vector values are zero-padded to width" =
  (* The VCD spec lets short vectors be left-padded with the leading bit; see below for
     the X / Z leading cases. *)
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 8 ! sig $end
$upscope $end
$enddefinitions $end
#0
b101 !
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  Array.iter waves ~f:print_wave;
  [%expect
    {|
    ((name  sig)
     (width 8)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events ((0 00000101))))
    |}]
;;

let to_tmp_file f =
  let filename = Stdlib.Filename.temp_file "hardcaml_vcd_test" ".vcd" in
  Stdio.Out_channel.with_file filename ~f:(fun chan -> f chan);
  filename
;;

let%expect_test "test roundtrips with waveterm" =
  let sim = create () in
  let waves, sim = Cyclesim.Waveform.create sim in
  run sim;
  Hardcaml_waveterm_kernel.Waveform.print waves ~wave_scale:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                │
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─               │
    │clear          ││                                                   │
    │               ││────────────────────────────────────               │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬───               │
    │b              ││ 00 │0A │14 │00 │0A │14 │00 │0A │14                │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴───               │
    │               ││────────────┬───────────┬───────────               │
    │a              ││ 00         │0A         │14                        │
    │               ││────────────┴───────────┴───────────               │
    │               ││────────┬───┬───┬───┬───┬───┬───┬───               │
    │c              ││ 00     │0A │14 │0A │14 │1E │14 │1E                │
    │               ││────────┴───┴───┴───┴───┴───┴───┴───               │
    │               ││────────────┬───┬───┬───┬───┬───┬───               │
    │d              ││ 00         │F6 │EC │0A │00 │F6 │14                │
    │               ││────────────┴───┴───┴───┴───┴───┴───               │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  let cycle_waves =
    match waves with
    | By_mutable_event waves -> waves
    | By_mutable_event_four_state _ -> raise_s [%message "expected a 2-state waveform"]
  in
  let vcdfile =
    to_tmp_file (fun chan -> Vcd.write_mutable_event_based chan cycle_waves)
  in
  let waves = Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_file vcdfile) in
  (* Reading a VCD always produces 4-state data. Every signal here is driven at time 0, so
     demoting back to 2-state recovers exactly what was written. *)
  Hardcaml_waveterm_kernel.Waveform.print
    (Wave_data.to_two_state (By_mutable_event_four_state waves))
    ~wave_scale:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                │
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─               │
    │clear          ││                                                   │
    │               ││────────────────────────────────────               │
    │               ││────────────┬───────────┬───────────               │
    │a              ││ 00         │0A         │14                        │
    │               ││────────────┴───────────┴───────────               │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬───               │
    │b              ││ 00 │0A │14 │00 │0A │14 │00 │0A │14                │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴───               │
    │               ││────────┬───┬───┬───┬───┬───┬───┬───               │
    │c              ││ 00     │0A │14 │0A │14 │1E │14 │1E                │
    │               ││────────┴───┴───┴───┴───┴───┴───┴───               │
    │               ││────────────┬───┬───┬───┬───┬───┬───               │
    │d              ││ 00         │F6 │EC │0A │00 │F6 │14                │
    │               ││────────────┴───┴───┴───┴───┴───┴───               │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

(* Tests for the 4-state VCD writer, and reader tests that need 4-state input. *)

module Events_four_state = Wave_data_in_mutable_events_four_state

let make_four_state_wave ~name ?(typ = Wave_data.Type.Internal) ?wave_format ~width events
  =
  let max_time = ref 0 in
  let wave_data = Events_four_state.create width max_time in
  List.iter events ~f:(fun (time, string) ->
    let value = Four_state_logic.of_string string in
    Events_four_state.insert
      wave_data
      time
      ~bits:(Four_state_logic.bits value)
      ~mask:(Four_state_logic.mask value);
    if time > !max_time then max_time := time);
  { Wave_data.Wave.name
  ; width
  ; typ
  ; wave_format = Option.value wave_format ~default:Wave_format.Binary
  ; wave_data
  }
;;

let%expect_test "write_mutable_event_based_four_state: X/Z values in 01XZ notation" =
  let waves =
    [| make_four_state_wave ~name:"a" ~width:4 [ 0, "XXXX"; 5, "01ZX"; 10, "0110" ]
     ; make_four_state_wave ~name:"b" ~width:1 [ 0, "X"; 5, "Z"; 10, "1" ]
    |]
  in
  Vcd.write_mutable_event_based_four_state Stdio.stdout waves;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module various $end
    $var wire 4 ! a $end
    $var wire 1 " b $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    bxxxx !
    x"
    $end
    #0
    x"
    bxxxx !
    #5
    b01zx !
    z"
    #10
    1"
    b0110 !
    |}]
;;

let%expect_test "read: 4-state round trip" =
  let waves =
    [| make_four_state_wave ~name:"a" ~width:4 [ 0, "XXXX"; 5, "01ZX"; 10, "0110" ]
     ; make_four_state_wave ~name:"b" ~width:1 [ 0, "X"; 5, "Z"; 10, "1" ]
    |]
  in
  let filename = Stdlib.Filename.temp_file "hardcaml_vcd_test" ".vcd" in
  Exn.protect
    ~f:(fun () ->
      let chan = Stdio.Out_channel.create filename in
      Vcd.write_mutable_event_based_four_state chan waves;
      Stdio.Out_channel.close chan;
      let waves' =
        Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_file filename)
      in
      print_s [%sexp (waves' : Events_four_state.t Wave_data.Wave.t array)])
    ~finally:(fun () -> Stdlib.Sys.remove filename);
  [%expect
    {|
    (((name  a)
      (width 4)
      (typ   Internal)
      (wave_format (Bit_or Hex))
      (wave_data (
        (time (0    5    10))
        (data (XXXX 01ZX 0110))
        (num_events 3)
        (width      4)
        (max_time   10))))
     ((name  b)
      (width 1)
      (typ   Internal)
      (wave_format (Bit_or Hex))
      (wave_data (
        (time (0 5 10))
        (data (X Z 1))
        (num_events 3)
        (width      1)
        (max_time   10)))))
    |}]
;;

let%expect_test "read: dump-block initialization values are kept, and overwritten by a \
                 value change at the same time"
  =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 4 ! sig $end
$upscope $end
$enddefinitions $end
$dumpvars
bxxxx !
$end
#0
b0101 !
#5
bz10x !
#7
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  print_s [%sexp (waves : Events_four_state.t Wave_data.Wave.t array)];
  [%expect
    {|
    ((
      (name  sig)
      (width 4)
      (typ   Internal)
      (wave_format (Bit_or Hex))
      (wave_data (
        (time (0    5))
        (data (0101 Z10X))
        (num_events 2)
        (width      4)
        (max_time   7)))))
    |}]
;;

let%expect_test "read: short vector values left-extend with the leading bit for X/Z" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 4 ! sig $end
$upscope $end
$enddefinitions $end
#0
bx1 !
#2
bz0 !
#4
b10 !
#5
|}
  in
  let waves =
    Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_string vcd_string)
  in
  print_s [%sexp (waves : Events_four_state.t Wave_data.Wave.t array)];
  [%expect
    {|
    ((
      (name  sig)
      (width 4)
      (typ   Internal)
      (wave_format (Bit_or Hex))
      (wave_data (
        (time (0    2    4))
        (data (XXX1 ZZZ0 0010))
        (num_events 3)
        (width      4)
        (max_time   5)))))
    |}]
;;
