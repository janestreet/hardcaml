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
            (ref_name accumulator)
            (lindex   31)
            (rindex   0)))))
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

(* Tests for [Vcd.write_event_based]. *)

(* Build an event-based wave with the given (time, value) pairs.

   Values are interpreted as unsigned ints and converted to [Bits.t] of the requested
   width. *)
let make_event_wave ~name ?(typ = Wave_data.Type.Internal) ?wave_format ~width events =
  let max_time = ref 0 in
  let bits = Wave_data_in_events.Bits.create width max_time in
  let store = Wave_data_in_events.Bits.event_store bits in
  List.iter events ~f:(fun (time, value) ->
    Wave_data_in_events.Bits.Event_store.insert
      store
      time
      (Bits.of_int_trunc ~width value);
    if time > !max_time then max_time := time);
  { Wave_data.Wave.name
  ; width
  ; typ
  ; wave_format =
      (match wave_format with
       | Some f -> f
       | None -> Wave_format.Binary)
  ; is_pseudo_clock = false
  ; wave_data = bits
  }
;;

let%expect_test "write_event_based: simple multi-wave example" =
  let waves =
    [| make_event_wave ~name:"clk" ~typ:Input ~width:1 [ 0, 0; 5, 1; 10, 0; 15, 1; 20, 0 ]
     ; make_event_wave ~name:"in_a" ~typ:Input ~width:8 [ 0, 0; 10, 0xab; 20, 0x42 ]
     ; make_event_wave ~name:"out_x" ~typ:Output ~width:8 [ 5, 0; 15, 0xab; 25, 0x42 ]
     ; make_event_wave ~name:"state" ~typ:Internal ~width:2 [ 0, 0; 10, 1; 20, 2 ]
    |]
  in
  Vcd.write_event_based Stdio.stdout waves;
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

let%expect_test "write_event_based: empty input" =
  Vcd.write_event_based Stdio.stdout [||];
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

let%expect_test "write_event_based: empty scopes are omitted" =
  let waves =
    [| make_event_wave ~name:"a" ~typ:Internal ~width:4 [ 0, 1; 3, 2 ]
     ; make_event_wave ~name:"b" ~typ:Internal ~width:4 [ 1, 5 ]
    |]
  in
  Vcd.write_event_based Stdio.stdout waves;
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

let%expect_test "write_event_based: simultaneous events share a single #time marker" =
  let waves =
    [| make_event_wave ~name:"a" ~typ:Input ~width:1 [ 0, 0; 10, 1 ]
     ; make_event_wave ~name:"b" ~typ:Input ~width:1 [ 0, 1; 10, 0 ]
     ; make_event_wave ~name:"c" ~typ:Output ~width:1 [ 0, 0; 10, 1 ]
    |]
  in
  Vcd.write_event_based Stdio.stdout waves;
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

let%expect_test "write_event_based: hierarchical names are split on $" =
  let waves =
    [| make_event_wave ~name:"top$mod_a$signal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ]
     ; make_event_wave ~name:"top$mod_b$signal" ~typ:Internal ~width:1 [ 0, 1; 5, 0 ]
    |]
  in
  Vcd.write_event_based Stdio.stdout waves;
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

let%expect_test "write_event_based: indexed wave format" =
  let waves =
    [| make_event_wave
         ~name:"state"
         ~typ:Internal
         ~width:2
         ~wave_format:(Wave_format.Index [ "IDLE"; "RUN"; "DONE" ])
         [ 0, 0; 5, 1; 10, 2 ]
    |]
  in
  Vcd.write_event_based Stdio.stdout waves;
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

(* Tests for [Vcd.write_cycle_based]. *)

(* Build a cycle-based wave with the given list of integer values, one per cycle. *)
let make_cycle_wave ~name ?(typ = Wave_data.Type.Internal) ?wave_format ~width values =
  let arr = Array.of_list values in
  let data =
    Wave_data_in_cycles.init (Array.length arr) ~width ~f:(fun i ->
      Bits.of_int_trunc ~width arr.(i))
  in
  { Wave_data.Wave.name
  ; width
  ; typ
  ; wave_format =
      (match wave_format with
       | Some f -> f
       | None -> Wave_format.Binary)
  ; is_pseudo_clock = false
  ; wave_data = data
  }
;;

let%expect_test "write_cycle_based: simple multi-wave example" =
  (* 4 cycles total: index 0 = post-reset, indices 1..3 = three normal cycles. *)
  let waves =
    [| make_cycle_wave ~name:"in_a" ~typ:Input ~width:8 [ 0; 0xab; 0xab; 0x42 ]
     ; make_cycle_wave ~name:"out_x" ~typ:Output ~width:8 [ 0; 0; 0xab; 0xab ]
     ; make_cycle_wave ~name:"state" ~typ:Internal ~width:2 [ 0; 1; 2; 0 ]
    |]
  in
  Vcd.write_cycle_based Stdio.stdout waves;
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
    $var wire 8 # in_a $end
    $upscope $end
    $scope module outputs $end
    $var wire 8 $ out_x $end
    $upscope $end
    $scope module various $end
    $var wire 2 % state $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxxxxxx #
    bxxxxxxxx $
    bxx %
    $end
    #0
    0!
    1"
    b00000000 #
    b00000000 $
    b00 %
    #2
    1!
    0"
    b10101011 #
    b00000000 $
    b01 %
    #3
    0!
    #4
    1!
    0"
    b10101011 $
    b10 %
    #5
    0!
    #6
    1!
    0"
    b01000010 #
    b00 %
    #7
    0!
    |}]
;;

let%expect_test "write_cycle_based: filters waves named clock and reset" =
  let waves =
    [| make_cycle_wave ~name:"clock" ~typ:Input ~width:1 [ 0; 1; 0; 1 ]
     ; make_cycle_wave ~name:"reset" ~typ:Input ~width:1 [ 1; 0; 0; 0 ]
     ; make_cycle_wave ~name:"data" ~typ:Input ~width:4 [ 0; 1; 2; 3 ]
    |]
  in
  Vcd.write_cycle_based Stdio.stdout waves;
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
    $var wire 4 # data $end
    $upscope $end
    $scope module outputs $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxx #
    $end
    #0
    0!
    1"
    b0000 #
    #2
    1!
    0"
    b0001 #
    #3
    0!
    #4
    1!
    0"
    b0010 #
    #5
    0!
    #6
    1!
    0"
    b0011 #
    #7
    0!
    |}]
;;

let%expect_test "write_cycle_based: empty input" =
  Vcd.write_cycle_based Stdio.stdout [||];
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
    $upscope $end
    $scope module outputs $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    $end
    |}]
;;

let%expect_test "write_cycle_based: only reset cycle (length-1 wave)" =
  let waves = [| make_cycle_wave ~name:"a" ~typ:Internal ~width:4 [ 7 ] |] in
  Vcd.write_cycle_based Stdio.stdout waves;
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
    $upscope $end
    $scope module outputs $end
    $upscope $end
    $scope module various $end
    $var wire 4 # a $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxx #
    $end
    #0
    0!
    1"
    b0111 #
    |}]
;;

let%expect_test "write_cycle_based: only changed values are written after the first cycle"
  =
  let waves =
    [| make_cycle_wave ~name:"a" ~typ:Internal ~width:4 [ 0; 1; 1; 1; 2 ]
     ; make_cycle_wave ~name:"b" ~typ:Internal ~width:4 [ 0; 5; 5; 6; 6 ]
    |]
  in
  Vcd.write_cycle_based Stdio.stdout waves;
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
    $upscope $end
    $scope module outputs $end
    $upscope $end
    $scope module various $end
    $var wire 4 # a $end
    $var wire 4 $ b $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxx #
    bxxxx $
    $end
    #0
    0!
    1"
    b0000 #
    b0000 $
    #2
    1!
    0"
    b0001 #
    b0101 $
    #3
    0!
    #4
    1!
    0"
    #5
    0!
    #6
    1!
    0"
    b0110 $
    #7
    0!
    #8
    1!
    0"
    b0010 #
    #9
    0!
    |}]
;;

let%expect_test "write_cycle_based: indexed wave format" =
  let waves =
    [| make_cycle_wave
         ~name:"state"
         ~typ:Internal
         ~width:2
         ~wave_format:(Wave_format.Index [ "IDLE"; "RUN"; "DONE" ])
         [ 0; 1; 2 ]
    |]
  in
  Vcd.write_cycle_based Stdio.stdout waves;
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
    $upscope $end
    $scope module outputs $end
    $upscope $end
    $scope module various $end
    $var wire 32 # state $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx #
    $end
    #0
    0!
    1"
    b01001001010001000100110001000101 #
    #2
    1!
    0"
    b00000000010100100101010101001110 #
    #3
    0!
    #4
    1!
    0"
    b01000100010011110100111001000101 #
    #5
    0!
    |}]
;;

(* Tests for [Vcd.read_event_based]. *)

(* Write [waves] to a temporary VCD file with [write_fn], then parse it back through
   [Vcd.read_event_based] and return the result. *)
let round_trip_via_temp_file ~write_fn waves =
  let filename = Stdlib.Filename.temp_file "hardcaml_vcd_test" ".vcd" in
  Exn.protect
    ~f:(fun () ->
      let chan = Stdio.Out_channel.create filename in
      write_fn chan waves;
      Stdio.Out_channel.close chan;
      Vcd.read_event_based (Hardcaml_vcd.from_file filename))
    ~finally:(fun () -> Stdlib.Sys.remove filename)
;;

(* Print all events in a wave for ease of inspection in expect tests. *)
let print_wave (wave : Wave_data_in_events.Bits.t Wave_data.Wave.t) =
  let store = Wave_data_in_events.Bits.event_store wave.wave_data in
  let n = Wave_data_in_events.Bits.Event_store.length store in
  let events =
    List.init n ~f:(fun i ->
      let t = Wave_data_in_events.Bits.Event_store.get_time_at_index store i in
      let bits = Wave_data_in_events.Bits.Event_store.get_data_at_index store i in
      t, Bits.to_bstr bits)
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

let%expect_test "read_event_based: round-trip event-based VCD" =
  let waves =
    [| make_event_wave ~name:"clk" ~typ:Input ~width:1 [ 0, 0; 5, 1; 10, 0; 15, 1; 20, 0 ]
     ; make_event_wave ~name:"in_a" ~typ:Input ~width:8 [ 0, 0; 10, 0xab; 20, 0x42 ]
     ; make_event_wave ~name:"out_x" ~typ:Output ~width:8 [ 5, 0; 15, 0xab; 25, 0x42 ]
     ; make_event_wave ~name:"state" ~typ:Internal ~width:2 [ 0, 0; 10, 1; 20, 2 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_event_based waves in
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

let%expect_test "read_event_based: hierarchical names round-trip via $" =
  let waves =
    [| make_event_wave ~name:"top$mod_a$signal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ]
     ; make_event_wave ~name:"top$mod_b$signal" ~typ:Internal ~width:1 [ 0, 1; 5, 0 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_event_based waves in
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

let%expect_test "read_event_based: -inputs / -outputs scope rename is reversed" =
  let waves =
    [| make_event_wave ~name:"i$x" ~typ:Internal ~width:4 [ 0, 0; 5, 0xa ]
     ; make_event_wave ~name:"o$y" ~typ:Internal ~width:4 [ 0, 0; 5, 0xb ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_event_based waves in
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

let%expect_test "read_event_based: round-trip cycle-based VCD as events" =
  let waves =
    [| make_cycle_wave ~name:"a" ~typ:Input ~width:4 [ 0; 1; 1; 2 ]
     ; make_cycle_wave ~name:"b" ~typ:Output ~width:4 [ 0; 0; 3; 4 ]
    |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_cycle_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  -clock)
     (width 1)
     (typ   Input)
     (wave_format (Bit_or Hex))
     (events (
       (0 0)
       (2 1)
       (3 0)
       (4 1)
       (5 0)
       (6 1)
       (7 0))))
    ((name  -reset)
     (width 1)
     (typ   Input)
     (wave_format (Bit_or Hex))
     (events (
       (0 1)
       (2 0)
       (4 0)
       (6 0))))
    ((name  a)
     (width 4)
     (typ   Input)
     (wave_format (Bit_or Hex))
     (events (
       (0 0000)
       (2 0001)
       (6 0010))))
    ((name  b)
     (width 4)
     (typ   Output)
     (wave_format (Bit_or Hex))
     (events (
       (0 0000)
       (2 0000)
       (4 0011)
       (6 0100))))
    |}]
;;

let%expect_test "read_event_based: empty inputs / outputs / various scopes round-trip" =
  (* Only [Internal] waves; [inputs] / [outputs] scopes are omitted by the writer, so the
     reader has to handle their absence. *)
  let waves =
    [| make_event_wave ~name:"only_internal" ~typ:Internal ~width:1 [ 0, 0; 5, 1 ] |]
  in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_event_based waves in
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

let%expect_test "read_event_based: empty VCD" =
  let waves = round_trip_via_temp_file ~write_fn:Vcd.write_event_based [||] in
  print_s [%message (Array.length waves : int)];
  [%expect {| ("Array.length waves" 0) |}]
;;

let%expect_test "read_event_based: X bits in $dumpvars are skipped" =
  (* Hardcaml's [write_event_based] writes an all-X [$dumpvars] block at the start. We
     should be able to round-trip such VCDs without error - the X entries are dropped, and
     only events at real timestamps survive. *)
  let waves = [| make_event_wave ~name:"sig" ~typ:Internal ~width:4 [ 0, 5; 5, 6 ] |] in
  let waves' = round_trip_via_temp_file ~write_fn:Vcd.write_event_based waves in
  Array.iter waves' ~f:print_wave;
  [%expect
    {|
    ((name  sig)
     (width 4)
     (typ   Internal)
     (wave_format (Bit_or Hex))
     (events (
       (0 0101)
       (5 0110))))
    |}]
;;

let%expect_test "read_event_based: X bits in mid-simulation value changes raise" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var wire 1 ! sig $end
$upscope $end
$enddefinitions $end
#0
0!
#5
x!
|}
  in
  require_does_raise (fun () ->
    Vcd.read_event_based (Hardcaml_vcd.from_string vcd_string));
  [%expect
    {|
    ("[Vcd.read_event_based]: cannot represent VCD X/Z bit in a [Bits.t] value"
     (bit Vx))
    |}]
;;

let%expect_test "read_event_based: $dumpoff regions raise" =
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
  require_does_raise (fun () ->
    Vcd.read_event_based (Hardcaml_vcd.from_string vcd_string));
  [%expect
    {| "[Vcd.read_event_based]: [$dumpoff] regions are not supported (X / Z values cannot be represented in [Bits.t])" |}]
;;

let%expect_test "read_event_based: real-valued signals raise" =
  let vcd_string =
    {|$timescale 1ns $end
$scope module various $end
$var real 64 ! sig $end
$upscope $end
$enddefinitions $end
#0
r1.5 !
|}
  in
  require_does_raise (fun () ->
    Vcd.read_event_based (Hardcaml_vcd.from_string vcd_string));
  [%expect
    {|
    ("[Vcd.read_event_based]: real-valued VCD signals are not supported"
     (vc (Real_value 1.5 !)))
    |}]
;;

let%expect_test "read_event_based: short vector values are zero-padded to width" =
  (* The VCD spec lets short vectors be left-padded with the leading bit. We support the
     V0-leading case; any leading X / Z would be unrepresentable and rejected separately. *)
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
  let waves = Vcd.read_event_based (Hardcaml_vcd.from_string vcd_string) in
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
  Hardcaml_waveterm_kernel.Waveform.print waves ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
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
    | By_cycle waves -> waves
    | By_event _ -> failwith ""
  in
  let vcdfile = to_tmp_file (fun chan -> Vcd.write_cycle_based chan cycle_waves) in
  let waves = Vcd.read_event_based (Hardcaml_vcd.from_file vcdfile) in
  (* There is a scaling difference here - cyclesim waveforms have a timestep of 1 per
     cycle (so 0.5 per half cycle which we can't represent in vcd). The vcd is 2 per
     cycle. *)
  Hardcaml_waveterm_kernel.Waveform.print (By_event waves) ~wave_width:0;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clear          ││                                                   │
    │               ││──────                                             │
    │-clock         ││    ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                │
    │               ││────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─               │
    │-reset         ││────┐                                              │
    │               ││    └─────────────────────────────                 │
    │               ││────────────┬───────────┬─                         │
    │a              ││ 00         │0A         │.                         │
    │               ││────────────┴───────────┴─                         │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬─                 │
    │b              ││ 00 │0A │14 │00 │0A │14 │00 │0A │.                 │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴─                 │
    │               ││────────┬───┬───┬───┬───┬───┬───┬─                 │
    │c              ││ 00     │0A │14 │0A │14 │1E │14 │.                 │
    │               ││────────┴───┴───┴───┴───┴───┴───┴─                 │
    │               ││────────────┬───┬───┬───┬───┬───┬─                 │
    │d              ││ 00         │F6 │EC │0A │00 │F6 │.                 │
    │               ││────────────┴───┴───┴───┴───┴───┴─                 │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
