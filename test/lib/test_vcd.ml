(* generate a small vcd file as an expect test *)

open! Import

let%expect_test "simple vcd file" =
  let module Sim = Cyclesim in
  let module S = Cyclesim in
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  let a, b = input "a" 8, input "b" 8 in
  let c, d =
    reg reg_spec ~enable:vdd (a +: b), pipeline reg_spec ~enable:vdd ~n:2 (a -: b)
  in
  let c, d = output "c" c, output "d" d in
  let circ = Circuit.create_exn ~name:"test" [ c; d ] in
  let sim = Sim.create circ in
  let sim = Vcd.wrap Stdio.stdout sim in
  let a, b = S.in_port sim "a", S.in_port sim "b" in
  for i = 0 to 2 do
    for j = 0 to 2 do
      a := Bits.of_int_trunc ~width:8 (i * 10);
      b := Bits.of_int_trunc ~width:8 (j * 10);
      S.cycle sim
    done
  done;
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
  let module Sim = Cyclesim in
  let module S = Cyclesim in
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
  let sim = Sim.create circ in
  let sim = Vcd.wrap Stdio.stdout sim in
  List.iter ports ~f:(fun (name, width) ->
    let port = S.in_port sim name in
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
      S.cycle sim;
      S.cycle sim));
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
  let module Sim = Cyclesim in
  let module S = Cyclesim in
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
  let sim = Sim.create ~config:Sim.Config.trace_all circ in
  let sim = Vcd.wrap Stdio.stdout sim in
  let a = S.in_port sim "a" in
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
  a := Bits.of_int_trunc ~width:4 0;
  S.cycle sim;
  a := Bits.of_int_trunc ~width:4 2;
  S.cycle sim;
  a := Bits.of_int_trunc ~width:4 4;
  S.cycle sim;
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
