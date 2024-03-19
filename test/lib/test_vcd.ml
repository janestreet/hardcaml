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
      a := Bits.of_int ~width:8 (i * 10);
      b := Bits.of_int ~width:8 (j * 10);
      S.cycle sim
    done
  done;
  [%expect
    {|
    $date
      ...
    $end
    $version
      Hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! clock $end
    $var wire 1 " reset $end
    $var wire 1 # clear $end
    $var wire 8 % b $end
    $var wire 8 & a $end
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
    x#
    bxxxxxxxx %
    bxxxxxxxx &
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
