open! Core

let () =
  Command.run
  @@ Command.group
       ~summary:"Hardcaml Synthesis reports"
       [ "clz", Count_leading_zeros.command ]
;;
