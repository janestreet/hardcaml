open Hardcaml

val print
  :  ?attribute:[ `auto | `gray | `johnson | `none | `one_hot | `sequential ]
  -> ?encoding:Hardcaml.Always.State_machine.Encoding.t
  -> string
  -> Rtl.Language.t
  -> unit
