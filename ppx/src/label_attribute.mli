(** Attributes that can be specified on a given field in a record declaration used to
    define a hardcaml interface. *)
open Ppxlib

type t

val has_exists : loc:'a -> label_declaration -> bool
val get_exists : loc:location -> label_declaration -> expression
val get_bits_opt : loc:location -> label_declaration -> expression option
val get_bits_with_default : loc:location -> label_declaration -> expression
val get_length : loc:location -> label_declaration -> expression
val get_wave_format : loc:location -> label_declaration -> expression
val get_rtlname : loc:location -> label -> label_declaration -> expression

val get_rtlprefix
  :  loc:'a
  -> opts:Interface_options.t
  -> label_declaration
  -> expression option

val get_rtlsuffix
  :  loc:'a
  -> opts:Interface_options.t
  -> label_declaration
  -> expression option

val get_rtlmangle
  :  loc:'a
  -> opts:Interface_options.t
  -> label_declaration
  -> expression option

val get_doc : loc:location -> label_declaration -> label option
