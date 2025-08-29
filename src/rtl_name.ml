open! Core0

module type Language = sig
  val legalize : string -> string
  val case_sensitive : bool
  val reserved_words : string list
end

module Verilog_base = struct
  let is_valid_first_char c = Char.is_alpha c || Char.equal c '_'
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$'
  let replace_with = '_'
  let prefix = "_"

  let rec legalize string =
    if String.is_empty string
    then raise_s [%message "[Rtl_name.legalize] string is empty"];
    if is_valid_first_char string.[0]
    then String.map string ~f:(fun c -> if is_valid_other_char c then c else replace_with)
    else legalize (prefix ^ string)
  ;;

  let case_sensitive = true
end

module Verilog = struct
  include Verilog_base

  let reserved_words = Reserved_words.verilog
end

module Systemverilog = struct
  include Verilog_base

  let reserved_words = Reserved_words.systemverilog
end

module Vhdl = struct
  (* Here are the (bonkers) rules.

     1 identifiers can contain only upper or lower case letters a-z, numerals 0-9, and underscore
     2 the first character must be a letter
     3 the last character cannot be an underscore
     4 two successive underscores are not allowed
     5 identifiers cannot contain spaces (i.e. the space character). Use underscore instead.
  *)

  let is_valid_first_char c = Char.is_alpha c
  let is_valid_last_char c = Char.is_alphanum c
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_'
  let replace_with = '_'
  let prefix = "hc_"
  let suffix = "_hc"
  let double_underscores = String.Search_pattern.create "__"
  let case_sensitive = false

  let rec remove_double_underscores in_ =
    let out = String.Search_pattern.replace_all double_underscores ~in_ ~with_:"_" in
    if String.equal in_ out then out else remove_double_underscores out
  ;;

  let rec legalize string =
    if String.is_empty string
    then raise_s [%message "[Rtl_name.legalize] string is empty"];
    if not (is_valid_first_char string.[0])
    then legalize (prefix ^ string)
    else if not (is_valid_last_char string.[String.length string - 1])
    then legalize (string ^ suffix)
    else
      String.map string ~f:(fun c -> if is_valid_other_char c then c else replace_with)
      |> remove_double_underscores
  ;;

  let reserved_words =
    Reserved_words.vhdl
    @ [ prefix ^ "sgn"; prefix ^ "uns"; prefix ^ "sl"; prefix ^ "slv"; "to_integer" ]
  ;;
end

type t =
  { mangler : Mangler.t
  ; instantiation_mangler : Mangler.t
  ; lang : (module Language)
  }
[@@deriving fields ~getters]

let create (module Lang : Language) =
  let mangler = Mangler.create ~case_sensitive:Lang.case_sensitive in
  let instantiation_mangler = Mangler.create ~case_sensitive:Lang.case_sensitive in
  Mangler.add_identifiers_exn mangler Lang.reserved_words;
  { mangler; instantiation_mangler; lang = (module Lang) }
;;

let add_port_name { mangler; instantiation_mangler = _; lang = (module Lang) } signal name
  =
  let legal_name = Lang.legalize name in
  if not (String.equal legal_name name)
  then
    raise_s
      [%message
        "[Rtl_name.add_port_name] illegal port name"
          (name : string)
          ~legal_name
          ~note:"Hardcaml will not change ports names."
          ~port:(signal : Signal.t)];
  match Mangler.add_identifier mangler name with
  | `Duplicate ->
    raise_s
      [%message
        "[Rtl_name.add_port_name] port name has already been defined or matches a \
         reserved identifier"
          ~port:(signal : Signal.t)]
  | `Ok -> ()
;;

let add_phantom_port_name
  { mangler; instantiation_mangler = _; lang = (module Lang) }
  name
  =
  let legal_name = Lang.legalize name in
  if not (String.equal legal_name name)
  then
    raise_s
      [%message
        "[Rtl_name.add_phantom_port_name] illegal port name"
          (name : string)
          ~legal_name
          ~note:"Hardcaml will not change ports names."]
  else (
    match Mangler.add_identifier mangler name with
    | `Duplicate ->
      raise_s
        [%message
          "[Rtl_name.add_phantom_port_name] port name has already been defined or \
           matches a reserved identifier"
            (name : string)]
    | `Ok -> ())
;;

let mangle_name { mangler; instantiation_mangler = _; lang = (module Lang) } name =
  let legal_name = Lang.legalize name in
  Mangler.mangle mangler legal_name
;;

let derived_name signal = "_" ^ Signal.Type.Uid.to_string (Signal.uid signal)

let mangle_signal_names t signal =
  match Signal.names signal with
  | [] -> [ mangle_name t (derived_name signal) ]
  | names -> List.map names ~f:(mangle_name t)
;;

let mangle_instantiation_name
  { mangler; instantiation_mangler = _; lang = (module Lang) }
  signal
  =
  match signal with
  | Signal.Type.Inst { instantiation; _ } ->
    let legal_name = Lang.legalize instantiation.instance_label in
    Mangler.mangle mangler legal_name
  | _ ->
    raise_s
      [%message
        "[Rtl_name.mangle_instantiation_name] requires an Inst signal" (signal : Signal.t)]
;;

let mangle_multiport_mem_name
  ({ mangler = _; instantiation_mangler = _; lang = (module Lang) } as t)
  signal
  =
  match signal with
  | Signal.Type.Multiport_mem _ ->
    (match Signal.names signal with
     | [] ->
       (* memory nodes themselves do not have names (only the q_out) *)
       mangle_name t (derived_name signal), mangle_name t (derived_name signal ^ "_type")
     | name :: _ ->
       let name = Lang.legalize name in
       mangle_name t name, mangle_name t (name ^ "_type"))
  | _ ->
    raise_s
      [%message "[Rtl_name.mangle_mem_name] requires a Mem signal" (signal : Signal.t)]
;;

let of_language = function
  | Rtl_language.Verilog -> create (module Verilog)
  | Systemverilog -> create (module Systemverilog)
  | Vhdl -> create (module Vhdl)
;;
