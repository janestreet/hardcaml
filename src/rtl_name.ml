open Base

module type Language = sig
  val is_valid_first_char : char -> bool
  val is_valid_other_char : char -> bool
  val replace_with : char
  val prefix : string
  val case_sensitive : bool
  val reserved_words : string list
end

module Verilog = struct
  let is_valid_first_char c = Char.is_alpha c || Char.equal c '_'
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$'
  let replace_with = '_'
  let prefix = "_"
  let case_sensitive = true
  let reserved_words = Reserved_words.verilog
end

module Vhdl = struct
  (* Here are the actual rules.  This is not properly enforced here - in particular 3 and 4.

     1 identifiers can contain only upper or lower case letters a-z, numerals 0-9, and underscore
     2 the first character must be a letter
     3 the last character cannot be an underscore
     4 two successive underscores are not allowed
     5 identifiers cannot contain spaces (i.e. the space character). Use underscore instead.
  *)

  let is_valid_first_char c = Char.is_alpha c
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_'
  let replace_with = '_'
  let prefix = "hc_"
  let case_sensitive = false

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

let create (module Lang : Language) =
  let mangler = Mangler.create ~case_sensitive:Lang.case_sensitive in
  let instantiation_mangler = Mangler.create ~case_sensitive:Lang.case_sensitive in
  Mangler.add_identifiers_exn mangler Lang.reserved_words;
  { mangler; instantiation_mangler; lang = (module Lang) }
;;

let rec legalize
  ({ mangler = _; instantiation_mangler = _; lang = (module Lang) } as t)
  string
  =
  if String.is_empty string then raise_s [%message "[Rtl_name] string is empty"];
  if Lang.is_valid_first_char string.[0]
  then
    String.map string ~f:(fun c ->
      if Lang.is_valid_other_char c then c else Lang.replace_with)
  else legalize t (Lang.prefix ^ string)
;;

let add_port_name ({ mangler; instantiation_mangler = _; lang = _ } as t) signal name =
  let legal_name = legalize t name in
  if not (String.equal legal_name name)
  then
    raise_s
      [%message
        "[Rtl_name.add_port_name] illegal port name"
          (name : string)
          ~legal_name:(legalize t name : string)
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

let add_phantom_port_name t name =
  let legal_name = legalize t name in
  if not (String.equal legal_name name)
  then
    raise_s
      [%message
        "[Rtl_name.add_phantom_port_name] illegal port name"
          (name : string)
          ~legal_name
          ~note:"Hardcaml will not change ports names."]
  else (
    match Mangler.add_identifier t.mangler name with
    | `Duplicate ->
      raise_s
        [%message
          "[Rtl_name.add_phantom_port_name] port name has already been defined or \
           matches a reserved identifier"
            (name : string)]
    | `Ok -> ())
;;

let mangle_name t name =
  let legal_name = legalize t name in
  Mangler.mangle t.mangler legal_name
;;

let derived_name (module Lang : Language) signal =
  Lang.prefix ^ Signal.Uid.to_string (Signal.uid signal)
;;

let mangle_signal_names
  ({ mangler = _; instantiation_mangler = _; lang = (module Lang) } as t)
  signal
  =
  match Signal.names signal with
  | [] -> [ mangle_name t (derived_name (module Lang) signal) ]
  | names -> List.map names ~f:(mangle_name t)
;;

let mangle_instantiation_name t signal =
  match signal with
  | Signal.Type.Inst { instantiation; _ } ->
    let legal_name = legalize t instantiation.inst_instance in
    Mangler.mangle t.mangler legal_name
  | _ ->
    raise_s
      [%message
        "[Rtl_name.mangle_instantiation_name] requires an Inst signal" (signal : Signal.t)]
;;

let mangle_multiport_mem_name t signal =
  match signal with
  | Signal.Type.Multiport_mem _ ->
    (match Signal.names signal with
     | [] ->
       (* memory nodes themselves do not have names (only the q_out) *)
       ( mangle_name t (derived_name t.lang signal)
       , mangle_name t (derived_name t.lang signal ^ "_type") )
     | name :: _ ->
       let name = legalize t name in
       mangle_name t name, mangle_name t (name ^ "_type"))
  | _ ->
    raise_s
      [%message "[Rtl_name.mangle_mem_name] requires a Mem signal" (signal : Signal.t)]
;;
