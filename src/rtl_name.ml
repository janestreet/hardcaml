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

module Vhdl_base = struct
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

  (* In reality, extended identifiers are case sensitive. *)
  let case_sensitive = false

  let rec remove_double_underscores in_ =
    let out = String.Search_pattern.replace_all double_underscores ~in_ ~with_:"_" in
    if String.equal in_ out then out else remove_double_underscores out
  ;;

  let is_valid_extended_identifier string =
    let is_extended_identifier =
      String.length string >= 3
      && String.is_prefix ~prefix:"\\" string
      && String.is_suffix ~suffix:"\\" string
    in
    if not is_extended_identifier
    then false
    else (
      let identifier =
        string
        |> String.chop_prefix_exn ~prefix:"\\"
        |> String.chop_suffix_exn ~suffix:"\\"
      in
      (* All backslashes inside an extended identifier must be escaped, remove
         double-backslash pairs from the string and verify that no backslashes remain *)
      let contains_single_backslashes =
        String.substr_replace_all identifier ~pattern:"\\\\" ~with_:""
        |> fun s -> String.contains s '\\'
      in
      let contains_spaces = String.contains identifier ' ' in
      (not contains_single_backslashes) && not contains_spaces)
  ;;

  let rec legalize_by_mangling string =
    if is_valid_extended_identifier string
    then string
    else (
      if String.is_empty string
      then raise_s [%message "[Rtl_name.legalize] string is empty"];
      if not (is_valid_first_char string.[0])
      then legalize_by_mangling (prefix ^ string)
      else if not (is_valid_last_char string.[String.length string - 1])
      then legalize_by_mangling (string ^ suffix)
      else
        String.map string ~f:(fun c -> if is_valid_other_char c then c else replace_with)
        |> remove_double_underscores)
  ;;

  let legalize_by_making_extended_identifier string =
    (* Check if the name is already legal and don't bother converting to an extended
       identifier if it is *)
    if String.equal string (legalize_by_mangling string)
    then string
    else
      string
      |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
         (* While spaces are technically allowed in extended identifiers, they break all
            sorts of other things, so just don't bother with them. We already have to
            modify the identifier in this step (escaping any backslashes it contains),
            so there's not much downside to doing this additional change as well. *)
      |> String.map ~f:(fun c -> if Char.is_whitespace c then '_' else c)
      (* Avoid the possibility of creating confusion by having the same identifier
           with different cases + avoid the issue of tools which don't handle
           case-sensitivity properly. *)
      |> String.lowercase
      |> fun s -> String.concat [ "\\"; s; "\\" ]
  ;;

  let reserved_words =
    Reserved_words.vhdl
    @ [ prefix ^ "sgn"; prefix ^ "uns"; prefix ^ "sl"; prefix ^ "slv"; "to_integer" ]
  ;;
end

module Vhdl_with_mangling = struct
  include Vhdl_base

  let legalize = legalize_by_mangling
end

module Vhdl_with_extended_identifiers = struct
  include Vhdl_base

  let legalize = legalize_by_making_extended_identifier
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
  Mangler.mangle mangler name |> Lang.legalize
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
    Mangler.mangle mangler instantiation.instance_label |> Lang.legalize
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
       mangle_name t (Lang.legalize name), mangle_name t (Lang.legalize (name ^ "_type")))
  | _ ->
    raise_s
      [%message "[Rtl_name.mangle_mem_name] requires a Mem signal" (signal : Signal.t)]
;;

let of_language = function
  | Rtl_language.Verilog -> create (module Verilog)
  | Systemverilog -> create (module Systemverilog)
  | Vhdl -> create (module Vhdl_with_extended_identifiers)
;;
