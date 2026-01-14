open! Core0

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
  (*=Here are the (bonkers) rules.

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

  let legalize string =
    (* Check if the name is already legal and don't bother converting to an extended
       identifier if it is *)
    if String.equal string (legalize_by_mangling string)
    then string
    else
      string
      |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
         (* While spaces are technically allowed in extended identifiers, they break all
            sorts of other things, so just don't bother with them. We already have to
            modify the identifier in this step (escaping any backslashes it contains), so
            there's not much downside to doing this additional change as well. *)
      |> String.map ~f:(fun c -> if Char.is_whitespace c then '_' else c)
      (* Avoid the possibility of creating confusion by having the same identifier with
         different cases + avoid the issue of tools which don't handle case-sensitivity
         properly. *)
      |> String.lowercase
      |> fun s -> String.concat [ "\\"; s; "\\" ]
  ;;

  let reserved_words =
    Reserved_words.vhdl
    @ [ prefix ^ "sgn"; prefix ^ "uns"; prefix ^ "sl"; prefix ^ "slv"; "to_integer" ]
  ;;
end

module Legalized : sig
  type t [@@deriving to_string, sexp_of]

  val legalize : Rtl_language.t -> string -> t
  val equal : t -> string -> bool
end = struct
  type t = string [@@deriving to_string, equal, sexp_of]

  let legalize (lang : Rtl_language.t) name =
    match lang with
    | Verilog -> Verilog.legalize name
    | Systemverilog -> Systemverilog.legalize name
    | Vhdl -> Vhdl.legalize name
  ;;
end

module Mangled_and_legalized : sig
  type t [@@deriving to_string]

  val mangle : Mangler.t -> Legalized.t -> t
  val add : Mangler.t -> Legalized.t -> [ `Ok | `Duplicate ]
end = struct
  type t = string [@@deriving to_string]

  let mangle mangler legalized = Mangler.mangle mangler (Legalized.to_string legalized)

  let add mangler legalized =
    let legalized = Legalized.to_string legalized in
    Mangler.add_identifier mangler legalized
  ;;
end

type t =
  { mangler : Mangler.t
  ; lang : Rtl_language.t
  }
[@@deriving fields ~getters]

let create (lang : Rtl_language.t) =
  let mangler = Mangler.create ~case_sensitive:true in
  let reserved_words =
    match lang with
    | Verilog -> Verilog.reserved_words
    | Systemverilog -> Systemverilog.reserved_words
    | Vhdl -> Vhdl.reserved_words
  in
  Mangler.add_identifiers_exn mangler reserved_words;
  { mangler; lang }
;;

let legalize t name = Legalized.legalize t.lang name
let mangle t legalized = Mangled_and_legalized.mangle t.mangler legalized

let[@cold] raise_illegal_port_name ?port name legal_name =
  let port_str = if Option.is_some port then "port" else "phantom port" in
  let msg = [%string {|[Illegal %{port_str} name|}] in
  raise_s
    [%message
      msg
        (name : string)
        (legal_name : Legalized.t)
        ~note:"Hardcaml will not change ports names."
        (port : Signal.t option)]
;;

let[@cold] raise_duplicate_port_name ?port name =
  let port_str = if Option.is_some port then "Port" else "Phantom port" in
  let msg = [%string {|%{port_str} name has already been defined|}] in
  raise_s [%message msg (name : string) (port : Signal.t option)]
;;

let[@inline] add_port_name' ?port t name =
  let legal_name = legalize t name in
  if not (Legalized.equal legal_name name)
  then raise_illegal_port_name ?port name legal_name;
  match Mangled_and_legalized.add t.mangler legal_name with
  | `Duplicate -> raise_duplicate_port_name ?port name
  | `Ok -> ()
;;

let add_port_name t port name = add_port_name' t name ~port
let add_phantom_port_name t name = add_port_name' t name
let derived_name signal = "_" ^ Signal.Type.Uid.to_string (Signal.uid signal)
let mangle_name t name = legalize t name |> mangle t |> Mangled_and_legalized.to_string

let mangle_signal_names t signal =
  match Signal.names signal with
  | [] -> [ mangle_name t (derived_name signal) ]
  | names -> List.map names ~f:(mangle_name t)
;;

let mangle_instantiation_name t signal =
  match signal with
  | Signal.Type.Inst { instantiation; _ } -> mangle_name t instantiation.instance_label
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
       mangle_name t (derived_name signal), mangle_name t (derived_name signal ^ "_type")
     | name :: _ -> mangle_name t name, mangle_name t (name ^ "_type"))
  | _ ->
    raise_s
      [%message "[Rtl_name.mangle_mem_name] requires a Mem signal" (signal : Signal.t)]
;;
