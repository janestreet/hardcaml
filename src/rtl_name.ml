open! Core0

module Make_verilog (M : sig
    val reserved_words : string list
  end) =
struct
  let is_valid_first_char c = Char.is_alpha c || Char.equal c '_'
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$'
  let reserved_words = lazy (Set.of_list (module String) M.reserved_words)

  let is_legal_identifier string =
    if String.is_empty string
    then raise_s [%message "[Rtl_name.legalize] string is empty"];
    is_valid_first_char string.[0]
    && String.for_all string ~f:is_valid_other_char
    && not (Set.mem (force reserved_words) string)
  ;;

  let legalize_and_keep_unique string =
    if is_legal_identifier string then string else String.concat [ "\\"; string; " " ]
  ;;
end

module Verilog = struct
  module Verilog_base = Make_verilog (struct
      let reserved_words = Reserved_words.verilog
    end)

  include Verilog_base
end

module Systemverilog = struct
  module Verilog_base = Make_verilog (struct
      let reserved_words = Reserved_words.systemverilog
    end)

  include Verilog_base
end

module Vhdl = struct
  (* Here are the (bonkers) rules.

     - identifiers can contain only upper or lower case letters a-z, numerals 0-9, and
       underscore
     - the first character must be a letter
     - the last character cannot be an underscore
     - two successive underscores are not allowed
     - identifiers cannot contain spaces (i.e. the space character). Use underscore
       instead.
  *)

  let is_valid_first_char c = Char.is_alpha c
  let is_valid_last_char c = Char.is_alphanum c
  let is_valid_other_char c = Char.is_alphanum c || Char.equal c '_'
  let reserved_words = lazy (Set.of_list (module String) Reserved_words.vhdl)

  let is_legal_identifier string =
    if String.is_empty string
    then raise_s [%message "[Rtl_name.legalize] string is empty"];
    is_valid_first_char string.[0]
    && is_valid_last_char string.[String.length string - 1]
    && String.for_all string ~f:is_valid_other_char
    && (not (String.is_substring string ~substring:"__"))
    && not (Set.mem (force reserved_words) string)
  ;;

  let legalize_and_keep_unique string =
    if is_legal_identifier string then string else String.concat [ "\\"; string; "\\" ]
  ;;
end

(* A validated identifier is one which can be expressed as an simple or extended
   identifier in either VHDL or Verilog.

   It should not be 'empty' nor contain a space (for Verilog compatibility) or backslash
   (to avoid escaping issues).
*)
module Validated : sig
  type t [@@deriving to_string, sexp_of]

  val validate : string -> t
end = struct
  type t = string [@@deriving to_string, equal, sexp_of]

  let validate identifier =
    let is_valid_char = function
      | ' ' | '\\' -> false
      | c -> Char.is_print c
    in
    if String.is_empty identifier
    then raise_s [%message "[Rtl_name] string must not be empty"];
    if not (String.for_all identifier ~f:is_valid_char)
    then
      raise_s
        [%message
          "[Rtl_name]s must only contain printable characters and may not contain spaces \
           or back slashes"
            (identifier : string)];
    identifier
  ;;
end

module Mangled_and_validated : sig
  type t [@@deriving to_string, sexp_of, equal ~localize]

  val mangle : Mangler.t -> Validated.t -> t
  val add : Mangler.t -> Validated.t -> [ `Ok of t | `Duplicate ]
  val from_raw_external_name : string -> t
end = struct
  type t = string [@@deriving to_string, sexp_of, equal ~localize]

  let from_raw_external_name name = name
  let mangle mangler validated = Mangler.mangle mangler (Validated.to_string validated)

  let add mangler validated =
    let validated = Validated.to_string validated in
    match Mangler.add_identifier mangler validated with
    | `Ok -> `Ok validated
    | `Duplicate -> `Duplicate
  ;;
end

module For_backend = struct
  module T = struct
    type name = Mangled_and_validated.t [@@deriving sexp_of, equal ~localize]

    type t =
      { for_backed : string
      ; backend_agnostic : name
      }
    [@@deriving sexp_of, equal ~localize, fields ~getters]

    let legalize backend_agnostic ~(language : Rtl_language.t) =
      let name = Mangled_and_validated.to_string backend_agnostic in
      let for_backed =
        match language with
        | Verilog -> Verilog.legalize_and_keep_unique name
        | Systemverilog -> Systemverilog.legalize_and_keep_unique name
        | Vhdl -> Vhdl.legalize_and_keep_unique name
      in
      { for_backed; backend_agnostic }
    ;;

    let backend_agnostic_string t = t.backend_agnostic |> Mangled_and_validated.to_string
    let to_string t = t.for_backed
    let compare a b = String.compare a.for_backed b.for_backed
  end

  include T
  include Comparable.Make_plain (T)
end

type t = Mangled_and_validated.t [@@deriving sexp_of, equal ~localize]

let from_raw_external_name name = Mangled_and_validated.from_raw_external_name name
let backend_agnostic t = Mangled_and_validated.to_string t
let legalize t ~language = For_backend.legalize t ~language

module Scope = struct
  type t = { mangler : Mangler.t } [@@deriving fields ~getters, sexp_of]

  let create () = { mangler = Mangler.create ~case_sensitive:false }
  let validate _t name = Validated.validate name
  let mangle t validated = Mangled_and_validated.mangle t.mangler validated

  let[@cold] raise_duplicate_port_name ?port name =
    let port_str = if Option.is_some port then "Port" else "Phantom port" in
    let msg = [%string {|%{port_str} name has already been defined or is a keyword|}] in
    raise_s [%message msg (name : string) (port : Signal.t option)]
  ;;

  let[@inline] add_port_name' ?port t name =
    let validated_name = validate t name in
    match Mangled_and_validated.add t.mangler validated_name with
    | `Duplicate -> raise_duplicate_port_name ?port name
    | `Ok mangled_and_validated -> mangled_and_validated
  ;;

  let add_port_name t port name = add_port_name' t name ~port
  let add_phantom_port_name t name = add_port_name' t name

  let derived_name signal =
    let type_ =
      match signal with
      | Signal.Type.Empty -> "empty"
      | Const _ -> "const"
      | Op2 { op; _ } ->
        (match op with
         | Add -> "add"
         | Sub -> "sub"
         | Mulu -> "mulu"
         | Muls -> "muls"
         | And -> "and"
         | Or -> "or"
         | Xor -> "xor"
         | Eq -> "eq"
         | Lt -> "lt")
      | Mux _ -> "mux"
      | Cases _ -> "cases"
      | Cat _ -> "cat"
      | Not _ -> "not"
      | Wire _ -> "wire"
      | Select _ -> "select"
      | Reg _ -> "reg"
      | Multiport_mem _ -> "multiport_mem"
      | Mem_read_port _ -> "mem_read_port"
      | Inst _ -> "inst"
    in
    "signal_" ^ type_
  ;;

  let mangle_name t name = validate t name |> mangle t

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
          "[Rtl_name.mangle_instantiation_name] requires an Inst signal"
            (signal : Signal.t)]
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
end

module For_test = struct
  let of_string name = Mangled_and_validated.from_raw_external_name name
end
