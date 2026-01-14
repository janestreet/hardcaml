open Ppxlib
open Ppxlib.Ast_builder.Default

let deriver = "hardcaml"
let raise_errorf = Location.raise_errorf

type t = (label_declaration, expression) Attribute.t

let find t label_declaration = Attribute.get t label_declaration

let create name =
  Attribute.declare
    name
    Label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let exists = create "exists"
let bits = create "bits"
let length = create "length"
let rtlmangle = create "rtlmangle"
let rtlname = create "rtlname"
let rtlprefix = create "rtlprefix"
let rtlsuffix = create "rtlsuffix"
let wave_format = create "wave_format"

(* This represents the [ocaml.doc] attribute, which maps to documentation comments. The
   leading [hardcaml.] token is required to bypass some compiler (or ppx) related checks.
   I mention it because it's an undocumented hack. *)
let doc = create "hardcaml.ocaml.doc"

let has_exists ~loc:_ label_declaration =
  (find exists) label_declaration |> Option.is_some
;;

let get_exists ~loc label_declaration =
  match (find exists) label_declaration with
  | Some exists -> exists
  | None -> raise_errorf ~loc "[%s] exists attribute required in [option]" deriver
;;

let get_bits_opt ~loc:_ label_declaration = (find bits) label_declaration

let get_bits_with_default ~loc label_declaration =
  match get_bits_opt ~loc label_declaration with
  | Some expr -> expr
  | None -> pexp_constant ~loc (Pconst_integer ("1", None))
;;

let get_length ~loc label_declaration =
  match (find length) label_declaration with
  | Some expr -> expr
  | None -> raise_errorf ~loc "[%s] length attribute must be set" deriver
;;

let get_wave_format ~loc label_declaration =
  match (find wave_format) label_declaration with
  | Some expr -> expr
  | None -> [%expr Ppx_hardcaml_runtime.Wave_format.default]
;;

let get_rtlname ~loc txt label_declaration =
  match (find rtlname) label_declaration with
  | Some expr -> expr
  | None -> pexp_constant ~loc (Pconst_string (txt, loc, None))
;;

let get_rtlprefix ~loc:_ ~(opts : Interface_options.t) label_declaration =
  match (find rtlprefix) label_declaration with
  | Some expr -> Some expr
  | None -> opts.rtlprefix
;;

let get_rtlsuffix ~loc:_ ~(opts : Interface_options.t) label_declaration =
  match (find rtlsuffix) label_declaration with
  | Some expr -> Some expr
  | None -> opts.rtlsuffix
;;

let get_rtlmangle ~loc:_ ~(opts : Interface_options.t) label_declaration =
  match (find rtlmangle) label_declaration with
  | Some expr -> Some expr
  | None -> opts.rtlmangle
;;

let get_doc ~loc label_declaration =
  match (find doc) label_declaration with
  | Some expr ->
    (match expr.pexp_desc with
     | Pexp_constant (Pconst_string (str, _, _)) -> Some str
     | _ -> raise_errorf ~loc "[%s] doc atttribute must be a string" deriver)
  | None -> None
;;
