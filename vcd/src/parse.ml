open! Core

type t = Types.t [@@deriving sexp_of]

let initialize_lexbuf filename =
  let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  fun (lexbuf : Lexing.lexbuf) ->
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos
;;

(* Remove Windows-style line breaks to simplify lexing *)
let normalize_linebreaks str = String.substr_replace_all str ~pattern:"\r\n" ~with_:"\n"

let parse_lexbuf lexbuf =
  Lexer.reset_state ();
  try Parser.input Lexer.token lexbuf with
  (* If there are no syntax errors, the parser returns an [Or_error.t] *)
  | _ ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise_s
      [%message
        "Parsing error"
          ~line:(pos.pos_lnum : int)
          ~col:(pos.pos_cnum - pos.pos_bol + 1 : int)]
;;

let from_file file_path =
  let contents = In_channel.read_all file_path |> normalize_linebreaks in
  let lexbuf = Lexing.from_string contents in
  initialize_lexbuf file_path lexbuf;
  parse_lexbuf lexbuf
;;

let from_string str =
  (* Parser does not support non-newline terminated strings *)
  let lexbuf = Lexing.from_string (normalize_linebreaks str ^ "\n") in
  initialize_lexbuf "<string>" lexbuf;
  parse_lexbuf lexbuf
;;
