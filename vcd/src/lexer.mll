{

open Parser

type state =
  | Initial
  | In_comment
  | In_date
  | In_version
  | In_timescale
  | In_scope
  | In_var
  | In_var_psize
  | In_var_pid
  | In_var_rng
  | In_simtime
  | In_val_changes
  | In_val_idcode

let state = ref Initial

(* Tracks whether we've seen [$enddefinitions], at which point [$comment] blocks
   should be lexed as a sim-phase comment so the parser can distinguish them
   from declaration-phase comments (the tokens are otherwise identical). *)
let in_sim_phase = ref false

let reset_state () =
  state := Initial;
  in_sim_phase := false
;;

(* Scalar and binary values use Verilog's 0/1/x/z; VHDL simulators also emit
   U (uninitialized), W (weak unknown), L (weak 0), H (weak 1) and - (don't care).
   Weak values map to their strong equivalents and the unknowns to X. *)
let bit_of_char c =
  match c with
  | '0' | 'l' | 'L' -> Types.Bit.V0
  | '1' | 'h' | 'H' -> V1
  | 'z' | 'Z' -> Vz
  | _ -> Vx
;;

(* Text tokens (comments, dates, versions) can span lines: account for their newlines so
   the positions in error messages stay accurate. *)
let count_newlines lexbuf s =
  String.iteri
    (fun i c ->
      if Char.equal c '\n'
      then (
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p
        <- { pos with
             pos_lnum = pos.pos_lnum + 1
           ; pos_bol = lexbuf.Lexing.lex_start_p.pos_cnum + i + 1
           }))
    s
;;

}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let decimal_num = ['0'-'9']+
let scalar_num = ['0' '1' 'x' 'X' 'z' 'Z' 'u' 'U' 'w' 'W' 'l' 'L' 'h' 'H' '-']

(* Zero digits are allowed: some producers write a bare [b] for an empty bit vector,
   which pads to all-zeros like any other short vector. *)
let bin_num = ['b' 'B'] ['0' '1' 'x' 'X' 'z' 'Z' 'u' 'U' 'w' 'W' 'l' 'L' 'h' 'H' '-']*

(* A string value is [s] followed by the string, which runs to the next whitespace;
   awkward characters are written as [\nnn] octal escapes, which [parse_string_value]
   decodes. The string may be empty. *)
let string_num = ['s' 'S'] [^ ' ' '\t' '\r' '\n']*

(* Simulators format reals with the equivalent of [%g], so accept a sign, a bare or
   trailing decimal point, and an exponent. [parse_real_value] hands the text to
   [float_of_string], which copes with all of these. *)
let real_num =
  ['r' 'R'] ['+' '-']?
  (['0'-'9']+ ('.' ['0'-'9']*)? | '.' ['0'-'9']+)
  (['e' 'E'] ['+' '-']? ['0'-'9']+)?

let identifier_code =
  ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '/' ',' '.' '@' '\'' ':' '~'
   '#' '*' '(' ')' '+' '{' '}' '$' '%' '[' ']' '`' '"' '&' ';'
   '<' '>' '=' '?' '-' '^' '|' '\\']+

(* Scope names in the wild are exotic: Xcelium writes [dft_buffer[11]], Verilator [|cpu],
   VCS [$unit], iverilog [$ivl_for_loop0], Yosys [output__payload$13], and so on. Accept
   any non-whitespace sequence; [$end] and the scope-type keywords still win ties because
   they are listed first. *)
let scope_name = [^ ' ' '\t' '\r' '\n']+

(* A [$var] reference is the printable signal name that follows the identifier code.
   Accept any non-whitespace sequence that does not start with ['[': a whitespace-
   separated range such as [foo [7:0]] is still parsed as a reference plus range, while
   names with attached brackets (escaped identifiers, or flattened names like
   [in[0].payload] and [sp_arr[0][15:0]]) are kept whole. [$end] is separated from the
   reference by whitespace, and wins the tie with this rule because it is listed
   first. *)
let reference_identifier =
  [^ ' ' '\t' '\r' '\n' '['] [^ ' ' '\t' '\r' '\n']*

let comment_text = [^ '$']*

rule token = parse
  | whitespace { token lexbuf }
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | eof        { EOF }
  | ""         {
    match !state with
    | Initial        -> initial lexbuf
    | In_comment     -> in_comment lexbuf
    | In_date        -> in_date lexbuf
    | In_version     -> in_version lexbuf
    | In_timescale   -> in_timescale lexbuf
    | In_scope       -> in_scope lexbuf
    | In_var         -> in_var lexbuf
    | In_var_psize   -> in_var_psize lexbuf
    | In_var_pid     -> in_var_pid lexbuf
    | In_var_rng     -> in_var_rng lexbuf
    | In_simtime     -> in_simtime lexbuf
    | In_val_changes -> in_val_changes lexbuf
    | In_val_idcode  -> in_val_idcode lexbuf
  }

and initial = parse
  | "$end"              { state := Initial; TOK_KW_END }
  | "$comment"          {
      state := In_comment;
      if !in_sim_phase then TOK_KW_SIM_COMMENT else TOK_KW_COMMENT
    }
  | "$date"             { state := In_date; TOK_KW_DATE }
  | "$enddefinitions"   {
      state := Initial;
      in_sim_phase := true;
      TOK_KW_ENDDEFINITIONS
    }
  | "$scope"            { state := In_scope; TOK_KW_SCOPE }
  | "$timescale"        { state := In_timescale; TOK_KW_TIMESCALE }
  | "$upscope"          { state := Initial; TOK_KW_UPSCOPE }
  | "$var"              { state := In_var; TOK_KW_VAR }
  | "$version"          { state := In_version; TOK_KW_VERSION }
  | "$dumpall"          { state := In_val_changes; TOK_KW_DUMPALL }
  | "$dumpoff"          { state := In_val_changes; TOK_KW_DUMPOFF }
  | "$dumpon"           { state := In_val_changes; TOK_KW_DUMPON }
  | "$dumpvars"         { state := In_val_changes; TOK_KW_DUMPVARS }
  | '#'                 { state := In_simtime; TOK_HASH }
  | scalar_num as c     { state := In_val_idcode; TOK_VALUE (bit_of_char c) }
  | bin_num as s        { state := In_val_idcode; TOK_BIN_NUM s }
  | real_num as s       { state := In_val_idcode; TOK_REAL_NUM s }
  | string_num as s     { state := In_val_idcode; TOK_STRING_NUM s }
  (* Simulators emit non-standard commands, such as nvc's [$attrbegin ... $end] blocks or
     bare markers like [$crash]. Skip to the [$end] or the end of the line, whichever
     comes first. Known keywords win ties with this rule because they are listed first. *)
  | '$' ['a'-'z' 'A'-'Z' '0'-'9' '_']+ { skip_unknown_command lexbuf }

and in_comment = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { count_newlines lexbuf s; TOK_COMMENT_TEXT s }

and in_date = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { count_newlines lexbuf s; TOK_DATE_TEXT s }

and in_version = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { count_newlines lexbuf s; TOK_VERSION_TEXT s }

and in_timescale = parse
  | whitespace   { in_timescale lexbuf }
  | newline      { Lexing.new_line lexbuf; in_timescale lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  (* The spec allows only 1, 10 and 100, but producers write other values (gameroy writes
     [$timescale 244 ns $end]). *)
  | decimal_num as s { TOK_TIME_NUMBER (int_of_string s) }
  | "fs"         { TOK_TIME_UNIT Types.Time_unit.Fs }
  | "ps"         { TOK_TIME_UNIT Types.Time_unit.Ps }
  | "ns"         { TOK_TIME_UNIT Types.Time_unit.Ns }
  | "us"         { TOK_TIME_UNIT Types.Time_unit.Us }
  | "ms"         { TOK_TIME_UNIT Types.Time_unit.Ms }
  | "s"          { TOK_TIME_UNIT Types.Time_unit.S }

and in_scope = parse
  | whitespace   { in_scope lexbuf }
  | newline      { Lexing.new_line lexbuf; in_scope lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | "begin"      { TOK_KW_BEGIN Types.Scope.Begin }
  | "fork"       { TOK_KW_FORK Types.Scope.Fork }
  | "function"   { TOK_KW_FUNCTION Types.Scope.Function }
  | "module"     { TOK_KW_MODULE Types.Scope.Module }
  | "task"       { TOK_KW_TASK Types.Scope.Task }
  | "class"     { TOK_SCOPE_TYPE Types.Scope.Class }
  | "generate"  { TOK_SCOPE_TYPE Types.Scope.Generate }
  | "interface" { TOK_SCOPE_TYPE Types.Scope.Interface }
  | "package"   { TOK_SCOPE_TYPE Types.Scope.Package }
  | "program"   { TOK_SCOPE_TYPE Types.Scope.Program }
  | "struct"    { TOK_SCOPE_TYPE Types.Scope.Struct }
  | "union"     { TOK_SCOPE_TYPE Types.Scope.Union }
  | "vhdl_architecture" { TOK_SCOPE_TYPE Types.Scope.Vhdl_architecture }
  | "vhdl_block"        { TOK_SCOPE_TYPE Types.Scope.Vhdl_block }
  | "vhdl_for_generate" { TOK_SCOPE_TYPE Types.Scope.Vhdl_for_generate }
  | "vhdl_function"     { TOK_SCOPE_TYPE Types.Scope.Vhdl_function }
  | "vhdl_generate"     { TOK_SCOPE_TYPE Types.Scope.Vhdl_generate }
  | "vhdl_if_generate"  { TOK_SCOPE_TYPE Types.Scope.Vhdl_if_generate }
  | "vhdl_package"      { TOK_SCOPE_TYPE Types.Scope.Vhdl_package }
  | "vhdl_procedure"    { TOK_SCOPE_TYPE Types.Scope.Vhdl_procedure }
  | "vhdl_process"      { TOK_SCOPE_TYPE Types.Scope.Vhdl_process }
  | "vhdl_record"       { TOK_SCOPE_TYPE Types.Scope.Vhdl_record }
  | "unknown"           { TOK_SCOPE_TYPE Types.Scope.Unknown }
  | scope_name as s { TOK_IDENTIFIER s }

and in_var = parse
  | whitespace   { in_var lexbuf }
  | newline      { Lexing.new_line lexbuf; in_var lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | "event"      { TOK_VAR_TYPE Types.Var_type.Event }
  | "integer"    { TOK_VAR_TYPE Types.Var_type.Integer }
  | "parameter"  { TOK_VAR_TYPE Types.Var_type.Parameter }
  | "realtime"   { TOK_VAR_TYPE Types.Var_type.Realtime }
  | "real"       { TOK_VAR_TYPE Types.Var_type.Real }
  | "reg"        { TOK_VAR_TYPE Types.Var_type.Reg }
  | "supply0"    { TOK_VAR_TYPE Types.Var_type.Supply0 }
  | "supply1"    { TOK_VAR_TYPE Types.Var_type.Supply1 }
  | "time"       { TOK_VAR_TYPE Types.Var_type.Time }
  | "triand"     { TOK_VAR_TYPE Types.Var_type.Triand }
  | "trior"      { TOK_VAR_TYPE Types.Var_type.Trior }
  | "trireg"     { TOK_VAR_TYPE Types.Var_type.Trireg }
  | "tri0"       { TOK_VAR_TYPE Types.Var_type.Tri0 }
  | "tri1"       { TOK_VAR_TYPE Types.Var_type.Tri1 }
  | "tri"        { TOK_VAR_TYPE Types.Var_type.Tri }
  | "wand"       { TOK_VAR_TYPE Types.Var_type.Wand }
  | "wire"       { TOK_VAR_TYPE Types.Var_type.Wire }
  | "wor"        { TOK_VAR_TYPE Types.Var_type.Wor }
  | "bit"        { TOK_VAR_TYPE Types.Var_type.Bit }
  | "byte"       { TOK_VAR_TYPE Types.Var_type.Byte }
  | "enum"       { TOK_VAR_TYPE Types.Var_type.Enum }
  | "int"        { TOK_VAR_TYPE Types.Var_type.Int }
  | "logic"      { TOK_VAR_TYPE Types.Var_type.Logic }
  | "longint"    { TOK_VAR_TYPE Types.Var_type.Longint }
  | "port"       { TOK_VAR_TYPE Types.Var_type.Port }
  | "shortint"   { TOK_VAR_TYPE Types.Var_type.Shortint }
  | "shortreal"  { TOK_VAR_TYPE Types.Var_type.Shortreal }
  | "string"     { TOK_VAR_TYPE Types.Var_type.String }
  | "real_parameter" { TOK_VAR_TYPE Types.Var_type.Real_parameter }
  | decimal_num as s { state := In_var_psize; TOK_DECIMAL_NUM (int_of_string s) }

and in_var_psize = parse
  | whitespace   { in_var_psize lexbuf }
  | newline      { Lexing.new_line lexbuf; in_var_psize lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | identifier_code as s { state := In_var_pid; TOK_IDENTIFIER s }

and in_var_pid = parse
  | whitespace   { in_var_pid lexbuf }
  | newline      { Lexing.new_line lexbuf; in_var_pid lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | '['          { state := In_var_rng; TOK_BRACKET_O }
  | reference_identifier as s { TOK_IDENTIFIER s }

and in_var_rng = parse
  | whitespace   { in_var_rng lexbuf }
  | newline      { Lexing.new_line lexbuf; in_var_rng lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | decimal_num as s { TOK_DECIMAL_NUM (int_of_string s) }
  | ':'          { TOK_COLON }
  | ']'          { TOK_BRACKET_C }

and in_simtime = parse
  | whitespace   { in_simtime lexbuf }
  | newline      { Lexing.new_line lexbuf; in_simtime lexbuf }
  (* Some producers (migen) write fractional timestamps such as [#0.5]; truncate the
     fractional part. *)
  | (decimal_num as s) ('.' decimal_num)?
    { state := Initial; TOK_DECIMAL_NUM (int_of_string s) }

and in_val_changes = parse
  | whitespace   { in_val_changes lexbuf }
  | newline      { Lexing.new_line lexbuf; in_val_changes lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  (* Some producers never terminate [$dumpvars] with [$end]; the [#] of the next
     timestamp ends the block instead. *)
  | '#'           { state := In_simtime; TOK_HASH }
  | scalar_num as c { state := In_val_idcode; TOK_VALUE (bit_of_char c) }
  | bin_num as s  { state := In_val_idcode; TOK_BIN_NUM s }
  | real_num as s { state := In_val_idcode; TOK_REAL_NUM s }
  | string_num as s { state := In_val_idcode; TOK_STRING_NUM s }

and in_val_idcode = parse
  | whitespace   { in_val_idcode lexbuf }
  | newline      { Lexing.new_line lexbuf; in_val_idcode lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | identifier_code as s { state := Initial; TOK_IDENTIFIER s }

(* Skip an unknown [$command], up to its [$end] or the end of the line. Commands like
   nvc's [$attrbegin ...] fit on one line and are terminated by [$end], while bare
   markers like [$crash] have no [$end] at all. *)
and skip_unknown_command = parse
  | "$end"  { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof     { EOF }
  | [^ '$' '\r' '\n']+ { skip_unknown_command lexbuf }
  | '$'     { skip_unknown_command lexbuf }
