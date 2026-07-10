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

}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let decimal_num = ['0'-'9']+
let scalar_num = ['0' '1' 'x' 'X' 'z' 'Z']
let bin_num = ['b' 'B'] ['0' '1' 'x' 'X' 'z' 'Z']+
let real_num = ['r' 'R'] ['0'-'9']+ ('.' ['0'-'9']+)?

let identifier_code =
  ['a'-'z' 'A'-'Z' '_' '0'-'9' '!' '/' ',' '.' '@' '\'' ':' '~'
   '#' '*' '(' ')' '+' '{' '}' '$' '%' '[' ']' '`' '"' '&' ';'
   '<' '>' '=' '?' '-' '^' '|' '\\']+

(* Verilog identifiers, with the additional concession of allowing leading and embedded
   ['-'] to support Hardcaml's synthesised [-clock], [-reset], [-inputs] and [-outputs]
   names. *)
let scope_identifier =
  ['a'-'z' 'A'-'Z' '_' '-'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '(' ')' '-']*

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
  | scalar_num as c     {
      state := In_val_idcode;
      let bit = match c with
        | '0' -> Types.Bit.V0
        | '1' -> V1
        | 'x' | 'X' -> Vx
        | 'z' | 'Z' -> Vz
        | _ -> Vx
      in
      TOK_VALUE bit
    }
  | bin_num as s        { state := In_val_idcode; TOK_BIN_NUM s }
  | real_num as s       { state := In_val_idcode; TOK_REAL_NUM s }

and in_comment = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { TOK_COMMENT_TEXT s }

and in_date = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { TOK_DATE_TEXT s }

and in_version = parse
  | "$end"       { state := Initial; TOK_KW_END }
  | comment_text as s { TOK_VERSION_TEXT s }

and in_timescale = parse
  | whitespace   { in_timescale lexbuf }
  | newline      { Lexing.new_line lexbuf; in_timescale lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | "100"        { TOK_TIME_NUMBER 100 }
  | "10"         { TOK_TIME_NUMBER 10 }
  | "1"          { TOK_TIME_NUMBER 1 }
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
  | scope_identifier as s { TOK_IDENTIFIER s }

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
  | scope_identifier as s { TOK_IDENTIFIER s }

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
  | decimal_num as s { state := Initial; TOK_DECIMAL_NUM (int_of_string s) }

and in_val_changes = parse
  | whitespace   { in_val_changes lexbuf }
  | newline      { Lexing.new_line lexbuf; in_val_changes lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | scalar_num as c {
      state := In_val_idcode;
      let bit = match c with
        | '0' -> Types.Bit.V0
        | '1' -> V1
        | 'x' | 'X' -> Vx
        | 'z' | 'Z' -> Vz
        | _ -> Vx
      in
      TOK_VALUE bit
    }
  | bin_num as s  { state := In_val_idcode; TOK_BIN_NUM s }
  | real_num as s { state := In_val_idcode; TOK_REAL_NUM s }

and in_val_idcode = parse
  | whitespace   { in_val_idcode lexbuf }
  | newline      { Lexing.new_line lexbuf; in_val_idcode lexbuf }
  | "$end"       { state := Initial; TOK_KW_END }
  | identifier_code as s { state := Initial; TOK_IDENTIFIER s }
