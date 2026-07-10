%{

open Types

let parse_bin_vector s =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then List.rev acc
    else begin
      let b = match s.[i] with
        | '0' -> Types.Bit.V0
        | '1' -> V1
        | 'x' | 'X' -> Vx
        | 'z' | 'Z' -> Vz
        | _ -> Vx
      in
      loop (i+1) (b :: acc)
    end
  in
  (* skip leading 'b' or 'B' character *)
  loop 1 []
;;

let parse_real_value s =
  (* skip leading 'r' or 'R' character *)
  float_of_string (String.sub s 1 (String.length s - 1))
;;

%}

%token TOK_BRACKET_O
%token TOK_BRACKET_C
%token TOK_COLON
%token TOK_KW_END
%token TOK_KW_COMMENT
%token TOK_KW_SIM_COMMENT
%token <string> TOK_COMMENT_TEXT
%token TOK_KW_DATE
%token <string> TOK_DATE_TEXT
%token TOK_KW_ENDDEFINITIONS
%token TOK_KW_SCOPE
%token TOK_KW_TIMESCALE
%token TOK_KW_UPSCOPE
%token TOK_KW_VAR
%token TOK_KW_VERSION
%token <string> TOK_VERSION_TEXT
%token TOK_KW_DUMPALL
%token TOK_KW_DUMPOFF
%token TOK_KW_DUMPON
%token TOK_KW_DUMPVARS
%token <Types.Scope.t> TOK_KW_BEGIN
%token <Types.Scope.t> TOK_KW_FORK
%token <Types.Scope.t> TOK_KW_FUNCTION
%token <Types.Scope.t> TOK_KW_MODULE
%token <Types.Scope.t> TOK_KW_TASK
%token <int> TOK_TIME_NUMBER
%token <Types.Time_unit.t> TOK_TIME_UNIT
%token <Types.Var_type.t> TOK_VAR_TYPE
%token TOK_HASH
%token <Types.Bit.t> TOK_VALUE
%token <string> TOK_BIN_NUM
%token <string> TOK_REAL_NUM
%token <string> TOK_IDENTIFIER
%token <int> TOK_DECIMAL_NUM
%token EOF

%start input
%type <Types.t> input

%%

input:
    EOF
    { { declarations = []; simulation_commands = [] } }
  | declaration_commands simulation_commands EOF
    { { declarations = $1; simulation_commands = $2 } }
  | simulation_commands EOF
    { { declarations = []; simulation_commands = $1 } }
  | declaration_commands EOF
    { { declarations = $1; simulation_commands = [] } }
;

declaration_commands:
    declaration_command
    { [ $1 ] }
  | declaration_commands declaration_command
    { $1 @ [ $2 ] }
;

simulation_commands:
    simulation_command
    { [ $1 ] }
  | simulation_commands simulation_command
    { $1 @ [ $2 ] }
;

declaration_command:
    TOK_KW_COMMENT comment_text TOK_KW_END
    { Comment $2 }
  | TOK_KW_DATE date_text TOK_KW_END
    { Date $2 }
  | TOK_KW_ENDDEFINITIONS TOK_KW_END
    { Enddefinitions }
  | TOK_KW_SCOPE scope_type TOK_IDENTIFIER TOK_KW_END
    { Scope ($2, $3) }
  | TOK_KW_TIMESCALE TOK_TIME_NUMBER TOK_TIME_UNIT TOK_KW_END
    { Timescale ($2, $3) }
  | TOK_KW_UPSCOPE TOK_KW_END
    { Upscope }
  | TOK_KW_VAR TOK_VAR_TYPE TOK_DECIMAL_NUM TOK_IDENTIFIER
    reference TOK_KW_END
    { Var { var_type = $2; var_size = $3; var_id = $4; var_ref = $5 } }
  | TOK_KW_VERSION version_text TOK_KW_END
    { Version $2 }
;

simulation_command:
    TOK_KW_DUMPALL value_changes_opt TOK_KW_END
    { Sim_dumpall $2 }
  | TOK_KW_DUMPOFF value_changes_opt TOK_KW_END
    { Sim_dumpoff $2 }
  | TOK_KW_DUMPON value_changes_opt TOK_KW_END
    { Sim_dumpon $2 }
  | TOK_KW_DUMPVARS value_changes_opt TOK_KW_END
    { Sim_dumpvars $2 }
  | TOK_KW_SIM_COMMENT comment_text TOK_KW_END
    { Sim_comment $2 }
  | simulation_time
    { $1 }
  | value_change
    { Sim_value_change $1 }
;

scope_type:
    TOK_KW_BEGIN    { $1 }
  | TOK_KW_FORK     { $1 }
  | TOK_KW_FUNCTION { $1 }
  | TOK_KW_MODULE   { $1 }
  | TOK_KW_TASK     { $1 }
;

simulation_time:
    TOK_HASH TOK_DECIMAL_NUM
    { Sim_time $2 }
;

value_changes:
    value_change
    { [ $1 ] }
  | value_changes value_change
    { $1 @ [ $2 ] }
;

value_changes_opt:
    /* empty */    { [] }
  | value_changes  { $1 }
;

value_change:
    scalar_value_change { $1 }
  | vector_value_change { $1 }
;

scalar_value_change:
    TOK_VALUE TOK_IDENTIFIER
    { Scalar_value ($1, $2) }
;

vector_value_change:
    TOK_BIN_NUM TOK_IDENTIFIER
    { Vector_value (parse_bin_vector $1, $2) }
  | TOK_REAL_NUM TOK_IDENTIFIER
    { Real_value (parse_real_value $1, $2) }
;

reference:
    TOK_IDENTIFIER
    { { ref_name = $1; lindex = -1; rindex = -1 } }
  | TOK_IDENTIFIER TOK_BRACKET_O TOK_DECIMAL_NUM TOK_BRACKET_C
    { { ref_name = $1; lindex = $3; rindex = -1 } }
  | TOK_IDENTIFIER TOK_BRACKET_O TOK_DECIMAL_NUM TOK_COLON TOK_DECIMAL_NUM
    TOK_BRACKET_C
    { if $3 = 0 && $3 = $5
      then { ref_name = $1; lindex = -1; rindex = -1 }
      else { ref_name = $1; lindex = $3; rindex = $5 } }
;

comment_text:
    /* empty */        { "" }
  | TOK_COMMENT_TEXT   { $1 }
;

version_text:
    /* empty */        { "" }
  | TOK_VERSION_TEXT   { $1 }
;

date_text:
    /* empty */        { "" }
  | TOK_DATE_TEXT      { $1 }
;
