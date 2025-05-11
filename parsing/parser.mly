/* The parser definition */


%{

open Asttypes
open Longident
open Parsetree
open Ast_helper

%}

/* Tokens */

%token <char> CHAR            "'a'" (* just an example *)
%token COLON                  ":"
%token ELSE                   "else"
%token EQUAL                  "="
%token FALSE                  "false"
%token <string> FLOAT "42.0" (* just an example *)
%token FUN                    "fun"
%token FUNCTION               "function"
%token IF                     "if"
%token IN                     "in"
%token <string> INFIXOP0      "!="   (* just an example *)
%token <string> INT "42"  (* just an example *)
%token LET                    "let"
%token <string> LIDENT        "lident" (* just an example *)
%token REC                    "rec"
%token SEMI                   ";"
%token <string * Location.t> STRING "\"hello\"" (* just an example *)
%token THEN                   "then"
%token TRUE                   "true"
%token EOF                    ""

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation

%%

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;
