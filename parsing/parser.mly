/* The parser definition */

%{

(* open Asttypes *)
(* open Longident *)
open Parsetree
open Asthelpers

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
}

let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
(* let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d *)
let mkconst ~loc c = Const.mk ~loc:(make_loc loc) c

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

%}

/* Tokens */

%token <char> CHAR            "'a'" (* just an example *)
/* %token COLON                  ":" */
/* %token ELSE                   "else" */
/* %token EQUAL                  "=" */
/* %token FALSE                  "false" */
%token <string> FLOAT "42.0" (* just an example *)
/* %token FUN                    "fun" */
/* %token FUNCTION               "function" */
/* %token IF                     "if" */
/* %token IN                     "in" */
/* %token <string> INFIXOP0      "!="   (* just an example *) */
%token <string> INT "42"  (* just an example *)
/* %token LET                    "let" */
/* %token <string> LIDENT        "lident" (* just an example *) */
/* %token REC                    "rec" */
/* %token SEMI                   ";" */
%token <string * Location.t> STRING "\"hello\"" (* just an example *)
/* %token THEN                   "then" */
/* %token TRUE                   "true" */
%token EOF                    ""
%token EOL                    "\\n"      (* not great, but EOL is unused *)

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation

%%

/* Macros */
%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }

%inline text_str(symb): symb
  { text_str $startpos @ [$1] }

(* An expression with attributes, wrapped as a structure item. *)
%inline str_exp:
  e = seq_expr
    { mkstrexp e }
;

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;

structure:
  | str_exp { [$1] }

fun_seq_expr:
  | fun_expr   /* %prec below_SEMI */  { $1 }
;

seq_expr:
  | or_function(fun_seq_expr) { $1 }
;

fun_expr:
    simple_expr /* %prec below_HASH */
      { $1 }

simple_expr:
  | mkexp(simple_expr_)
      { $1 }

%inline simple_expr_:
  | constant
      { Pexp_constant $1 }

constant:
  | INT          { let n = $1 in
                   mkconst ~loc:$sloc (Pconst_integer n) }
  | CHAR         { mkconst ~loc:$sloc (Pconst_char $1) }
  | STRING       { let (s, strloc) = $1 in
                   mkconst ~loc:$sloc (Pconst_string (s, strloc)) }
  | FLOAT        { let f = $1 in
                   mkconst ~loc:$sloc (Pconst_float f) }
;

%inline or_function(EXPR):
  | EXPR
      { $1 }
