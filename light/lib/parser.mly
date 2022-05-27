%{
open Const
open Paraux
open Syntax
%}

/* Identifiers, prefixes and infixes */
%token <string> IDENT

/* Literals */
%token <int> INT
%token <string> STRING

/* Keywords */
%token LET
%token IN
%token VALUE

/* Special symbols */
%token EQUAL
%token LPAREN
%token RPAREN
%token SEMI
%token SEMISEMI
%token COLON
%token AND
%token MINUSGREATER
%token QUOTE

/* The end-of-file marker */
%token EOF

%start <Syntax.impl_phrase> implementation
%start <Syntax.intf_phrase> interface

%%

/* One phrase from a module implementation */

implementation:
  | e = expr SEMISEMI
    { make_impl (Zexpr e) }
  | EOF
    { raise End_of_file }

/* One phrase from a module interface */

interface:
  | VALUE v = value_decl SEMISEMI
    { make_intf (Zvaluedecl v) }

/* Expressions */

expr:
  | s = simple_expr
    { s }

simple_expr:
  | s = struct_constant
    { make_expr (Zconstant s) }

/* Constants */

struct_constant:
  | a = atomic_constant
    { SCatom a }

atomic_constant:
  | i = INT
    { ACint i }

/* Declarations */

value_decl:
  | v = value1_decl AND vs = value_decl
    { v :: vs }
  | v = value1_decl
    { [v] }

value1_decl:
  | id = ide COLON ty = typ
    { (id, ty, ValueNotPrim) }
  | id = ide COLON ty = typ EQUAL d = prim_decl
    { (id, ty, d) }

prim_decl:
  | i = INT s = STRING
    { find_primitive i s }

/* Identifiers */

ide:
  | i = IDENT
    { i }

/* Type expressions */

typ:
  | ty = simple_type
    { ty }
  | ty1 = typ MINUSGREATER ty2 = typ
    { make_typ (Ztypearrow (ty1, ty2)) }

simple_type:
  | ty = type_var
    { make_typ (Ztypevar ty) }

type_var:
  | QUOTE id = IDENT
    { id }
