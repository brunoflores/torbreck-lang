%{
open Const
open Paraux
open Syntax
open Globals
open Primdecl
open Builtins
%}

/* Identifiers, prefixes and infixes */
%token <string> IDENT
%token <string> INFIX0
%token <string> INFIX2

/* Literals */
%token <int> INT
%token <string> STRING

/* Keywords */
%token LET
%token REC
%token IN
%token VALUE
%token IF
%token THEN
%token ELSE

/* Special symbols */
%token EQUAL
%token LPAREN
%token RPAREN
%token SEMISEMI
%token COLON
%token AND
%token MINUSGREATER
%token QUOTE

/* The end-of-file marker */
%token EOF

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right MINUSGREATER
%right prec_if
%left INFIX0 /* comparisons */
%left INFIX2 /* additives, subtractives */

/* Entry points */

%start <Syntax.impl_phrase> implementation
%start <Syntax.intf_phrase> interface

%%

/* One phrase from a module implementation */

implementation:
  | e = expr SEMISEMI
    { make_impl (Zexpr e) }
  /* | LET REC b = binding_list SEMISEMI */
  /*   { make_impl (Zletdef (true, b)) } */
  | EOF
    { raise End_of_file }

/* One phrase from a module interface */

interface:
  | VALUE v = value_decl SEMISEMI
    { make_intf (Zvaluedecl v) }

/* Expressions */

expr:
  | e = simple_expr
    { e }
  | e = simple_expr more = simple_expr_list
    { make_apply (e, more) }
  | e1 = expr i = INFIX2 e2 = expr
    { make_binop i e1 e2 }
  | e1 = expr i = INFIX0 e2 = expr
    { make_binop i e1 e2 }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr %prec prec_if
    { make_expr (Zcondition (e1, e2, e3)) }
  | LET REC b = binding_list IN e = expr %prec prec_let
    { make_expr (Zlet (true, b, e)) }

simple_expr:
  | s = struct_constant
    { make_expr (Zconstant s) }
  | e = ext_ident
    { expr_constr_or_ident e }
  | LPAREN e = opt_expr RPAREN
    { e }

simple_expr_list:
  | s = simple_expr more = simple_expr_list
    { s :: more }
  | s = simple_expr
    { [s] }

opt_expr:
  | e = expr
    { e }
  | /*empty */
    { make_expr (Zconstruct0 (constr_void)) }

/* Constants */

struct_constant:
  | a = atomic_constant
    { SCatom a }

atomic_constant:
  | i = INT
    { ACint i }
  | s = STRING
    { ACstring s }

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

ext_ident:
  | id = ide
    { GRname id }

/* Type expressions */

typ:
  | ty = simple_type
    { ty }
  | ty1 = typ MINUSGREATER ty2 = typ
    { make_typ (Ztypearrow (ty1, ty2)) }

simple_type:
  | ty = type_var
    { make_typ (Ztypevar ty) }
  | id = ext_ident
    { make_typ (Ztypeconstr (id, [])) }
  | ty = simple_type id = ext_ident
    { make_typ (Ztypeconstr (id, [ty])) }

type_var:
  | QUOTE id = IDENT
    { id }

/* Definitions by pattern matchings */

binding_list:
  | b = binding
    { [b] }

binding:
  | id = ide pat = simple_pattern_list EQUAL e = expr
    { (pat_constr_or_var id, make_expr (Zfunction [pat, e])) }

/* Patterns */

simple_pattern_list:
  | pat = simple_pattern
    { [pat] }

pattern:
  | pat = simple_pattern
    { pat }

simple_pattern:
  | id = ide
    { pat_constr_or_var id }
  | LPAREN pat = pattern RPAREN
    { pat }
