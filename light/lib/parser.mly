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
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX2

/* Literals */
%token <int> INT
%token <string> STRING
%token <char> CHAR

/* Keywords */
%token LET
%token REC
%token IN
%token VALUE
%token IF
%token THEN
%token ELSE
%token PREF
%token TYPE
%token EXCEPTION
%token OF
%token MUTABLE
%token TRY
%token WITH
%token WHEN
%token WHILE
%token DO
%token DONE

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
%token COMMA
%token BAR
%token COLONEQUAL
%token DOTLPAREN      /* ".(" */
%token BARBAR         /* "||" */
%token SHARP          /* "#" */
%token UNDERUNDER     /* "__" */

/* The end-of-file marker */
%token EOF

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right MINUSGREATER
%right prec_if
%right SEMI
%right COLONEQUAL
%left BAR
%left BARBAR
%left INFIX0 EQUAL    /* comparisons */
%left INFIX2          /* additives, subtractives */
%left DOTLPAREN
%right PREFIX         /* prefix operators, e.g. ! */

/* Entry points */

%start <Syntax.impl_phrase> implementation
%start <Syntax.intf_phrase> interface

%%

/* One phrase from a module implementation */

implementation:
  | e = expr SEMISEMI
    { make_impl (Zexpr e) }
  | LET bs = binding_list SEMISEMI
    { make_impl (Zletdef (false, bs)) }
  | LET REC bs = binding_list SEMISEMI
    { make_impl (Zletdef (true, bs)) }
  | TYPE decl = type_decl SEMISEMI
    { make_impl (Ztypedef decl) }
  | SHARP d = directive SEMISEMI
    { make_impl (Zimpldirective d) }
  | EXCEPTION e = exc_decl SEMISEMI
    { make_impl (Zexcdef e) }
  | EOF
    { raise End_of_file }

/* One phrase from a module interface */

interface:
  | VALUE v = value_decl SEMISEMI
    { make_intf (Zvaluedecl v) }
  | TYPE ty = type_decl SEMISEMI
    { make_intf (Ztypedecl ty) }
  | SHARP d = directive SEMISEMI
    { make_intf (Zintfdirective d) }
  | EXCEPTION e = exc_decl SEMISEMI
    { make_intf (Zexcdecl e) }
  | EOF
    { raise End_of_file }

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
  | e1 = expr EQUAL e2 = expr
    { make_binop "=" e1 e2 }
  | e1 = expr BARBAR e2 = expr
    { make_binop "||" e1 e2 }
  | e1 = expr COLONEQUAL e2 = expr
    { make_binop ":=" e1 e2 }
  | WHILE cond = expr DO body = opt_expr DONE
    { make_expr (Zwhile (cond, body)) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr %prec prec_if
    { make_expr (Zcondition (e1, e2, e3)) }
  | e1 = expr SEMI e2 = expr
    { make_expr (Zsequence (e1, e2)) }
  | e = expr SEMI
    { e }
  | LET b = binding_list IN e = expr  %prec prec_let
    { make_expr (Zlet (false, b, e)) }
  | LET REC b = binding_list IN e = expr %prec prec_let
    { make_expr (Zlet (true, b, e)) }
  | TRY e = expr WITH m = try_match
    { make_expr (Ztrywith (e, m)) }

simple_expr:
  | s = struct_constant
    { make_expr (Zconstant s) }
  | e = ext_ident
    { expr_constr_or_ident e }
  | LPAREN e = opt_expr RPAREN
    { e }
  | p = PREFIX e = simple_expr
    { make_unop p e }
  | lexpr = simple_expr DOTLPAREN rexpr = expr RPAREN
    { make_binop "vect_item" lexpr rexpr }

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
  | c = CHAR
    { ACchar c }

/* Declarations */

value_decl:
  | v = value1_decl AND vs = value_decl
    { v :: vs }
  | v = value1_decl
    { [v] }

type_decl:
  | ty = type1_decl AND more = type_decl
    { ty :: more }
  | ty = type1_decl
    { [ty] }

exc_decl:
  | c = constr1_decl AND cs = exc_decl
    { c :: cs }
  | c = constr1_decl
    { [c] }

constr_decl:
  | decl = constr1_decl BAR decls = constr_decl
    { decl :: decls }
  | decl = constr1_decl
    { [decl] }

value1_decl:
  | id = ide COLON ty = typ
    { (id, ty, ValueNotPrim) }
  | id = ide COLON ty = typ EQUAL d = prim_decl
    { (id, ty, d) }

prim_decl:
  | arity = INT name = STRING
    { find_primitive arity name }

type1_decl:
  | params = type_params id = IDENT ty_def = type1_def
    { (id, params, ty_def) }

type1_def:
  | /* epsilon */
    { Zabstract_type }
  | EQUAL opt_bar c = constr_decl
    { Zvariant_type c }

type_params:
  | LPAREN tvars = type_var_list RPAREN
    { tvars }
  | tvar = type_var
    { [tvar] }
  | /* empty */
    { [] }

type_var_list:
  | tvar = type_var COMMA tvars = type_var_list
    { tvar :: tvars }
  | tvar = type_var
    { [tvar] }

constr1_decl:
  | i = ide OF m = mutable_option ty = typ
    { Zconstr1decl (i, ty, m) }
  | i = ide
    { Zconstr0decl i }

mutable_option:
  | MUTABLE
    { Mutable }
  | /* epsilon */
    { Notmutable }

/* Definitions by pattern matchings */

opt_bar:
  | BAR
    { () }
  | /* epsilon */
    { () }

action:
  | MINUSGREATER e = expr
    { e }
  | WHEN e1 = expr MINUSGREATER e2 = expr
    { make_expr (Zwhen (e1, e2)) }

try_match:
  | BAR p = pattern a = action BAR more = try_match
    { (p, a) :: more }
  | p = pattern a = action
    { [(p, a)] }

binding_list:
  | b = binding AND bs = binding_list
    { b :: bs }
  | b = binding
    { [b] }

binding:
  | id = ide pat = simple_pattern_list EQUAL e = expr
    { (pat_constr_or_var id, make_expr (Zfunction [pat, e])) }
  | pat = simple_pattern EQUAL e = expr
    { (pat, e) }

/* Patterns */

pattern:
  | pat = simple_pattern
    { pat }
  | id = ext_ident pat = simple_pattern
    { make_pat (Zconstruct1pat (find_constructor id, pat)) }
  | p1 = pattern BAR p2 = pattern
    { make_pat (Zorpat (p1, p2)) }

simple_pattern_list:
  | pat = simple_pattern pats = simple_pattern_list
    { pat :: pats }
  | pat = simple_pattern
    { [pat] }

simple_pattern:
  | id = IDENT
    { pat_constr_or_var id }
  | LPAREN pat = pattern RPAREN
    { pat }

/* Identifiers */

ide:
  | i = IDENT
    { i }
  | PREF i = infx
    { i }

infx:
  | i = INFIX0
  | i = INFIX2
  | i = PREFIX
    { i }
  | BARBAR
    { "||" }
  | COLONEQUAL
    { ":=" }
  | EQUAL
    { "=" }

qual_ident:
  | qual = IDENT UNDERUNDER id = ide
    { {qual; id} }

ext_ident:
  | id = qual_ident
    { GRmodname id }
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

/* Directives */

directive:
  | id = IDENT s = STRING
    { Zdir (id, s) }
