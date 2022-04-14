%{
  open Syntax
%}

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> AS
%token <Support.Error.info> USTRING
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> UNIT
%token <Support.Error.info> UUNIT
%token <Support.Error.info> TIMESFLOAT
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> INERT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> FIX
%token <Support.Error.info> LETREC
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> NAT
%token <Support.Error.info> REF
%token <Support.Error.info> RREF
%token <Support.Error.info> TBOT
%token <Support.Error.info> TTOP
%token <Support.Error.info> SSOURCE
%token <Support.Error.info> SSINK

/* TODO
%token <Support.Error.info> TYPE
*/

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase / symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> ARROW
%token <Support.Error.info> COLON
%token <Support.Error.info> COMMA
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> GT
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> BANG

/* TODO
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> DARROW
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
*/

/* The starting production is the syntactic class [topLevel].
   The parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same
   pattern: they take a context as argument and return a fully
   parsed abstract syntax tree (and, if they involve any constructs
   that bind variables in some following phrase, a new context). */
%start <Syntax.context -> (Syntax.command list * Syntax.context)> topLevel

%%

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
topLevel:
  | EOF
    { fun ctx -> ([], ctx) }
  | cmd = Command; SEMI; top = topLevel
    { fun ctx ->
        let cmd', ctx' = cmd ctx in
        let cmds, ctx'' = top ctx' in
        ((cmd' :: cmds), ctx'') }

/* A top-level command */
Command:
  | IMPORT; s = STRINGV
    { fun ctx -> (Import(s.v), ctx) }
  | t = Term
    { fun ctx -> ((let t = t ctx in Eval (tmInfo t, t)), ctx) }
  | x = LCID; b = Binder
    { fun ctx -> (Bind (x.i, x.v, b ctx), addname ctx x.v) }
  | x = UCID; b = Tybinder
    { fun ctx -> (Bind (x.i, x.v, b ctx), addname ctx x.v) }

/* Right-hand sides of top-level bindings */
Binder:
  | COLON; ty = Type
    { fun ctx -> VarBind (ty ctx) }
  | SLASH
    { fun _ -> NameBind }
  | EQ; t = Term
    { fun ctx -> TmAbbBind (t ctx, None) }

/* All type expressions */
Type:
  | ty = ArrowType
    { ty }
  | RREF; ty = AType
    { fun ctx -> TyRef (ty ctx) }
  | SSOURCE; ty = AType
    { fun ctx -> TySource (ty ctx) }
  | SSINK; ty = AType
    { fun ctx -> TySink (ty ctx) }

/* Atomic types are those that never need extra parentheses */
AType:
  | LPAREN; ty = Type; RPAREN
    { ty }
  | TBOT
    { fun _ -> TyBot }
  | TTOP
    { fun _ -> TyTop }
  | USTRING
    { fun _ -> TyString }
  | BOOL
    { fun _ -> TyBool }
  | NAT
    { fun _ -> TyNat }
  | UFLOAT
    { fun _ -> TyFloat }
  | LT; tys = FieldTypes; GT
    { fun ctx -> TyVariant (tys ctx 1) }
  | UUNIT
    { fun _ -> TyUnit }
  | id = UCID
    { fun ctx ->
        if isnamebound ctx id.v then
          TyVar (name_to_index id.i ctx id.v, ctxlength ctx)
        else
          TyId id.v }
  | LCURLY; tys = FieldTypes; RCURLY
    { fun ctx -> TyRecord (tys ctx 1) }

AscribeTerm:
  | t = ATerm; fi = AS; ty = Type
    { fun ctx -> TmAscribe (fi, t ctx, ty ctx) }
  | t = ATerm
    { t }

FieldTypes:
  | /* Empty */
    { fun _ _ -> [] }
  | tys = NEFieldTypes
    { tys }

NEFieldTypes:
  | ty = FieldType
    { fun ctx i -> [ty ctx i] }
  | ty = FieldType; COMMA; tys = NEFieldTypes
    { fun ctx i -> (ty ctx i)::(tys ctx (i+1)) }

FieldType:
  | id = LCID; COLON; ty = Type
    { fun ctx _ -> (id.v, ty ctx) }
  | ty = Type
    { fun ctx i -> (string_of_int i, ty ctx) }

PathTerm:
  | p = PathTerm; fi = DOT; id = LCID
    { fun ctx -> TmProj (fi, p ctx, id.v) }
  | p = PathTerm; fi = DOT; v = INTV
    { fun ctx -> TmProj (fi, p ctx, string_of_int v.v) }
  | t = AscribeTerm
    { t }

/* An "arrow type" is a sequence of atomic types separated by arrows */
ArrowType:
  | ty1 = AType; ARROW; ty2 = ArrowType
    { fun ctx -> TyArr (ty1 ctx, ty2 ctx) }
  | ty = AType
    { ty }

Term:
  | a = AppTerm
    { a }

  | fi = IF; t1 = Term; THEN; t2 = Term; ELSE; t3 = Term
    { fun ctx -> TmIf (fi, t1 ctx, t2 ctx, t3 ctx) }

  | fi = CASE; t = Term; OF; cs = Cases
    { fun ctx ->
      TmCase (fi, t ctx, cs ctx) }

  | fi = LET; id = LCID; EQ; t1 = Term; IN; t2 = Term
    { fun ctx -> TmLet (fi, id.v, t1 ctx, t2 (addname ctx id.v)) }
  | fi = LET; USCORE; EQ; t1 = Term; IN; t2 = Term
    { fun ctx -> TmLet(fi, "_", t1 ctx, t2 (addname ctx "_")) }
  | fi = LETREC; id = LCID; COLON; ty = Type; EQ; t1 = Term; IN; t2 = Term
    { fun ctx ->
        let ctx' = addname ctx id.v in
        TmLet (fi, id.v, TmFix (fi, TmAbs (fi, id.v, ty ctx, t1 ctx')), t2 ctx') }

  | fi = LAMBDA; id = LCID; COLON; ty = Type; DOT; t = Term
    { fun ctx ->
        let ctx' = addname ctx id.v in
        TmAbs (fi, id.v, ty ctx', t ctx') }
  | fi = LAMBDA; USCORE; COLON; ty = Type; DOT; t = Term
    { fun ctx ->
        let ctx' = addname ctx "_" in
        TmAbs(fi, "_", ty ctx', t ctx') }

  | app1 = AppTerm; fi = COLONEQ; app2 = AppTerm
    { fun ctx -> TmAssign (fi, app1 ctx, app2 ctx) }

AppTerm:
  | p = PathTerm
    { p }
  | fi = TIMESFLOAT; p1 = PathTerm; p2 = PathTerm
    { fun ctx -> TmTimesFloat (fi, p1 ctx, p2 ctx) }
  | a = AppTerm; p = PathTerm
    { fun ctx ->
        let e1 = a ctx in
        let e2 = p ctx in
        TmApp (tmInfo e1, e1, e2) }
  | fi = FIX; p = PathTerm
    { fun ctx -> TmFix (fi, p ctx) }
  | fi = SUCC; p = PathTerm
    { fun ctx -> TmSucc (fi, p ctx) }
  | fi = PRED; p = PathTerm
    { fun ctx -> TmPred (fi, p ctx) }
  | fi = ISZERO; p = PathTerm
    { fun ctx -> TmIsZero (fi, p ctx) }

  | fi = REF; p = PathTerm
    { fun ctx -> TmRef (fi, p ctx) }
  | fi = BANG; p = PathTerm
    { fun ctx -> TmDeref (fi, p ctx) }

TermSeq:
  | t = Term
    { t }
  | t = Term; fi = SEMI; seq = TermSeq
    { fun ctx -> TmApp (fi, TmAbs (fi, "_", TyUnit, seq (addname ctx "_")), t ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm:
  | LPAREN; t = TermSeq; RPAREN
    { t }
  | s = STRINGV
    { fun _ -> TmString (s.i, s.v) }
  | t = TRUE
    { fun _ -> TmTrue (t) }
  | f = FALSE
    { fun _ -> TmFalse (f) }
  | fi = LT; id = LCID; EQ; t = Term; GT; AS; ty = Type
    { fun ctx -> TmTag (fi, id.v, t ctx, ty ctx) }
  | fi = UNIT
    { fun _ -> TmUnit fi }
  | id = LCID
    { fun ctx -> TmVar (id.i, name_to_index id.i ctx id.v, ctxlength ctx) }
  | fi = LCURLY; f = Fields; RCURLY
    { fun ctx -> TmRecord (fi, f ctx 1) }
  | v = INTV
    { fun _ ->
        let rec f n = match n with
          | 0 -> TmZero (v.i)
          | n -> TmSucc (v.i, f (n - 1))
        in f v.v }
  | f = FLOATV
    { fun _ -> TmFloat (f.i, f.v) }
  | fi = INERT; LSQUARE; ty = Type; RSQUARE
    { fun ctx -> TmInert (fi, ty ctx) }

Cases:
  | c = Case
    { fun ctx -> [c ctx] }
  | c = Case; VBAR; cs = Cases
    { fun ctx -> (c ctx) :: (cs ctx) }

Case:
  | LT; id1 = LCID; EQ; id2 = LCID; GT; DDARROW; app = AppTerm
    { fun ctx ->
        let ctx' = addname ctx id2.v in
        (id1.v, (id2.v, app ctx')) }

Fields:
  | /* Empty */
    { fun _ _ -> [] }
  | x = NeFields
    { x }

NeFields:
  | f = Field
    { fun ctx i -> [f ctx i] }
  | f = Field; COMMA; ne = NeFields
    { fun ctx i -> (f ctx i) :: (ne ctx (i + 1)) }

Field:
  | id = LCID; EQ; t = Term
    { fun ctx _ -> (id.v, t ctx) }
  | t = Term
    { fun ctx i -> (string_of_int i, t ctx) }

Tybinder:
  | /* Empty */
    { fun _ -> TyVarBind }
  | EQ; ty = Type
    { fun ctx -> TyAbbBind (ty ctx) }
