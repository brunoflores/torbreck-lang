%{
  open Syntax
%}

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> TIMESFLOAT

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase / symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

%start <Syntax.context -> (Syntax.command list * Syntax.context)> topLevel

%%

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
topLevel:
  | EOF
    { fun ctx -> ([], ctx) }
  | c = command; SEMI; t = topLevel
    { fun ctx ->
        let cmd, ctx = c ctx in
        let cmds, ctx = t ctx in
        ((cmd :: cmds), ctx) }

/* A top-level command */
command:
  | IMPORT; s = STRINGV
    { fun ctx -> (Import(s.v), ctx) }
  | t = term
    { fun ctx -> ((let t = t ctx in Eval (tmInfo t, t)), ctx) }
  | x = LCID; b = binder
    { fun ctx -> (Bind (x.i, x.v, b ctx), addname ctx x.v) }

/* Right-hand sides of top-level bindings */
binder:
  | SLASH
    { fun _ -> NameBind }
  | EQ; t = term
    { fun ctx -> TmAbbBind (t ctx) }

term:
  | a = appTerm
    { a }
  | i = IF; t1 = term; THEN; t2 = term; ELSE; t3 = term
    { fun ctx -> TmIf (i, t1 ctx, t2 ctx, t3 ctx) }
  | i = LET; id = LCID; EQ; t1 = term; IN; t2 = term
    { fun ctx -> TmLet (i, id.v, t1 ctx, t2 (addname ctx id.v)) }
  | i = LET USCORE EQ t1 = term IN t2 = term
    { fun ctx -> TmLet(i, "_", t1 ctx, t2 (addname ctx "_")) }
  | i = LAMBDA; id = LCID; DOT; t = term
    { fun ctx ->
        let ctx' = addname ctx id.v in
        TmAbs (i, id.v, t ctx') }
  | i = LAMBDA; USCORE; DOT; t = term
    { fun ctx ->
        let ctx' = addname ctx "_" in
        TmAbs(i, "_", t ctx') }

appTerm:
  | p = pathTerm
    { p }
  | a = appTerm; p = pathTerm
    { fun ctx ->
        let e1 = a ctx in
        let e2 = p ctx in
        TmApp (tmInfo e1,e1,e2) }
  | i = SUCC; p = pathTerm
    { fun ctx -> TmSucc (i, p ctx) }
  | i = PRED; p = pathTerm
    { fun ctx -> TmPred (i, p ctx) }
  | i = ISZERO; p = pathTerm
    { fun ctx -> TmIsZero (i, p ctx) }
  | i = TIMESFLOAT; p1 = pathTerm; p2 = pathTerm
    { fun ctx -> TmTimesFloat (i, p1 ctx, p2 ctx) }

pathTerm:
  | p = pathTerm; i = DOT; id = LCID
    { fun ctx ->
        TmProj (i, p ctx, id.v) }
  | p = pathTerm; i = DOT; v = INTV
    { fun ctx ->
        TmProj (i, p ctx, string_of_int v.v) }
  | t = aTerm
    { t }

/* Atomic terms are ones that never require extra parentheses */
aTerm:
  | LPAREN; t = term; RPAREN
    { t }
  | s = STRINGV
    { fun _ -> TmString (s.i, s.v) }
  | id = LCID
    { fun ctx -> TmVar (id.i, name_to_index id.i ctx id.v, ctxlength ctx) }
  | t = TRUE
    { fun _ -> TmTrue (t) }
  | f = FALSE
    { fun _ -> TmFalse (f) }
  | i = LCURLY; f = fields; RCURLY
    { fun ctx -> TmRecord (i, f ctx 1) }
  | v = INTV
    { fun _ ->
        let rec f n = match n with
          | 0 -> TmZero (v.i)
          | n -> TmSucc (v.i, f (n - 1))
        in f v.v }
  | f = FLOATV
    { fun _ -> TmFloat (f.i, f.v) }

fields:
  | /* Empty */
    { fun _ _ -> [] }
  | x = neFields
    { x }

neFields:
  | f = field
    { fun ctx i -> [f ctx i] }
  | f = field; COMMA; ne = neFields
    { fun ctx i -> (f ctx i) :: (ne ctx (i + 1)) }

field:
  | id = LCID; EQ; t = term
    { fun ctx _ -> (id.v, t ctx) }
  | t = term
    { fun ctx i -> (string_of_int i, t ctx) }
