open Format
open Support.Error

type ty =
  | TyBot
  | TyTop
  | TyRef of ty
  | TySource of ty
  | TySink of ty
  | TyId of string
  | TyVar of int * int
  | TyUnit
  | TyFloat
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyString
  | TyBool
  | TyArr of ty * ty
  | TyNat
  | TyRec of string * ty
[@@deriving show { with_path = false }]

type term =
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term
  | TmAssign of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmCase of info * term * (string * (string * term)) list
  | TmTag of info * string * term * ty
  | TmUnit of info
  | TmVar of info * int * int
  | TmFloat of info * float
  | TmTimesFloat of info * term * term
  | TmLet of info * string * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmAbs of info * string * ty option * term
  | TmApp of info * term * term
  | TmFix of info * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmError of info
  | TmTry of info * term * term
[@@deriving show { with_path = false }]

type binding =
  | NameBind
  | TmAbbBind of term * ty option
  | VarBind of ty
  | TyVarBind
  | TyAbbBind of ty
[@@deriving show { with_path = false }]

type command = Eval of info * term | Bind of info * string * binding
[@@deriving show { with_path = false }]

type context = (string * binding) list

let emptycontext = []
let ctxlength ctx = List.length ctx
let addbinding ctx x bind = (x, bind) :: ctx
let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x ^ "'")
  else ((x, NameBind) :: ctx, x)

let index_to_name _ ctx x =
  try
    let xn, _ = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in
    failwith (msg x (List.length ctx))

let rec name_to_index fi ctx x =
  match ctx with
  | [] -> error fi @@ Printf.sprintf "Identifier %s is unbound" x
  | (y, _) :: rest -> if y = x then 0 else 1 + name_to_index fi rest x

let tymap onvar c tyT =
  let rec walk c tyT =
    match tyT with
    | TyBot -> TyBot
    | TyTop -> TyTop
    | TyRec (x, tyT) -> TyRec (x, walk (c + 1) tyT)
    | TyRef tyT1 -> TyRef (walk c tyT1)
    | TySource tyT1 -> TySource (walk c tyT1)
    | TySink tyT1 -> TySink (walk c tyT1)
    | TyString -> TyString
    | TyId _ as tyT -> tyT
    | TyVariant fieldTys ->
        TyVariant (List.map (fun (li, tyTi) -> (li, walk c tyTi)) fieldTys)
    | TyUnit -> TyUnit
    | TyFloat -> TyFloat
    | TyRecord fieldTys ->
        TyRecord (List.map (fun (li, tyTi) -> (li, walk c tyTi)) fieldTys)
    | TyVar (x, n) -> onvar c x n
    | TyArr (tyT1, tyT2) -> TyArr (walk c tyT1, walk c tyT2)
    | TyBool -> TyBool
    | TyNat -> TyNat
  in
  walk c tyT

let tmmap onvar ontype c t =
  let rec walk c t =
    match t with
    | TmLoc _ as t -> t
    | TmRef (fi, t1) -> TmRef (fi, walk c t1)
    | TmDeref (fi, t1) -> TmDeref (fi, walk c t1)
    | TmAssign (fi, t1, t2) -> TmAssign (fi, walk c t1, walk c t2)
    | TmAscribe (fi, t1, tyT1) -> TmAscribe (fi, walk c t1, ontype c tyT1)
    | TmString _ as t -> t
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmTrue _ as t -> t
    | TmFalse _ as t -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
    | TmTag (fi, l, t1, tyT) -> TmTag (fi, l, walk c t1, ontype c tyT)
    | TmCase (fi, t, cases) ->
        TmCase
          ( fi,
            walk c t,
            List.map (fun (li, (xi, ti)) -> (li, (xi, walk (c + 1) ti))) cases
          )
    | TmLet (fi, x, t1, t2) -> TmLet (fi, x, walk c t1, walk (c + 1) t2)
    | TmUnit _ as t -> t
    | TmInert (fi, tyT) -> TmInert (fi, ontype c tyT)
    | TmFloat _ as t -> t
    | TmTimesFloat (fi, t1, t2) -> TmTimesFloat (fi, walk c t1, walk c t2)
    | TmProj (fi, t1, l) -> TmProj (fi, walk c t1, l)
    | TmRecord (fi, fields) ->
        let fieldswalked = List.map (fun (li, ti) -> (li, walk c ti)) fields in
        TmRecord (fi, fieldswalked)
    | TmAbs (fi, x, tyT1, t2) -> TmAbs (fi, x, ontype c tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmFix (fi, t1) -> TmFix (fi, walk c t1)
    | TmZero fi -> TmZero fi
    | TmSucc (fi, t1) -> TmSucc (fi, walk c t1)
    | TmPred (fi, t1) -> TmPred (fi, walk c t1)
    | TmIsZero (fi, t1) -> TmIsZero (fi, walk c t1)
    | TmError _ as t -> t
    | TmTry (fi, t1, t2) -> TmTry (fi, walk c t1, walk c t2)
  in
  walk c t

let typeshiftabove d c tyT =
  tymap
    (fun c x n -> if x >= c then TyVar (x + d, n + d) else TyVar (x, n + d))
    c tyT

let termshiftabove d c t =
  tmmap
    (fun fi c x n ->
      if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    (typeshiftabove d) c t

let termshift d t = termshiftabove d 0 t
let typeshift d tyT = typeshiftabove d 0 tyT

let bindingshift d bind =
  match bind with
  | NameBind -> NameBind
  | TmAbbBind (t, tyT) ->
      let tyT' =
        match tyT with None -> None | Some tyT -> Some (typeshift d tyT)
      in
      TmAbbBind (termshift d t, tyT')
  | VarBind tyT -> VarBind (typeshift d tyT)
  | TyVarBind -> TyVarBind
  | TyAbbBind tyT -> TyAbbBind (typeshift d tyT)

let termsubst j s t =
  tmmap
    (fun fi j x n -> if x = j then termshift j s else TmVar (fi, x, n))
    (fun _ tyT -> tyT)
    j t

(* Beta-reduction rule.
   1) The term being substituted for the bound variable is first shifted by one
   2) then the substitution is made
   3) then the whole result is shifted down by one to accounr for the fact that
      the bound variable has been used up. *)
let termsubsttop s t = termshift (-1) (termsubst 0 (termshift 1 s) t)

let typesubst tyS j tyT =
  tymap (fun j x n -> if x = j then typeshift j tyS else TyVar (x, n)) j tyT

let typesubsttop tyS tyT = typeshift (-1) (typesubst (typeshift 1 tyS) 0 tyT)

let tytermsubst tyS j t =
  tmmap
    (fun fi _ x n -> TmVar (fi, x, n))
    (fun j tyT -> typesubst tyS j tyT)
    j t

let tytermsubsttop tyS t = termshift (-1) (tytermsubst (typeshift 1 tyS) 0 t)

let getbinding fi ctx i =
  try
    let _, bind = List.nth ctx i in
    bindingshift (i + 1) bind
  with Failure _ ->
    let msg =
      Printf.sprintf "variable lookup failure: offset: %d, ctx size: %d"
    in
    let offset = i in
    let length = List.length ctx in
    error fi @@ msg offset length

let string_of_context (ctx : (string * binding) list) =
  let idx = ref 0 in
  let string_of_item (id, b) =
    let idx' = !idx in
    incr idx;
    Format.sprintf "\n\t[%d] %s: %s" idx' id (show_binding b)
  in
  List.fold_left (fun acc x -> acc ^ string_of_item x) "" ctx

let gettypefromcontext fi ctx i =
  match getbinding fi ctx i with
  | VarBind tyT -> tyT
  | TyAbbBind tyT -> tyT
  | TmAbbBind (_, Some tyT) -> tyT
  | TmAbbBind (_, None) ->
      error fi
        (Printf.sprintf "no type recorded for variable %s"
           (index_to_name fi ctx i))
  | _ as b ->
      error fi
        (Printf.sprintf
           "wrong kind of binding for variable %s at index %d: %s\n\
            with context:\n\
            %s"
           (index_to_name fi ctx i) i (show_binding b) (string_of_context ctx))

let tmInfo t =
  match t with
  | TmLoc (fi, _) -> fi
  | TmRef (fi, _) -> fi
  | TmDeref (fi, _) -> fi
  | TmAssign (fi, _, _) -> fi
  | TmAscribe (fi, _, _) -> fi
  | TmString (fi, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmTag (fi, _, _, _) -> fi
  | TmCase (fi, _, _) -> fi
  | TmUnit fi -> fi
  | TmVar (fi, _, _) -> fi
  | TmFloat (fi, _) -> fi
  | TmTimesFloat (fi, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmProj (fi, _, _) -> fi
  | TmRecord (fi, _) -> fi
  | TmInert (fi, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmFix (fi, _) -> fi
  | TmZero fi -> fi
  | TmSucc (fi, _) -> fi
  | TmPred (fi, _) -> fi
  | TmIsZero (fi, _) -> fi
  | TmError fi -> fi
  | TmTry (fi, _, _) -> fi

let obox0 () = open_hvbox 0
let obox () = open_hvbox 2
let cbox () = close_box ()
let break () = print_break 0 0

let rec printty_Type outer ctx tyT =
  match tyT with
  | TyRef tyT ->
      print_string "Ref ";
      printty_AType false ctx tyT
  | TySource tyT ->
      print_string "Source ";
      printty_AType false ctx tyT
  | TySink tyT ->
      print_string "Sink ";
      printty_AType false ctx tyT
  | TyRec (x, tyT) ->
      let ctx', x = pickfreshname ctx x in
      obox ();
      print_string "Rec ";
      print_string x;
      print_string ".";
      print_space ();
      printty_Type outer ctx' tyT;
      cbox ()
  | tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      obox0 ();
      printty_AType false ctx tyT1;
      if outer then print_string " ";
      print_string "->";
      if outer then print_space () else break ();
      printty_ArrowType outer ctx tyT2;
      cbox ()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT =
  match tyT with
  | TyBot -> print_string "Bot"
  | TyTop -> print_string "Top"
  | TyString -> print_string "String"
  | TyBool -> print_string "Bool"
  | TyVariant fields ->
      let pf i (li, tyTi) =
        if li <> string_of_int i then (
          print_string li;
          print_string ":");
        printty_Type false ctx tyTi
      in
      let rec p i l =
        match l with
        | [] -> ()
        | [ f ] -> pf i f
        | f :: rest ->
            pf i f;
            print_string ", ";
            if not outer then break ();
            p (i + 1) rest
      in
      print_string "<";
      open_hovbox 0;
      p 1 fields;
      print_string ">";
      cbox ()
  | TyUnit -> print_string "Unit"
  | TyId b -> print_string b
  | TyFloat -> print_string "Float"
  | TyRecord fields ->
      let pf i (li, tyTi) =
        if li <> string_of_int i then (
          print_string li;
          print_string ": ");
        printty_Type false ctx tyTi
      in
      let rec p i l =
        match l with
        | [] -> ()
        | [ f ] -> pf i f
        | f :: rest ->
            pf i f;
            print_string ", ";
            if not outer then break ();
            p (i + 1) rest
      in
      print_string "{ ";
      open_hovbox 0;
      p 1 fields;
      print_string " }";
      cbox ()
  | TyVar (x, n) ->
      if ctxlength ctx = n then print_string (index_to_name dummyinfo ctx x)
      else
        print_string
          ("[bad index: " ^ string_of_int x ^ "/" ^ string_of_int n ^ " in {"
          ^ List.fold_left (fun s (x, _) -> s ^ " " ^ x) "" ctx
          ^ " }]")
  | TyNat -> print_string "Nat"
  | tyT ->
      print_string "(";
      printty_Type outer ctx tyT;
      print_string ")"

let printty ctx tyT = printty_Type true ctx tyT

let rec printtm_term outer ctx t =
  let small t = match t with TmVar _ -> true | _ -> false in
  match t with
  | TmIf (_, t1, t2, t3) ->
      obox0 ();
      print_string "if ";
      printtm_term false ctx t1;
      print_space ();
      print_string "then ";
      printtm_term false ctx t2;
      print_space ();
      print_string "else ";
      printtm_term false ctx t3;
      cbox ()
  | TmLet (_, x, t1, t2) ->
      obox0 ();
      print_string "let ";
      print_string x;
      print_string " = ";
      printtm_term false ctx t1;
      print_space ();
      print_string "in";
      print_space ();
      printtm_term false (addname ctx x) t2;
      cbox ()
  | TmAbs (_, x, tyT1, t2) ->
      let ctx', x' = pickfreshname ctx x in
      obox ();
      print_string "lambda ";
      print_string x';
      print_string ":";
      printty_Type false ctx tyT1;
      print_string ".";
      if small t2 && not outer then break () else print_space ();
      printtm_term outer ctx' t2;
      cbox ()
  | TmAssign (_, t1, t2) ->
      obox ();
      printtm_appterm false ctx t1;
      print_string " := ";
      printtm_appterm false ctx t2;
      cbox ()
  | TmCase (_, t, cases) ->
      obox ();
      print_string "case ";
      printtm_term false ctx t;
      print_string " of";
      print_space ();
      let pc (li, (xi, ti)) =
        let ctx', xi' = pickfreshname ctx xi in
        print_string "<";
        print_string li;
        print_string "=";
        print_string xi';
        print_string ">==>";
        printtm_term false ctx' ti
      in
      let rec p l =
        match l with
        | [] -> ()
        | [ c ] -> pc c
        | c :: rest ->
            pc c;
            print_space ();
            print_string "|";
            p rest
      in
      p cases;
      cbox ()
  | TmFix (_, t1) ->
      obox ();
      print_string "fix ";
      printtm_term false ctx t1;
      cbox ()
  | TmTry (_, t1, t2) ->
      obox0 ();
      print_string "try ";
      printtm_term false ctx t1;
      print_space ();
      print_string "with ";
      printtm_term false ctx t2;
      cbox ()
  | t -> printtm_appterm outer ctx t

and printtm_appterm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      obox0 ();
      printtm_appterm false ctx t1;
      print_space ();
      printtm_appterm false ctx t2;
      cbox ()
  | TmRef (_, t1) ->
      obox ();
      print_string "ref ";
      printtm_aterm false ctx t1;
      cbox ()
  | TmDeref (_, t1) ->
      obox ();
      print_string "!";
      printtm_aterm false ctx t1;
      cbox ()
  | TmPred (_, t1) ->
      print_string "pred ";
      printtm_aterm false ctx t1
  | TmIsZero (_, t1) ->
      print_string "iszero ";
      printtm_aterm false ctx t1
  | TmTimesFloat (_, t1, t2) ->
      print_string "timesfloat ";
      printtm_aterm false ctx t1;
      print_string " ";
      printtm_aterm false ctx t2
  | t -> printtm_pathterm outer ctx t

and printtm_ascribeterm outer ctx t =
  match t with
  | TmAscribe (_, t1, tyT1) ->
      obox0 ();
      printtm_appterm false ctx t1;
      print_space ();
      print_string "as ";
      printty_Type false ctx tyT1;
      cbox ()
  | t -> printtm_aterm outer ctx t

and printtm_pathterm outer ctx t =
  match t with
  | TmProj (_, t1, l) ->
      printtm_aterm false ctx t1;
      print_string ".";
      print_string l
  | t -> printtm_ascribeterm outer ctx t

and printtm_aterm outer ctx t =
  match t with
  | TmUnit _ -> print_string "unit"
  | TmLoc (_, l) ->
      print_string "<loc #";
      print_int l;
      print_string ">"
  | TmTag (_, l, t, tyT) ->
      obox ();
      print_string "<";
      print_string l;
      print_string "=";
      printtm_term false ctx t;
      print_string ">";
      print_space ();
      print_string "as ";
      printty_Type outer ctx tyT;
      cbox ()
  | TmFloat (_, s) -> print_string (string_of_float s)
  | TmInert (_, tyT) ->
      print_string "inert[";
      printty_Type false ctx tyT;
      print_string "]"
  | TmString (_, s) -> print_string ("\"" ^ s ^ "\"")
  | TmVar (i, x, n) ->
      if ctxlength ctx = n then print_string (index_to_name i ctx x)
      else
        print_string
          ("[bad index: " ^ string_of_int x ^ "/" ^ string_of_int n ^ " in {"
          ^ List.fold_left (fun s (x, _) -> s ^ " " ^ x) "" ctx
          ^ " }]")
  | TmTrue _ -> print_string "true"
  | TmFalse _ -> print_string "false"
  | TmRecord (_, fields) ->
      let pf i (li, ti) =
        if li <> string_of_int i then (
          print_string li;
          print_string "=");
        printtm_term false ctx ti
      in
      let rec p i l =
        match l with
        | [] -> ()
        | [ f ] -> pf i f
        | f :: rest ->
            pf i f;
            print_string ",";
            if outer then print_space () else break ();
            p (i + 1) rest
      in
      print_string "{ ";
      open_hovbox 0;
      p 1 fields;
      print_string " }";
      cbox ()
  | TmZero _ -> print_string "0"
  | TmSucc (_, t1) ->
      let rec f n t =
        match t with
        | TmZero _ -> print_string (string_of_int n)
        | TmSucc (_, s) -> f (n + 1) s
        | _ ->
            print_string "(succ ";
            printtm_aterm false ctx t1;
            print_string ")"
      in
      f 1 t1
  | TmError _ -> print_string "error"
  | t ->
      print_string "(";
      printtm_term outer ctx t;
      print_string ")"

let printtm ctx t = printtm_term true ctx t

let prbinding ctx b =
  match b with
  | NameBind -> ()
  | TmAbbBind (t, _) ->
      print_string "= ";
      printtm ctx t
  | VarBind tyT ->
      print_string ": ";
      printty ctx tyT
  | TyVarBind -> ()
  | TyAbbBind tyT ->
      print_string "= ";
      printty ctx tyT
