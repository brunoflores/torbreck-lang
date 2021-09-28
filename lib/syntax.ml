open Format
open Support.Error

type term =
  | TmString of info * string
  | TmVar of info * int * int
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmLet of info * string * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmAbs of info * string * term
  | TmApp of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmFloat of info * float
  | TmTimesFloat of info * term * term

type binding = NameBind | TmAbbBind of term

type command =
  | Import of string
  | Eval of info * term
  | Bind of info * string * binding

type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x, bind) :: ctx

let addname ctx x = addbinding ctx x NameBind

let obox0 () = open_hvbox 0

let obox () = open_hvbox 2

let cbox () = close_box ()

let break () = print_break 0 0

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

let small t = match t with TmVar _ -> true | _ -> false

let rec printtm_term outer ctx t =
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
  | TmAbs (_, x, t2) ->
      let ctx', x' = pickfreshname ctx x in
      obox ();
      print_string "lambda ";
      print_string x';
      print_string ".";
      if small t2 && not outer then break () else print_space ();
      printtm_term outer ctx' t2;
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

and printtm_pathterm outer ctx t =
  match t with
  | TmProj (_, t1, l) ->
      printtm_aterm false ctx t1;
      print_string ".";
      print_string l
  | t -> printtm_aterm outer ctx t

and printtm_aterm outer ctx t =
  match t with
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
      print_string "{";
      open_hovbox 0;
      p 1 fields;
      print_string "}";
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
  | t ->
      print_string "(";
      printtm_term outer ctx t;
      print_string ")"

let printtm ctx t = printtm_term true ctx t

let prbinding ctx b =
  match b with
  | NameBind -> ()
  | TmAbbBind t ->
      print_string " = ";
      printtm ctx t

let tmmap onvar c t =
  let rec walk c t =
    match t with
    | TmString _ as t -> t
    | TmVar (i, x, n) -> onvar i c x n
    | TmTrue _ as t -> t
    | TmFalse _ as t -> t
    | TmIf (i, t1, t2, t3) -> TmIf (i, walk c t1, walk c t2, walk c t3)
    | TmLet (i, x, t1, t2) -> TmLet (i, x, walk c t1, walk (c + 1) t2)
    | TmProj (i, t1, l) -> TmProj (i, walk c t1, l)
    | TmRecord (i, fields) ->
        let fieldswalked = List.map (fun (li, ti) -> (li, walk c ti)) fields in
        TmRecord (i, fieldswalked)
    | TmAbs (i, x, t2) -> TmAbs (i, x, walk (c + 1) t2)
    | TmApp (i, t1, t2) -> TmApp (i, walk c t1, walk c t2)
    | TmZero i -> TmZero i
    | TmSucc (i, t1) -> TmSucc (i, walk c t1)
    | TmPred (i, t1) -> TmPred (i, walk c t1)
    | TmIsZero (i, t1) -> TmIsZero (i, walk c t1)
    | TmFloat _ as t -> t
    | TmTimesFloat (i, t1, t2) -> TmTimesFloat (i, walk c t1, walk c t2)
  in
  walk c t

let termshiftabove d c t =
  tmmap
    (fun fi c x n ->
      if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    c t

let termshift d t = termshiftabove d 0 t

let bindingshift d bind =
  match bind with
  | NameBind -> NameBind
  | TmAbbBind t -> TmAbbBind (termshift d t)

let termsubst j s t =
  tmmap
    (fun fi c x n -> if x = j + c then termshift c s else TmVar (fi, x, n))
    0 t

let termsubsttop s t = termshift (-1) (termsubst 0 (termshift 1 s) t)

let getbinding _ ctx i =
  try
    let _, bind = List.nth ctx i in
    bindingshift (i + 1) bind
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size : %d"
    in
    failwith (msg i (List.length ctx))

let rec name_to_index fi ctx x =
  match ctx with
  | [] -> failwith (Printf.sprintf "Identifier %s is unbound" x)
  | (y, _) :: rest -> if y = x then 0 else 1 + name_to_index fi rest x

let tmInfo t =
  match t with
  | TmString (fi, _) -> fi
  | TmVar (fi, _, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmProj (fi, _, _) -> fi
  | TmRecord (fi, _) -> fi
  | TmAbs (fi, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmZero fi -> fi
  | TmSucc (fi, _) -> fi
  | TmPred (fi, _) -> fi
  | TmIsZero (fi, _) -> fi
  | TmFloat (fi, _) -> fi
  | TmTimesFloat (fi, _, _) -> fi
