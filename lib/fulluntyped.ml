open Syntax

exception NoRuleApplies

let rec isnumericval ctx t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t =
  match t with
  | TmString _ -> true
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs _ -> true
  | TmRecord (_, fields) -> List.for_all (fun (_, ti) -> isval ctx ti) fields
  | TmFloat _ -> true
  | _ -> false

let rec eval1 ctx t =
  match t with
  | TmVar (i, n, _) -> (
      match getbinding i ctx n with
      | TmAbbBind t -> t
      | _ -> raise NoRuleApplies)
  | TmIf (_, TmTrue _, t2, _) -> t2
  | TmIf (_, TmFalse _, _, t3) -> t3
  | TmIf (i, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (i, t1', t2, t3)
  | TmLet (_, _, v1, t2) when isval ctx v1 -> termsubsttop v1 t2
  | TmLet (i, x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLet (i, x, t1', t2)
  | TmRecord (i, fields) ->
      let rec evalfields l =
        match l with
        | [] -> raise NoRuleApplies
        | (l, vi) :: rest when isval ctx vi -> (l, vi) :: evalfields rest
        | (l, ti) :: rest ->
            let ti' = eval1 ctx ti in
            (l, ti') :: rest
      in
      TmRecord (i, evalfields fields)
  | TmProj (_, TmRecord (_, fields), l) -> (
      try List.assoc l fields with Not_found -> raise NoRuleApplies)
  | TmProj (i, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj (i, t1', l)
  | TmApp (_, TmAbs (_, _, t12), v2) when isval ctx v2 -> termsubsttop v2 t12
  | TmApp (i, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (i, v1, t2')
  | TmApp (i, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (i, t1', t2)
  | TmSucc (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc (i, t1')
  | TmPred (_, TmZero _) -> TmZero DUMMY
  | TmPred (_, TmSucc (_, nv1)) when isnumericval ctx nv1 -> nv1
  | TmPred (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmPred (i, t1')
  | TmIsZero (_, TmZero _) -> TmTrue DUMMY
  | TmIsZero (_, TmSucc (_, nv1)) when isnumericval ctx nv1 -> TmFalse DUMMY
  | TmIsZero (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero (i, t1')
  | TmTimesFloat (i, TmFloat (_, f1), TmFloat (_, f2)) -> TmFloat (i, f1 *. f2)
  | TmTimesFloat (i, (TmFloat (_, _) as t1), t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesFloat (i, t1, t2')
  | TmTimesFloat (i, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesFloat (i, t1', t2)
  | _ -> raise NoRuleApplies

let rec eval (ctx : context) (t : term) =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies -> t

let evalbinding (ctx : context) (b : binding) =
  match b with
  | TmAbbBind t ->
      let t' = eval ctx t in
      TmAbbBind t'
  | bind -> bind
