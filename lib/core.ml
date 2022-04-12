open Syntax
open Support.Error

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
  | TmTag (_, _, t1, _) -> isval ctx t1
  | TmUnit _ -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs _ -> true
  | TmRecord (_, fields) -> List.for_all (fun (_, ti) -> isval ctx ti) fields
  | TmZero _ | TmSucc _ | TmAscribe _ | TmIf _ | TmCase _ | TmVar _
  | TmTimesFloat _ | TmLet _ | TmProj _ | TmApp _ | TmIsZero _ | TmInert _
  | TmFix _ | TmPred _ ->
      false

let rec eval1 ctx t =
  match t with
  | TmAscribe (_, v1, _) when isval ctx v1 -> v1
  | TmAscribe (fi, t1, tyT) ->
      let t1' = eval1 ctx t1 in
      TmAscribe (fi, t1', tyT)
  | TmIf (_, TmTrue _, t2, _) -> t2
  | TmIf (_, TmFalse _, _, t3) -> t3
  | TmIf (i, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (i, t1', t2, t3)
  | TmTag (fi, l, t1, tyT) ->
      let t1' = eval1 ctx t1 in
      TmTag (fi, l, t1', tyT)
  | TmCase (_, TmTag (_, li, v11, _), branches) when isval ctx v11 -> (
      try
        let _, body = List.assoc li branches in
        termsubsttop v11 body
      with Not_found -> raise NoRuleApplies)
  | TmCase (fi, t1, branches) ->
      let t1' = eval1 ctx t1 in
      TmCase (fi, t1', branches)
  | TmVar (fi, n, _) -> (
      match getbinding fi ctx n with
      | TmAbbBind (t, _) -> t
      | _ -> raise NoRuleApplies)
  | TmTimesFloat (i, TmFloat (_, f1), TmFloat (_, f2)) -> TmFloat (i, f1 *. f2)
  | TmTimesFloat (i, (TmFloat (_, _) as t1), t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesFloat (i, t1, t2')
  | TmTimesFloat (i, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesFloat (i, t1', t2)
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
  | TmApp (_, TmAbs (_, _, _, t12), v2) when isval ctx v2 -> termsubsttop v2 t12
  | TmApp (i, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (i, v1, t2')
  | TmApp (i, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (i, t1', t2)
  | TmFix (_, v1) as t when isval ctx v1 -> (
      match v1 with
      | TmAbs (_, _, _, t12) -> termsubsttop t t12
      | _ -> raise NoRuleApplies)
  | TmSucc (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc (i, t1')
  | TmPred (_, TmZero _) -> TmZero dummyinfo
  | TmPred (_, TmSucc (_, nv1)) when isnumericval ctx nv1 -> nv1
  | TmPred (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmPred (i, t1')
  | TmIsZero (_, TmZero _) -> TmTrue dummyinfo
  | TmIsZero (_, TmSucc (_, nv1)) when isnumericval ctx nv1 -> TmFalse dummyinfo
  | TmIsZero (i, t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero (i, t1')
  | TmString _ | TmTrue _ | TmFalse _ | TmUnit _ | TmFloat _ | TmAbs _ | TmFix _
  | TmZero _ | TmInert _ ->
      raise NoRuleApplies

let rec eval (ctx : context) (t : term) =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies -> t

let evalbinding (ctx : context) (b : binding) =
  match b with
  | TmAbbBind (t, tyT) ->
      let t' = eval ctx t in
      TmAbbBind (t', tyT)
  | TyAbbBind _ | VarBind _ | TyVarBind | NameBind -> b

let istyabb ctx i =
  match getbinding dummyinfo ctx i with TyAbbBind _ -> true | _ -> false

let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
  | TyAbbBind tyT -> tyT
  | _ -> raise NoRuleApplies

let computety ctx tyT =
  match tyT with
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | TyUnit, TyUnit -> true
  | TyId b1, TyId b2 -> b1 = b2
  | TyVariant fields1, TyVariant fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all2
           (fun (li1, tyTi1) (li2, tyTi2) -> li1 = li2 && tyeqv ctx tyTi1 tyTi2)
           fields1 fields2
  | TyString, TyString -> true
  | TyFloat, TyFloat -> true
  | TyArr (tyS1, tyS2), TyArr (tyT1, tyT2) ->
      tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
  | TyBool, TyBool -> true
  | TyNat, TyNat -> true
  | TyRecord fields1, TyRecord fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all
           (fun (li2, tyTi2) ->
             try
               let tyTi1 = List.assoc li2 fields1 in
               tyeqv ctx tyTi1 tyTi2
             with Not_found -> false)
           fields2
  | TyVar (i, _), _ when istyabb ctx i -> tyeqv ctx (gettyabb ctx i) tyT
  | _, TyVar (i, _) when istyabb ctx i -> tyeqv ctx tyS (gettyabb ctx i)
  | TyVar (i, _), TyVar (j, _) -> i = j
  | _ -> false

(* A transcription of the inversion lemma (9.3.1). *)
let rec typeof ctx t =
  match t with
  | TmAscribe (fi, t1, tyT) ->
      if tyeqv ctx (typeof ctx t1) tyT then tyT
      else error fi "body of as-term does not have the expected type"
  | TmString _ -> TyString
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if tyeqv ctx (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if tyeqv ctx tyT2 (typeof ctx t3) then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
  | TmCase (fi, t, cases) -> (
      match simplifyty ctx (typeof ctx t) with
      | TyVariant fieldtys ->
          List.iter
            (fun (li, (_, _)) ->
              try
                let _ = List.assoc li fieldtys in
                ()
              with Not_found -> error fi ("label " ^ li ^ " not in type"))
            cases;
          let casetypes =
            List.map
              (fun (li, (xi, ti)) ->
                let tyTi =
                  try List.assoc li fieldtys
                  with Not_found -> error fi ("label " ^ li ^ " not found")
                in
                let ctx' = addbinding ctx xi (VarBind tyTi) in
                typeshift (-1) (typeof ctx' ti))
              cases
          in
          let tyT1 = List.hd casetypes in
          let restTy = List.tl casetypes in
          List.iter
            (fun tyTi ->
              if not (tyeqv ctx tyTi tyT1) then
                error fi "fields do not have the same type")
            restTy;
          tyT1
      | _ -> error fi "expected variant type")
  | TmTag (fi, li, ti, tyT) -> (
      match simplifyty ctx tyT with
      | TyVariant fieldtys -> (
          try
            let tyTiExpected = List.assoc li fieldtys in
            let tyTi = typeof ctx ti in
            if tyeqv ctx tyTi tyTiExpected then tyT
            else error fi "field does not have expected type"
          with Not_found -> error fi ("label " ^ li ^ " not found"))
      | _ -> error fi "annotation is not a variant type")
  | TmUnit _ -> TyUnit
  | TmFloat _ -> TyFloat
  | TmTimesFloat (fi, t1, t2) ->
      if tyeqv ctx (typeof ctx t1) TyFloat && tyeqv ctx (typeof ctx t2) TyFloat
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmVar (fi, i, _) -> gettypefromcontext fi ctx i
  | TmLet (_, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind tyT1) in
      typeshift (-1) (typeof ctx' t2)
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
      TyRecord fieldtys
  | TmProj (fi, t1, l) -> (
      match simplifyty ctx (typeof ctx t1) with
      | TyRecord fieldtys -> (
          try List.assoc l fieldtys
          with Not_found -> error fi ("label " ^ l ^ " not found"))
      | _ -> error fi "expected record type")
  | TmInert (_, tyT) -> tyT
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, typeshift (-1) tyT2)
  | TmApp (fi, t1, t2) -> (
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match simplifyty ctx tyT1 with
      | TyArr (tyT11, tyT12) ->
          if tyeqv ctx tyT2 tyT11 then tyT12
          else error fi "parameter type mismatch"
      | TyVar _ -> tyT2
      | _ -> error fi "arrow type or type var expected")
  | TmFix (fi, t1) -> (
      let tyT1 = typeof ctx t1 in
      match simplifyty ctx tyT1 with
      | TyArr (tyT11, tyT12) ->
          if tyeqv ctx tyT12 tyT11 then tyT12
          else error fi "result of body not compatible with domain"
      | _ -> error fi "arrow type expected")
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
