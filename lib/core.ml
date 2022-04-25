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
  | TmLoc _ -> true
  | TmString _ -> true
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmTag (_, _, t1, _) -> isval ctx t1
  | TmUnit _ -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
  | TmAbs _ -> true
  | TmRecord (_, fields) -> List.for_all (fun (_, ti) -> isval ctx ti) fields
  (* Else *)
  | TmZero _ | TmSucc _ | TmAscribe _ | TmIf _ | TmCase _ | TmVar _
  | TmTimesFloat _ | TmLet _ | TmProj _ | TmApp _ | TmIsZero _ | TmInert _
  | TmTry _ | TmError _ | TmAssign _ | TmRef _ | TmDeref _ | TmFix _ | TmPred _
    ->
      false

type store = term list

let emptystore = []
let extendstore store v = (List.length store, List.append store [ v ])
let lookuploc store l = List.nth store l

let updatestore store n v =
  let rec f s =
    match s with
    | 0, _ :: rest -> (* Replace _ with v *) v :: rest
    | n, v' :: rest -> (* Recurse *) v' :: f (n - 1, rest)
    | _ -> error dummyinfo "updatestore: bad index"
  in
  f (n, store)

let shiftstore i store = List.map (fun t -> termshift i t) store

let rec eval1 ctx store t =
  match t with
  | TmRef (fi, t1) ->
      if not (isval ctx t1) then
        let t1', store' = eval1 ctx store t1 in
        (TmRef (fi, t1'), store')
      else
        let l, store' = extendstore store t1 in
        (TmLoc (dummyinfo, l), store')
  | TmDeref (fi, t1) -> (
      if not (isval ctx t1) then
        let t1', store' = eval1 ctx store t1 in
        (TmDeref (fi, t1'), store')
      else
        match t1 with
        | TmLoc (_, l) -> (lookuploc store l, store)
        | _ -> raise NoRuleApplies)
  | TmAssign (fi, t1, t2) -> (
      if not (isval ctx t1) then
        let t1', store' = eval1 ctx store t1 in
        (TmAssign (fi, t1', t2), store')
      else if not (isval ctx t2) then
        let t2', store' = eval1 ctx store t2 in
        (TmAssign (fi, t1, t2'), store')
      else
        match t1 with
        | TmLoc (_, l) -> (TmUnit dummyinfo, updatestore store l t2)
        | _ -> raise NoRuleApplies)
  (* TmAscribe *)
  | TmAscribe (_, v1, _) when isval ctx v1 -> (v1, store)
  | TmAscribe (fi, t1, tyT) ->
      let t1', store' = eval1 ctx store t1 in
      (TmAscribe (fi, t1', tyT), store')
  (* TmIf *)
  | TmIf (_, TmError fi, _, _) -> (TmError fi, store)
  | TmIf (_, TmTrue _, t2, _) -> (t2, store)
  | TmIf (_, TmFalse _, _, t3) -> (t3, store)
  | TmIf (fi, t1, t2, t3) ->
      let t1', store' = eval1 ctx store t1 in
      (TmIf (fi, t1', t2, t3), store')
  | TmTag (fi, l, t1, tyT) ->
      let t1', store' = eval1 ctx store t1 in
      (TmTag (fi, l, t1', tyT), store')
  (* TmCase *)
  | TmCase (_, TmTag (_, li, v11, _), branches) when isval ctx v11 -> (
      try
        let _, body = List.assoc li branches in
        (termsubsttop v11 body, store)
      with Not_found -> raise NoRuleApplies)
  | TmCase (fi, t1, branches) ->
      let t1', store' = eval1 ctx store t1 in
      (TmCase (fi, t1', branches), store')
  | TmVar (fi, n, _) -> (
      match getbinding fi ctx n with
      | TmAbbBind (t, _) -> (t, store)
      | _ -> raise NoRuleApplies)
  (* TmTimesFloat *)
  | TmTimesFloat (fi, TmFloat (_, f1), TmFloat (_, f2)) ->
      (TmFloat (fi, f1 *. f2), store)
  | TmTimesFloat (fi, (TmFloat (_, _) as t1), t2) ->
      let t2', store' = eval1 ctx store t2 in
      (TmTimesFloat (fi, t1, t2'), store')
  | TmTimesFloat (fi, t1, t2) ->
      let t1', store' = eval1 ctx store t1 in
      (TmTimesFloat (fi, t1', t2), store')
  (* TmLet *)
  | TmLet (_, _, v1, t2) when isval ctx v1 -> (termsubsttop v1 t2, store)
  | TmLet (fi, x, t1, t2) ->
      let t1', store' = eval1 ctx store t1 in
      (TmLet (fi, x, t1', t2), store')
  | TmRecord (fi, fields) ->
      let rec evalfields l =
        match l with
        | [] -> raise NoRuleApplies
        | (l, vi) :: rest when isval ctx vi ->
            let rest', store' = evalfields rest in
            ((l, vi) :: rest', store')
        | (l, ti) :: rest ->
            let ti', store' = eval1 ctx store ti in
            ((l, ti') :: rest, store')
      in
      let fields', store' = evalfields fields in
      (TmRecord (fi, fields'), store')
  (* TmProj *)
  | TmProj (_, TmRecord (_, fields), l) -> (
      try (List.assoc l fields, store) with Not_found -> raise NoRuleApplies)
  | TmProj (fi, t1, l) ->
      let t1', store' = eval1 ctx store t1 in
      (TmProj (fi, t1', l), store')
  (* TmApp *)
  | TmApp (_, TmError fi, _) -> (TmError fi, store)
  | TmApp (_, v1, TmError fi) when isval ctx v1 -> (TmError fi, store)
  | TmApp (_, TmAbs (_, _, _, t12), v2) when isval ctx v2 ->
      (termsubsttop v2 t12, store)
  | TmApp (i, v1, t2) when isval ctx v1 ->
      let t2', store' = eval1 ctx store t2 in
      (TmApp (i, v1, t2'), store')
  | TmApp (i, t1, t2) ->
      let t1', store' = eval1 ctx store t1 in
      (TmApp (i, t1', t2), store')
  | TmFix (_, v1) as t when isval ctx v1 -> (
      match v1 with
      | TmAbs (_, _, _, t12) -> (termsubsttop t t12, store)
      | _ -> raise NoRuleApplies)
  | TmSucc (i, t1) ->
      let t1', store' = eval1 ctx store t1 in
      (TmSucc (i, t1'), store')
  (* TmPred *)
  | TmPred (_, TmZero _) -> (TmZero dummyinfo, store)
  | TmPred (_, TmSucc (_, nv1)) when isnumericval ctx nv1 -> (nv1, store)
  | TmPred (i, t1) ->
      let t1', store' = eval1 ctx store t1 in
      (TmPred (i, t1'), store')
  (* TmIsZero *)
  | TmIsZero (_, TmZero _) -> (TmTrue dummyinfo, store)
  | TmIsZero (_, TmSucc (_, nv1)) when isnumericval ctx nv1 ->
      (TmFalse dummyinfo, store)
  | TmIsZero (i, t1) ->
      let t1', store' = eval1 ctx store t1 in
      (TmIsZero (i, t1'), store')
  (* Else *)
  | TmError _ | TmTry _ | TmString _ | TmTrue _ | TmFalse _ | TmUnit _
  | TmFloat _ | TmAbs _ | TmFix _ | TmLoc _ | TmZero _ | TmInert _ ->
      raise NoRuleApplies

let rec eval (ctx : context) (store : store) (t : term) =
  try
    let t', store' = eval1 ctx store t in
    eval ctx store' t'
  with NoRuleApplies -> (t, store)

let evalbinding (ctx : context) (store : store) (b : binding) =
  match b with
  | TmAbbBind (t, tyT) ->
      let t', store' = eval ctx store t in
      (TmAbbBind (t', tyT), store')
  | TyAbbBind _ | VarBind _ | TyVarBind | NameBind -> (b, store)

let istyabb ctx i =
  match getbinding dummyinfo ctx i with TyAbbBind _ -> true | _ -> false

let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
  | TyAbbBind tyT -> tyT
  | _ -> raise NoRuleApplies

let computety ctx tyT =
  match tyT with
  | TyRec (_, tyS1) as tyS -> typesubsttop tyS tyS1
  | TyVar (i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let rec tyeqv seen ctx tyS tyT =
  List.mem (tyS, tyT) seen
  ||
  match (tyS, tyT) with
  | TyRec (_, tyS1), _ ->
      tyeqv ((tyS, tyT) :: seen) ctx (typesubsttop tyS tyS1) tyT
  | _, TyRec (_, tyT1) ->
      tyeqv ((tyS, tyT) :: seen) ctx tyS (typesubsttop tyT tyT1)
  | TyUnit, TyUnit -> true
  | TyBot, TyBot -> true
  | TyId b1, TyId b2 -> b1 = b2
  | TyVariant fields1, TyVariant fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all2
           (fun (li1, tyTi1) (li2, tyTi2) ->
             li1 = li2 && tyeqv seen ctx tyTi1 tyTi2)
           fields1 fields2
  | TyString, TyString -> true
  | TyFloat, TyFloat -> true
  | TyArr (tyS1, tyS2), TyArr (tyT1, tyT2) ->
      tyeqv seen ctx tyS1 tyT1 && tyeqv seen ctx tyS2 tyT2
  | TyBool, TyBool -> true
  | TyNat, TyNat -> true
  | TyRecord fields1, TyRecord fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all
           (fun (li2, tyTi2) ->
             try
               let tyTi1 = List.assoc li2 fields1 in
               tyeqv seen ctx tyTi1 tyTi2
             with Not_found -> false)
           fields2
  | TyVar (i, _), _ when istyabb ctx i -> tyeqv seen ctx (gettyabb ctx i) tyT
  | _, TyVar (i, _) when istyabb ctx i -> tyeqv seen ctx tyS (gettyabb ctx i)
  | TyVar (i, _), TyVar (j, _) -> i = j
  | _ -> false

let tyeqv ctx tyS tyT = tyeqv [] ctx tyS tyT

exception Not_subtype of info * string

let rec subtype ctx tyS tyT =
  tyeqv ctx tyS tyT
  ||
  (* The [||] operator is a _short-circuiting_ boolean "or": optimization.
     The heuristic is that most times when the subtype checker is called,
     the two types being compared are actually equal. *)
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | TyBot, _ -> true
  | _, TyTop -> true
  | TyArr (tyS1, tyS2), TyArr (tyT1, tyT2) ->
      subtype ctx tyT1 tyS1 && subtype ctx tyS2 tyT2
  | TyRef tyT1, TyRef tyT2 -> subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1
  | TyRef tyT1, TySource tyT2 -> subtype ctx tyT1 tyT2
  | TySource tyT1, TySource tyT2 -> subtype ctx tyT1 tyT2
  | TyRef tyT1, TySink tyT2 -> subtype ctx tyT2 tyT1
  | TySink tyT1, TySink tyT2 -> subtype ctx tyT2 tyT1
  | TyVariant fS, TyVariant fT ->
      List.for_all
        (fun (li, tySi) ->
          try
            let tyTi = List.assoc li fT in
            subtype ctx tySi tyTi
          with Not_found -> raise (Not_subtype (dummyinfo, "")))
        fS
  | TyRecord fS, TyRecord fT ->
      let field_is_subtype (li, tyTi) =
        try
          let tySi = List.assoc li fS in
          subtype ctx tySi tyTi
        with Not_found ->
          let msg = Printf.sprintf "field %s not in record provided here" li in
          raise (Not_subtype (dummyinfo, msg))
      in
      List.for_all field_is_subtype fT
  (* Else, handle remaining errors here *)
  | _ -> raise (Not_subtype (dummyinfo, ""))

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT
  else if subtype ctx tyT tyS then tyS
  else
    let tyS = simplifyty ctx tyS in
    let tyT = simplifyty ctx tyT in
    match (tyS, tyT) with
    | TyArr (tyS1, tyS2), TyArr (tyT1, tyT2) -> (
        try TyArr (meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
        with Not_found -> TyTop)
    | TyRef tyT1, TyRef tyT2 ->
        if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 then TyRef tyT1
        else (* Warning: incomplete *) TySource (join ctx tyT1 tyT2)
    | TySource tyT1, TySource tyT2 -> TySource (join ctx tyT1 tyT2)
    | TyRef tyT1, TySource tyT2 -> TySource (join ctx tyT1 tyT2)
    | TySource tyT1, TyRef tyT2 -> TySource (join ctx tyT1 tyT2)
    | TySink tyT1, TySink tyT2 -> TySink (meet ctx tyT1 tyT2)
    | TyRef tyT1, TySink tyT2 -> TySink (meet ctx tyT1 tyT2)
    | TySink tyT1, TyRef tyT2 -> TySink (meet ctx tyT1 tyT2)
    | TyRecord fS, TyRecord fT ->
        let labelsS = List.map (fun (li, _) -> li) fS in
        let labelsT = List.map (fun (li, _) -> li) fT in
        let commonLabels =
          List.find_all (fun l -> List.mem l labelsT) labelsS
        in
        let commonFields =
          List.map
            (fun li ->
              let tySi = List.assoc li fS in
              let tyTi = List.assoc li fT in
              (li, join ctx tySi tyTi))
            commonLabels
        in
        TyRecord commonFields
    | _ -> TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS
  else if subtype ctx tyT tyS then tyT
  else
    let tyS = simplifyty ctx tyS in
    let tyT = simplifyty ctx tyT in
    match (tyS, tyT) with
    | TyArr (tyS1, tyS2), TyArr (tyT1, tyT2) ->
        TyArr (join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
    | TyRef tyT1, TyRef tyT2 ->
        if subtype ctx tyT1 tyT2 && subtype ctx tyT1 tyT1 then TyRef tyT1
        else (* Warning: incomplete *) TySource (meet ctx tyT1 tyT2)
    | TySource tyT1, TySource tyT2 -> TySource (meet ctx tyT1 tyT2)
    | TyRef tyT1, TySource tyT2 -> TySource (meet ctx tyT1 tyT2)
    | TySource tyT1, TyRef tyT2 -> TySource (meet ctx tyT1 tyT2)
    | TySink tyT1, TySink tyT2 -> TySink (join ctx tyT1 tyT2)
    | TyRef tyT1, TySink tyT2 -> TySink (join ctx tyT1 tyT2)
    | TySink tyT1, TyRef tyT2 -> TySink (join ctx tyT1 tyT2)
    | TyRecord fS, TyRecord fT ->
        let labelsS = List.map (fun (li, _) -> li) fS in
        let labelsT = List.map (fun (li, _) -> li) fT in
        let allLabels =
          List.append labelsS
            (List.find_all (fun l -> not (List.mem l labelsS)) labelsT)
        in
        let allFields =
          List.map
            (fun li ->
              if List.mem li allLabels then
                let tySi = List.assoc li fS in
                let tyTi = List.assoc li fT in
                (li, meet ctx tySi tyTi)
              else if List.mem li labelsS then (li, List.assoc li fS)
              else (li, List.assoc li fT))
            allLabels
        in
        TyRecord allFields
    | _ -> TyBot

(* A transcription of the inversion lemma (9.3.1). *)
let rec typeof ctx t =
  match t with
  | TmDeref (fi, t1) -> (
      match simplifyty ctx (typeof ctx t1) with
      | TyRef tyT1 -> tyT1
      | TyBot -> TyBot
      | TySource tyT1 -> tyT1
      | _ -> error fi "argument of ! is not a Ref or Source")
  | TmAssign (fi, t1, t2) -> (
      match simplifyty ctx (typeof ctx t1) with
      | TyRef tyT1 ->
          if subtype ctx (typeof ctx t2) tyT1 then TyUnit
          else error fi "arguments of := are incompatible"
      | TyBot -> TyBot
      | TySink tyT1 ->
          if subtype ctx (typeof ctx t2) tyT1 then TyUnit
          else error fi "arguments of := are incompatible"
      | _ -> error fi "argument of ! is not a Ref or Sink")
  | TmRef (_, t1) -> TyRef (typeof ctx t1)
  | TmLoc (fi, _) ->
      error fi "locations are not supposed to occur in source programs"
  | TmAscribe (fi, t1, tyT) ->
      let sub =
        try subtype ctx (typeof ctx t1) tyT
        with Not_subtype (_, msg) -> raise (Not_subtype (fi, msg))
      in
      if sub then tyT
      else error fi "body of as-term does not have the expected type"
  | TmString _ -> TyString
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if subtype ctx (typeof ctx t1) TyBool then
        join ctx (typeof ctx t2) (typeof ctx t3)
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
                  with Not_found ->
                    error fi @@ Format.sprintf "label %s not found" li
                in
                let ctx' = addbinding ctx xi (VarBind tyTi) in
                typeshift (-1) (typeof ctx' ti))
              cases
          in
          List.fold_left (join ctx) TyBot casetypes
      | TyBot -> TyBot
      | _ -> error fi "expected variant type")
  | TmTag (fi, li, ti, tyT) -> (
      match simplifyty ctx tyT with
      | TyVariant fieldtys -> (
          try
            let tyTiExpected = List.assoc li fieldtys in
            let tyTi = typeof ctx ti in
            if subtype ctx tyTi tyTiExpected then tyT
            else error fi "field does not have expected type"
          with Not_found -> error fi @@ Format.sprintf "label %s not found" li)
      | _ -> error fi "annotation is not a variant type")
  | TmUnit _ -> TyUnit
  | TmFloat _ -> TyFloat
  | TmTimesFloat (fi, t1, t2) ->
      if
        subtype ctx (typeof ctx t1) TyFloat
        && subtype ctx (typeof ctx t2) TyFloat
      then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmVar (fi, i, _) -> gettypefromcontext fi ctx i
  | TmLet (fi, x, t1, t2) -> (
      let tyT1 =
        try typeof ctx t1
        with Not_subtype (fi', msg) -> error ?context:(Some fi) fi' msg
      in
      let ctx' = addbinding ctx x (VarBind tyT1) in
      try typeshift (-1) (typeof ctx' t2)
      with Not_subtype (fi', msg) -> error fi' msg ?context:(Some fi))
  | TmRecord (_, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
      TyRecord fieldtys
  | TmProj (fi, t1, l) -> (
      match simplifyty ctx (typeof ctx t1) with
      | TyRecord fieldtys -> (
          try List.assoc l fieldtys
          with Not_found -> error fi @@ Format.sprintf "label %s not found" l)
      | TyBot -> TyBot
      | TyVar (i, _) ->
          error fi
          @@ Format.sprintf "expected record type, got: %s"
          @@ Syntax.index_to_name fi ctx i
      | _ as ty ->
          error fi
          @@ Format.sprintf "expected record type, got: %s"
          @@ Syntax.show_ty ty)
  | TmInert (_, tyT) -> tyT
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, typeshift (-1) tyT2)
  | TmApp (fi, t1, t2) -> (
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match simplifyty ctx tyT1 with
      | TyArr (tyT11, tyT12) -> (
          try
            if subtype ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
          with Not_subtype (_, msg) -> error fi msg)
      | TyBot -> TyBot
      | _ as ty ->
          error fi
          @@ Format.sprintf "expected arrow type, got: %s\nwith context:\n%s"
               (Syntax.show_ty ty)
               (Syntax.string_of_context ctx))
  | TmFix (fi, t1) -> (
      let tyT1 = typeof ctx t1 in
      match simplifyty ctx tyT1 with
      | TyArr (tyT11, tyT12) ->
          if subtype ctx tyT12 tyT11 then tyT12
          else error fi "result of body not compatible with domain"
      | TyBot -> TyBot
      | _ -> error fi "arrow type expected")
  | TmZero _ -> TyNat
  | TmSucc (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero (fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmError _ -> TyBot
  | TmTry (_, t1, t2) -> join ctx (typeof ctx t1) (typeof ctx t2)
