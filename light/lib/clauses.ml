(* Detection of unused match clauses and incomplete matchings *)

open Error
open Syntax
open Location
open Globals
open Types

let make_pat desc ty = { p_desc = desc; p_loc = no_location; p_typ = ty }
let omega = make_pat Zwildpat no_type
let rec omegas i = if i <= 0 then [] else omega :: omegas (i - 1)

let simple_match p1 p2 =
  match (p1.p_desc, p2.p_desc) with
  | Zconstruct0pat c1, Zconstruct0pat c2 -> c1.info.cs_tag = c2.info.cs_tag
  | Zconstruct1pat (c1, _), Zconstruct1pat (c2, _) ->
      c1.info.cs_tag = c2.info.cs_tag
  | Zconstantpat c1, Zconstantpat c2 -> c1 = c2
  | Ztuplepat _, Ztuplepat _ -> true
  | Zrecordpat _, Zrecordpat _ -> true
  | _, (Zwildpat | Zvarpat _) -> true
  | _, _ -> false

let record_labels p = labels_of_type p.p_typ
let record_nargs p = List.length (record_labels p)

let set_fields size l =
  let arr = Array.make size omega in
  let rec change_rec l =
    match l with
    | (lbl, p) :: l ->
        arr.(lbl.info.lbl_pos) <- p;
        change_rec l
    | [] -> ()
  in
  change_rec l;
  Array.to_list arr

let simple_match_args p1 p2 =
  match p2.p_desc with
  | Zconstruct1pat (_, arg) -> [ arg ]
  | Ztuplepat args -> args
  | Zrecordpat args -> set_fields (record_nargs p1) args
  | Zwildpat | Zvarpat _ -> begin
      match p1.p_desc with
      | Zconstruct1pat (_, _) -> [ omega ]
      | Ztuplepat args -> List.map (fun _ -> omega) args
      | Zrecordpat args -> List.map (fun _ -> omega) args
      | _ -> []
    end
  | _ -> []

(* Check the discriminating pattern for a matching by the first
   column of pss *)

let rec simple_pat q pss =
  match pss with
  | ({ p_desc = Zaliaspat (p, _); _ } :: ps) :: pss ->
      simple_pat q ((p :: ps) :: pss)
  | ({ p_desc = Zconstraintpat (p, _); _ } :: ps) :: pss ->
      simple_pat q ((p :: ps) :: pss)
  | ({ p_desc = Zorpat (p1, p2); _ } :: ps) :: pss ->
      simple_pat q ((p1 :: ps) :: (p2 :: ps) :: pss)
  | ({ p_desc = Zwildpat | Zvarpat _; _ } :: _) :: pss -> simple_pat q pss
  | (({ p_desc = Ztuplepat args; _ } as p) :: _) :: _ ->
      make_pat (Ztuplepat (List.map (fun _ -> omega) args)) p.p_typ
  | (({ p_desc = Zrecordpat _; _ } as p) :: _) :: _ ->
      make_pat
        (Zrecordpat (List.map (fun lbl -> (lbl, omega)) (record_labels p)))
        p.p_typ
  | _ -> q

let filter_one q pss =
  let rec filter_rec pss =
    match pss with
    | ({ p_desc = Zaliaspat (p, _); _ } :: ps) :: pss ->
        filter_rec ((p :: ps) :: pss)
    | ({ p_desc = Zconstraintpat (p, _); _ } :: ps) :: pss ->
        filter_rec ((p :: ps) :: pss)
    | ({ p_desc = Zorpat (p1, p2); _ } :: ps) :: pss ->
        filter_rec ((p1 :: ps) :: (p2 :: ps) :: pss)
    | (p :: ps) :: pss ->
        if simple_match q p then (simple_match_args q p @ ps) :: filter_rec pss
        else filter_rec pss
    | _ -> []
  in
  filter_rec pss

let filter_extra pss =
  let rec filter_rec pss =
    match pss with
    | ({ p_desc = Zaliaspat (p, _); _ } :: ps) :: pss ->
        filter_rec ((p :: ps) :: pss)
    | ({ p_desc = Zconstraintpat (p, _); _ } :: ps) :: pss ->
        filter_rec ((p :: ps) :: pss)
    | ({ p_desc = Zorpat (p1, p2); _ } :: ps) :: pss ->
        filter_rec ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({ p_desc = Zwildpat | Zvarpat _; _ } :: qs) :: pss ->
        qs :: filter_rec pss
    | _ :: pss -> filter_rec pss
    | [] -> []
  in
  filter_rec pss

let filter_all pat0 pss =
  let rec insert q qs env =
    match env with
    | [] -> [ (q, [ simple_match_args q q @ qs ]) ]
    | ((p, pss) as c) :: env ->
        if simple_match q p then (p, (simple_match_args p q @ qs) :: pss) :: env
        else c :: insert q qs env
  in
  let rec filter_rec env pss =
    match pss with
    | ({ p_desc = Zaliaspat (p, _); _ } :: ps) :: pss ->
        filter_rec env ((p :: ps) :: pss)
    | ({ p_desc = Zconstraintpat (p, _); _ } :: ps) :: pss ->
        filter_rec env ((p :: ps) :: pss)
    | ({ p_desc = Zorpat (p1, p2); _ } :: ps) :: pss ->
        filter_rec env ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({ p_desc = Zwildpat | Zvarpat _; _ } :: _) :: pss -> filter_rec env pss
    | (p :: ps) :: pss -> filter_rec (insert p ps env) pss
    | _ -> env
  and filter_omega env pss =
    match pss with
    | ({ p_desc = Zaliaspat (p, _); _ } :: ps) :: pss ->
        filter_omega env ((p :: ps) :: pss)
    | ({ p_desc = Zconstraintpat (p, _); _ } :: ps) :: pss ->
        filter_omega env ((p :: ps) :: pss)
    | ({ p_desc = Zorpat (p1, p2); _ } :: ps) :: pss ->
        filter_omega env ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({ p_desc = Zwildpat | Zvarpat _; _ } :: ps) :: pss ->
        filter_omega
          (List.map
             (fun (q, qss) -> (q, (simple_match_args q omega @ ps) :: qss))
             env)
          pss
    | _ :: pss -> filter_omega env pss
    | [] -> env
  in
  filter_omega
    (filter_rec
       (match pat0.p_desc with
       | Zrecordpat _ | Ztuplepat _ -> [ (pat0, []) ]
       | _ -> [])
       pss)
    pss

let get_span_of_constr cstr =
  match cstr.info.cs_tag with
  | ConstrExtensible _ -> 0 (* Meaningless ... *)
  | ConstrRegular (_, span) -> span

let full_match env =
  match env with
  | ({ p_desc = Zconstruct0pat c; _ }, _) :: _ ->
      List.length env == get_span_of_constr c
  | ({ p_desc = Zconstruct1pat (c, _); _ }, _) :: _ ->
      List.length env = get_span_of_constr c
  | ({ p_desc = Zconstantpat (ACchar _); _ }, _) :: _ -> List.length env == 256
  | ({ p_desc = Zconstantpat _; _ }, _) :: _ -> false
  | ({ p_desc = Ztuplepat _; _ }, _) :: _ -> true
  | ({ p_desc = Zrecordpat _; _ }, _) :: _ -> true
  | _ -> failwith "full_match"

let rec satisfiable pss qs =
  match pss with
  | [] -> true
  | _ -> begin
      match qs with
      | [] -> false
      | { p_desc = Zorpat (q1, q2); _ } :: qs ->
          satisfiable pss (q1 :: qs) || satisfiable pss (q2 :: qs)
      | { p_desc = Zaliaspat (q, _); _ } :: qs -> satisfiable pss (q :: qs)
      | { p_desc = Zconstraintpat (q, _); _ } :: qs -> satisfiable pss (q :: qs)
      | { p_desc = Zwildpat | Zvarpat _; _ } :: qs ->
          let q0 = simple_pat omega pss in
          begin
            match filter_all q0 pss with
            | [] -> satisfiable (filter_extra pss) qs
            | constrs ->
                let try_non_omega (p, pss) =
                  satisfiable pss (simple_match_args p omega @ qs)
                in
                if full_match constrs then List.exists try_non_omega constrs
                else
                  satisfiable (filter_extra pss) qs
                  || List.exists try_non_omega constrs
          end
      | q :: qs ->
          let q0 = simple_pat q pss in
          satisfiable (filter_one q0 pss) (simple_match_args q0 q @ qs)
    end

let rec make_matrix pses =
  match pses with
  | [] -> []
  | (ps, action) :: pses ->
      if Lambda.has_guard action then make_matrix pses
      else ps :: make_matrix pses

let rec le_pat p q =
  match (p.p_desc, q.p_desc) with
  | (Zvarpat _ | Zwildpat), _ -> true
  | Zaliaspat (p, _), _ -> le_pat p q
  | _, Zaliaspat (q, _) -> le_pat p q
  | Zconstraintpat (p, _), _ -> le_pat p q
  | _, Zconstraintpat (q, _) -> le_pat p q
  | Zorpat (p1, p2), _ -> le_pat p1 q || le_pat p2 q
  | _, Zorpat (q1, q2) -> le_pat p q1 || le_pat p q2
  | Zconstantpat c1, Zconstantpat c2 -> c1 = c2
  | Zconstruct0pat c1, Zconstruct0pat c2 -> c1.info.cs_tag == c2.info.cs_tag
  | Zconstruct1pat (c1, p), Zconstruct1pat (c2, q) ->
      c1.info.cs_tag == c2.info.cs_tag && le_pat p q
  | Ztuplepat ps, Ztuplepat qs -> le_pats ps qs
  | Zrecordpat l1, Zrecordpat l2 ->
      let size = record_nargs p in
      le_pats (set_fields size l1) (set_fields size l2)
  | ( Zconstantpat _,
      ( Zwildpat | Zvarpat _ | Zconstruct0pat _
      | Zconstruct1pat (_, _)
      | Ztuplepat _ | Zrecordpat _ ) )
  | ( Zconstruct0pat _,
      ( Zwildpat | Zvarpat _
      | Zconstruct1pat (_, _)
      | Zconstantpat _ | Ztuplepat _ | Zrecordpat _ ) )
  | ( Zconstruct1pat (_, _),
      ( Zwildpat | Zvarpat _ | Zconstruct0pat _ | Zconstantpat _ | Ztuplepat _
      | Zrecordpat _ ) )
  | ( Ztuplepat _,
      ( Zwildpat | Zvarpat _ | Zconstruct0pat _
      | Zconstruct1pat (_, _)
      | Zconstantpat _ | Zrecordpat _ ) )
  | ( Zrecordpat _,
      ( Zwildpat | Zvarpat _ | Zconstruct0pat _
      | Zconstruct1pat (_, _)
      | Zconstantpat _ | Ztuplepat _ ) ) ->
      false

and le_pats ps qs =
  match (ps, qs) with
  | p :: ps, q :: qs -> le_pat p q && le_pats ps qs
  | _ -> true

let get_mins ps =
  let rec select_rec r ps =
    match ps with
    | [] -> r
    | p :: ps ->
        if List.exists (fun p0 -> le_pats p0 p) ps then select_rec r ps
        else select_rec (p :: r) ps
  in
  select_rec [] (select_rec [] ps)

let partial_match casel =
  let pss = get_mins (make_matrix casel) in
  match pss with
  | [] -> true
  | ps :: _ -> satisfiable pss (List.map (fun _ -> omega) ps)

let extract_loc_from_clause = function
  | pat :: _ -> pat.p_loc
  | _ -> failwith "extract_loc_from_clause"

let check_unused casel =
  let prefs =
    List.fold_right
      (fun ((ps, action) as clause) r ->
        if Lambda.has_guard action then ([], clause) :: r
        else
          ([], clause) :: List.map (fun (pss, clause) -> (ps :: pss, clause)) r)
      casel []
  in
  let rec check_rec l =
    match l with
    | (pss, ((qs, _) as clause)) :: l ->
        if satisfiable pss qs then clause :: check_rec l
        else begin
          unused_cases_warning (extract_loc_from_clause qs);
          check_rec l
        end
    | [] -> []
  in
  check_rec prefs
