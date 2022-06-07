(* Expansion of pattern-matching as a cascade of tests *)

open Lambda
open Clauses
open Error
open Syntax
open Const
open Globals
open Location
open Builtins

type pattern_matching = Matching of (pattern list * lambda) list * lambda list

(* Simple pattern manipulations *)

let make_path n = function
  | path :: pathl ->
      let rec make i =
        if i >= n then pathl else Lprim (Pfield i, [ path ]) :: make (i + 1)
      in
      make 0
  | _ -> failwith "make_path"

let add_to_match (Matching (casel, pathl)) cas = Matching (cas :: casel, pathl)

and make_constant_match pathl cas =
  match (pathl, cas) with
  | _path :: pathl, cas -> Matching ([ cas ], pathl)
  | _, _ -> failwith "make_constant_match"

and make_tuple_match arity pathl = Matching ([], make_path arity pathl)

and make_construct_match cstr pathl cas =
  match (cstr, pathl, cas) with
  | cstr, (path :: pathl as pathl0), cas -> begin
      match cstr.info.cs_kind with
      | Constr_constant -> Matching ([ cas ], pathl)
      | Constr_superfluous _n -> Matching ([ cas ], pathl0)
      | _ -> Matching ([ cas ], Lprim (Pfield 0, [ path ]) :: pathl)
    end
  | _, _, _ -> failwith "make_construct_match"

(* Auxiliaries for factoring common tests *)

let add_to_division make_match divlist key cas =
  try
    let matchref = List.assoc key divlist in
    matchref := add_to_match !matchref cas;
    divlist
  with Not_found -> (key, ref (make_match cas)) :: divlist

(* Skip type constraints and aliases, and flatten "or" patterns. *)

let rec simpl_casel = function
  | ({ p_desc = Zaliaspat (pat, _v); _ } :: patl, action) :: rest ->
      simpl_casel ((pat :: patl, action) :: rest)
  | ({ p_desc = Zconstraintpat (pat, _ty); _ } :: patl, action) :: rest ->
      simpl_casel ((pat :: patl, action) :: rest)
  | ({ p_desc = Zorpat (pat1, pat2); _ } :: patl, action) :: rest ->
      simpl_casel ((pat1 :: patl, action) :: (pat2 :: patl, action) :: rest)
  | casel -> casel

(* Factoring pattern-matchings. *)

let divide_constant_matching (Matching (casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
    | ({ p_desc = Zconstantpat cst; _ } :: patl, action) :: rest ->
        let constant, others = divide_rec rest in
        ( add_to_division (make_constant_match pathl) constant cst (patl, action),
          others )
    | casel -> ([], Matching (casel, pathl))
  in
  divide_rec casel

let wildcard_pat = { p_desc = Zwildpat; p_loc = no_location; p_typ = no_type }

let divide_tuple_matching arity (Matching (casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
    | ({ p_desc = Ztuplepat args; _ } :: patl, action) :: rest ->
        add_to_match (divide_rec rest) (args @ patl, action)
    | ({ p_desc = Zwildpat | Zvarpat _; _ } :: patl, action) :: rest ->
        let rec make_pats i =
          if i >= arity then [] else wildcard_pat :: make_pats (i + 1)
        in
        add_to_match (divide_rec rest) (make_pats 0 @ patl, action)
    | [] -> make_tuple_match arity pathl
    | _ -> failwith "divide_tuple_matching"
  in
  divide_rec casel

let divide_construct_matching (Matching (casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
    | ({ p_desc = Zconstruct0pat c; _ } :: patl, action) :: rest ->
        let constrs, others = divide_rec rest in
        ( add_to_division
            (make_construct_match c pathl)
            constrs c.info.cs_tag (patl, action),
          others )
    | ({ p_desc = Zconstruct1pat (c, arg); _ } :: patl, action) :: rest ->
        let patl' =
          match c.info.cs_kind with Constr_constant -> patl | _ -> arg :: patl
        in
        let constrs, others = divide_rec rest in
        ( add_to_division
            (make_construct_match c pathl)
            constrs c.info.cs_tag (patl', action),
          others )
    | casel -> ([], Matching (casel, pathl))
  in
  divide_rec casel

let divide_var_matching = function
  | Matching (casel, (_ :: endpathl as pathl)) ->
      let rec divide_rec casel =
        match simpl_casel casel with
        | ({ p_desc = Zwildpat | Zvarpat _; _ } :: patl, action) :: rest ->
            let vars, others = divide_rec rest in
            (add_to_match vars (patl, action), others)
        | casel -> (Matching ([], endpathl), Matching (casel, pathl))
      in
      divide_rec casel
  | _ -> failwith "divide_var_matching"

let divide_record_matching ty_record (Matching (casel, pathl)) =
  let labels = Types.labels_of_type ty_record in
  let num_labels = List.length labels in
  let rec divide_rec = function
    | ({ p_desc = Zaliaspat (pat, _v); _ } :: patl, action) :: rest ->
        divide_rec ((pat :: patl, action) :: rest)
    | ({ p_desc = Zconstraintpat (pat, _ty); _ } :: patl, action) :: rest ->
        divide_rec ((pat :: patl, action) :: rest)
    | ({ p_desc = Zorpat (pat1, pat2); _ } :: patl, action) :: rest ->
        divide_rec ((pat1 :: patl, action) :: (pat2 :: patl, action) :: rest)
    | ({ p_desc = Zrecordpat pat_expr_list; _ } :: patl, action) :: rest ->
        divide_rec_cont pat_expr_list patl action rest
    | ({ p_desc = Zwildpat | Zvarpat _; _ } :: patl, action) :: rest ->
        divide_rec_cont [] patl action rest
    | [] -> Matching ([], make_path num_labels pathl)
    | _ -> failwith "divide_record_matching"
  and divide_rec_cont pat_expr_list patl action rest =
    let v = Array.make num_labels wildcard_pat in
    List.iter (fun (lbl, pat) -> v.(lbl.info.lbl_pos) <- pat) pat_expr_list;
    add_to_match (divide_rec rest) (Array.to_list v @ patl, action)
  in
  divide_rec casel

(* Utilities on pattern-matchings *)

let length_of_matching (Matching (casel, _)) = List.length casel

let upper_left_pattern =
  let rec strip = function
    | { p_desc = Zaliaspat (pat, _); _ } -> strip pat
    | { p_desc = Zconstraintpat (pat, _); _ } -> strip pat
    | { p_desc = Zorpat (pat1, _pat2); _ } -> strip pat1
    | pat -> pat
  in
  function
  | Matching ((pat :: _, _) :: _, _) -> strip pat
  | _ -> failwith "upper_left_pattern"

let get_span_of_constr cstr =
  match cstr.info.cs_tag with
  | ConstrExtensible _ -> 0 (* Meaningless ... *)
  | ConstrRegular (_tag, span) -> span

let get_span_of_matching matching =
  match upper_left_pattern matching with
  | { p_desc = Zconstruct0pat c; _ } -> get_span_of_constr c
  | { p_desc = Zconstruct1pat (c, _); _ } -> get_span_of_constr c
  | _ -> failwith "get_span_of_matching"

(* The main compilation function.

   Input: a pattern-matching,
   Output: a lambda term and a "total" flag.

   The "total" flag is approximated: it is true if the matching is
   guaranteed to be total, and false otherwise. *)
let rec conquer_matching =
  let rec conquer_divided_matching = function
    | [] -> ([], true)
    | (key, matchref) :: rest ->
        let lambda1, total1 = conquer_matching !matchref in
        let list2, total2 = conquer_divided_matching rest in
        ((key, lambda1) :: list2, total1 && total2)
  in
  function
  | Matching ([], _) -> (Lstaticfail 0, false)
  | Matching (([], action) :: rest, pathl) ->
      if has_guard action then begin
        let lambda2, total2 = conquer_matching (Matching (rest, pathl)) in
        (Lstatichandle (action, lambda2), total2)
      end
      else (action, true)
  | Matching (_, path :: _) as matching -> begin
      match upper_left_pattern matching with
      | { p_desc = Zwildpat | Zvarpat _; _ } ->
          let vars, rest = divide_var_matching matching in
          let lambda1, total1 = conquer_matching vars in
          let lambda2, total2 = conquer_matching rest in
          if total1 then (lambda1, true)
          else (Lstatichandle (lambda1, lambda2), total2)
      | { p_desc = Ztuplepat patl; _ } ->
          conquer_matching (divide_tuple_matching (List.length patl) matching)
      | { p_desc = Zconstruct0pat _ | Zconstruct1pat _; _ } ->
          let constrs, vars = divide_construct_matching matching in
          let switchlst, total1 = conquer_divided_matching constrs in
          let lambda, total2 = conquer_matching vars in
          let span = get_span_of_matching matching in
          let num_cstr = List.length constrs in
          if num_cstr = span && total1 then
            (Lswitch (span, path, switchlst), true)
          else (Lstatichandle (Lswitch (span, path, switchlst), lambda), total2)
      | { p_desc = Zconstantpat _; _ } ->
          let constants, vars = divide_constant_matching matching in
          let condlist1, _ = conquer_divided_matching constants in
          let lambda2, total2 = conquer_matching vars in
          (Lstatichandle (Lcond (path, condlist1), lambda2), total2)
      | { p_desc = Zrecordpat _; p_typ = ty; _ } ->
          conquer_matching (divide_record_matching ty matching)
      | _ -> failwith "conquer_matching 2"
    end
  | _ -> failwith "conquer_matching 1"

(* Auxiliaries to build the initial matching *)

let make_initial_matching = function
  | [] -> failwith "make_initial_matching: empty"
  | (patl, _) :: _ as casel ->
      let rec make_path n =
        if n <= 0 then [] else Lvar (n - 1) :: make_path (n - 1)
      in
      Matching (casel, make_path (List.length patl))

let partial_fun (Loc (start, stop)) =
  Lprim
    ( Praise,
      [
        Lconst
          (SCblock
             ( match_failure_tag,
               [
                 SCatom (ACstring !input_name);
                 SCatom (ACint start);
                 SCatom (ACint stop);
               ] ));
      ] )

(* The entry points *)
let translate_matching_check_failure loc casel =
  let casel' =
    List.map
      (fun (patl, action) -> (patl, share_lambda action))
      (check_unused casel)
  in
  if partial_match casel then not_exhaustive_warning loc;
  let lambda, total = conquer_matching (make_initial_matching casel') in
  if total then lambda else Lstatichandle (lambda, partial_fun loc)
