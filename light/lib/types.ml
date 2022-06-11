(* Basic operations over types *)

open Globals
open Modules

(* Type constructor equality *)
let same_type_constr cstr1 cstr2 =
  cstr1.info.ty_stamp == cstr2.info.ty_stamp
  && cstr1.qualid.qual = cstr2.qualid.qual

(* To take the canonical representative of a type *)
let rec type_repr ty =
  match ty.typ_desc with
  | Tvar r -> begin
      match r.link with
      | Tnolink -> ty
      | Tlinkto t1 -> begin
          let t2 = type_repr t1 in
          r.link <- Tlinkto t2;
          t2
        end
    end
  | _ -> ty

(* The current nesting level of lets *)

let current_level = ref 0

let reset_type_var () = current_level := 0
and push_type_level () = incr current_level
and pop_type_level () = decr current_level

(* To get fresh type variables *)

let new_type_var () =
  { typ_desc = Tvar { link = Tnolink }; typ_level = !current_level }

let rec type_var_list n level =
  if n <= 0 then []
  else
    { typ_desc = Tvar { link = Tnolink }; typ_level = level }
    :: type_var_list (pred n) level

let new_type_var_list n = type_var_list n !current_level

let new_global_type_var () =
  { typ_desc = Tvar { link = Tnolink }; typ_level = 1 }

(* Compute the free type variables in a type *)
let free_type_vars level ty =
  let fv = ref [] in
  let rec free_vars ty =
    let ty = type_repr ty in
    match ty.typ_desc with
    | Tvar _ -> if ty.typ_level >= level then fv := ty :: !fv
    | Tarrow (t1, t2) ->
        free_vars t1;
        free_vars t2
    | Tproduct ty_list -> List.iter free_vars ty_list
    | Tconstr (_, ty_list) -> List.iter free_vars ty_list
  in
  free_vars ty;
  !fv

(* To generalize a type *)

let rec gen_type ty =
  let ty = type_repr ty in
  begin
    match ty.typ_desc with
    | Tvar _ -> if ty.typ_level > !current_level then ty.typ_level <- generic
    | Tarrow (t1, t2) ->
        let lvl1 = gen_type t1 in
        let lvl2 = gen_type t2 in
        ty.typ_level <- (if lvl1 <= lvl2 then lvl1 else lvl2)
    | Tproduct ty_list -> ty.typ_level <- gen_type_list ty_list
    | Tconstr (_, ty_list) -> ty.typ_level <- gen_type_list ty_list
  end;
  ty.typ_level

and gen_type_list = function
  | [] -> notgeneric
  | ty :: rest ->
      let lvl1 = gen_type ty in
      let lvl2 = gen_type_list rest in
      if lvl1 <= lvl2 then lvl1 else lvl2

let generalize_type ty =
  let _ = gen_type ty in
  ()

(* Lower the level of all generalizable variables of a type *)

let rec nongen_type ty =
  let ty = type_repr ty in
  match ty.typ_desc with
  | Tvar _ ->
      if ty.typ_level > !current_level then ty.typ_level <- !current_level
  | Tarrow (t1, t2) ->
      nongen_type t1;
      nongen_type t2
  | Tproduct ty_list -> List.iter nongen_type ty_list
  | Tconstr (_constr, ty_list) -> List.iter nongen_type ty_list

(* Take an instance of a type *)

let rec copy_type = function
  | { typ_desc = Tvar link; typ_level = level } as ty -> begin
      match link.link with
      | Tnolink ->
          if level == generic then begin
            let v = new_type_var () in
            link.link <- Tlinkto v;
            v
          end
          else ty
      | Tlinkto ty -> if level == generic then ty else copy_type ty
    end
  | { typ_desc = Tarrow (t1, t2); typ_level = level } as ty ->
      if level == generic then
        {
          typ_desc = Tarrow (copy_type t1, copy_type t2);
          typ_level = notgeneric;
        }
      else ty
  | { typ_desc = Tproduct ty_list; typ_level = level } as ty ->
      if level == generic then
        {
          typ_desc = Tproduct (List.map copy_type ty_list);
          typ_level = notgeneric;
        }
      else ty
  | { typ_desc = Tconstr (cstr, ty_list); typ_level = level } as ty ->
      if level == generic then
        {
          typ_desc = Tconstr (cstr, List.map copy_type ty_list);
          typ_level = notgeneric;
        }
      else ty

(* When copying is over, we restore the "link" field of generic variables to
   Tnolink. *)
let rec cleanup_type = function
  | { typ_desc = Tvar link; typ_level = level } -> begin
      match link.link with
      | Tnolink -> ()
      | Tlinkto ty ->
          if level == generic then link.link <- Tnolink else cleanup_type ty
    end
  | { typ_desc = Tarrow (t1, t2); typ_level = level } ->
      if level == generic then begin
        cleanup_type t1;
        cleanup_type t2
      end
      else ()
  | { typ_desc = Tproduct ty_list; typ_level = level } ->
      if level == generic then List.iter cleanup_type ty_list else ()
  | { typ_desc = Tconstr (_, ty_list); typ_level = level } ->
      if level == generic then List.iter cleanup_type ty_list else ()

(* The actual instantiation functions *)

let type_instance ty =
  let ty' = copy_type ty in
  cleanup_type ty;
  ty'

(* Expansion of an abbreviation *)

let bind_variable ty1 ty2 =
  match ty1.typ_desc with
  | Tvar link -> begin
      match link.link with
      | Tnolink -> link.link <- Tlinkto ty2
      | Tlinkto _ -> failwith "bind_variable"
    end
  | _ -> failwith "bind_variable"

let expand_abbrev params body args =
  let params' = List.map copy_type params in
  let body' = copy_type body in
  List.iter cleanup_type params;
  cleanup_type body;
  List.iter2 bind_variable params' args;
  body'

(* Unification *)

exception Unify

let occur_check level0 v =
  let rec occurs_rec ty =
    match type_repr ty with
    | { typ_desc = Tvar _; typ_level = level } as ty' ->
        if level > level0 then ty.typ_level <- level0;
        ty' == v
    | { typ_desc = Tarrow (t1, t2); _ } -> occurs_rec t1 || occurs_rec t2
    | { typ_desc = Tproduct ty_list; _ } -> List.exists occurs_rec ty_list
    | { typ_desc = Tconstr (_, ty_list); _ } -> List.exists occurs_rec ty_list
  in
  occurs_rec

let rec unify (ty1, ty2) =
  if ty1 == ty2 then ()
  else begin
    let ty1 = type_repr ty1 in
    let ty2 = type_repr ty2 in
    if ty1 == ty2 then ()
    else begin
      match (ty1.typ_desc, ty2.typ_desc) with
      | Tvar link1, Tvar link2 ->
          if ty1.typ_level < ty2.typ_level then begin
            ty2.typ_level <- ty1.typ_level;
            link2.link <- Tlinkto ty1
          end
          else begin
            ty1.typ_level <- ty2.typ_level;
            link1.link <- Tlinkto ty2
          end
      | Tvar link1, _ when not (occur_check ty1.typ_level ty1 ty2) ->
          link1.link <- Tlinkto ty2
      | _, Tvar link2 when not (occur_check ty2.typ_level ty2 ty1) ->
          link2.link <- Tlinkto ty1
      | Tarrow (t1arg, t1res), Tarrow (t2arg, t2res) ->
          unify (t1arg, t2arg);
          unify (t1res, t2res)
      | Tproduct tyl1, Tproduct tyl2 -> unify_list (tyl1, tyl2)
      | Tconstr (cstr1, []), Tconstr (cstr2, [])
        when cstr1.info.ty_stamp == cstr2.info.ty_stamp
             && cstr1.qualid.qual = cstr2.qualid.qual ->
          ()
      | Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args), _
        ->
          unify (expand_abbrev params body args, ty2)
      | _, Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args)
        ->
          unify (ty1, expand_abbrev params body args)
      | Tconstr (cstr1, tyl1), Tconstr (cstr2, tyl2)
        when cstr1.info.ty_stamp == cstr2.info.ty_stamp
             && cstr1.qualid.qual = cstr2.qualid.qual ->
          unify_list (tyl1, tyl2)
      | _, _ -> raise Unify
    end
  end

and unify_list = function
  | [], [] -> ()
  | ty1 :: rest1, ty2 :: rest2 ->
      unify (ty1, ty2);
      unify_list (rest1, rest2)
  | _ -> raise Unify

(* Two special cases of unification *)

let rec filter_arrow ty =
  match type_repr ty with
  | { typ_desc = Tvar link; typ_level = level } ->
      let ty1 = { typ_desc = Tvar { link = Tnolink }; typ_level = level } in
      let ty2 = { typ_desc = Tvar { link = Tnolink }; typ_level = level } in
      link.link <-
        Tlinkto { typ_desc = Tarrow (ty1, ty2); typ_level = notgeneric };
      (ty1, ty2)
  | { typ_desc = Tarrow (ty1, ty2); _ } -> (ty1, ty2)
  | {
   typ_desc =
     Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args);
   _;
  } ->
      filter_arrow (expand_abbrev params body args)
  | _ -> raise Unify

let rec filter_product arity ty =
  match type_repr ty with
  | { typ_desc = Tvar link; typ_level = level } ->
      let ty_list = type_var_list arity level in
      link.link <-
        Tlinkto { typ_desc = Tproduct ty_list; typ_level = notgeneric };
      ty_list
  | { typ_desc = Tproduct ty_list; _ } ->
      if List.length ty_list == arity then ty_list else raise Unify
  | {
   typ_desc =
     Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args);
   _;
  } ->
      filter_product arity (expand_abbrev params body args)
  | _ -> raise Unify

(* Type matching.
 *
 * Instantiates ty1 so that it is equal to ty2, or raises Unify if not
 * possible.
 *
 * Type ty2 is unmodified. Since the levels in ty1 are not properly updated,
 * ty1 must not be generalized afterwards. *)
let rec filter (ty1, ty2) =
  if ty1 == ty2 then ()
  else begin
    let ty1 = type_repr ty1 in
    let ty2 = type_repr ty2 in
    if ty1 == ty2 then ()
    else begin
      match (ty1.typ_desc, ty2.typ_desc) with
      | Tvar link1, Tvar _ when ty1.typ_level != generic ->
          link1.link <- Tlinkto ty2
      | Tvar link1, _
        when ty1.typ_level != generic && not (occur_check ty1.typ_level ty1 ty2)
        ->
          link1.link <- Tlinkto ty2
      | Tarrow (t1arg, t1res), Tarrow (t2arg, t2res) ->
          filter (t1arg, t2arg);
          filter (t1res, t2res)
      | Tproduct t1args, Tproduct t2args -> filter_list (t1args, t2args)
      | Tconstr (cstr1, []), Tconstr (cstr2, [])
        when same_type_constr cstr1 cstr2 ->
          ()
      | Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args), _
        ->
          filter (expand_abbrev params body args, ty2)
      | _, Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args)
        ->
          filter (ty1, expand_abbrev params body args)
      | Tconstr (cstr1, tyl1), Tconstr (cstr2, tyl2)
        when same_type_constr cstr1 cstr2 ->
          filter_list (tyl1, tyl2)
      | _, _ -> raise Unify
    end
  end

and filter_list = function
  | [], [] -> ()
  | ty1 :: rest1, ty2 :: rest2 ->
      filter (ty1, ty2);
      filter_list (rest1, rest2)
  | _ -> raise Unify

(* Extract the list of labels of a record type *)
let rec labels_of_type ty =
  match (type_repr ty).typ_desc with
  | Tconstr ({ info = { ty_abbr = Tabbrev (params, body); _ }; _ }, args) ->
      labels_of_type (expand_abbrev params body args)
  | Tconstr (cstr, _) -> begin
      match (type_descr_of_type_constr cstr).info.ty_desc with
      | Record_type lbl_list -> lbl_list
      | _ -> failwith "Types.labels_of_type"
    end
  | _ -> failwith "Types.labels_of_type"

(* Check whether a type constructor is a recursive abbrev *)
exception Recursive_abbrev

let check_recursive_abbrev cstr =
  match cstr.info.ty_abbr with
  | Tnotabbrev -> ()
  | Tabbrev (_params, body) ->
      let rec check_abbrev seen ty =
        begin
          match (type_repr ty).typ_desc with
          | Tvar _ -> ()
          | Tarrow (t1, t2) -> begin
              check_abbrev seen t1;
              check_abbrev seen t2
            end
          | Tproduct tlist -> List.iter (check_abbrev seen) tlist
          | Tconstr (c, tlist) ->
              if List.memq c seen then raise Recursive_abbrev
              else begin
                List.iter (check_abbrev seen) tlist;
                begin
                  match c.info.ty_abbr with
                  | Tnotabbrev -> ()
                  | Tabbrev (_params, body) -> check_abbrev (c :: seen) body
                end
              end
        end
      in
      check_abbrev [ cstr ] body
