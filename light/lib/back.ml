(* Translation of lambda terms to lists of instructions *)

open Instruct
open Lambda
open Prim

(* Determine if we are in tail call position *)
let rec is_return = function
  | Kreturn :: _ -> true
  | Klabel _ :: c -> is_return c
  | Kevent _ :: c -> is_return c
  | _ -> false

(* Label generation *)

let label_counter = ref 0

let reset_label () = label_counter := 0

and new_label () =
  incr label_counter;
  !label_counter

(* Add a label to a list of instructions *)
let label_code = function
  | Kbranch lbl :: _ as code -> (lbl, code)
  | Klabel lbl :: _ as code -> (lbl, code)
  | code ->
      let lbl = new_label () in
      (lbl, Klabel lbl :: code)

(* Generate a branch to the given list of instructions.
   Note that [as] has a lower precedence level than cons [::]. *)
let make_branch = function
  | Kreturn :: _ as code -> (Kreturn, code)
  | (Kbranch _ as branch) :: _ as code -> (branch, code)
  | code ->
      let lbl = new_label () in
      (Kbranch lbl, Klabel lbl :: code)

(* Discard all instructions up to the next label.
   Note that [as] has a lower precedence level than cons [::]. *)
let rec discard_dead_code = function
  | [] -> []
  | Klabel _ :: _ as code -> code (* Actually [(Klabel _ :: _) as code] *)
  | Kset_global _ :: _ as code ->
      code (* Actually [(Kset_global _ :: _) as code] *)
  | _ :: code -> discard_dead_code code

(* Inversion of a boolean test ( < becomes >= and so on ) *)
let invert_bool_test =
  let invert_prim_test = function
    | PTeq -> PTnoteq
    | PTnoteq -> PTeq
    | PTnoteqimm _ -> failwith "Back.invert_prim_test"
    | PTlt -> PTge
    | PTle -> PTgt
    | PTgt -> PTle
    | PTge -> PTlt
  in
  function
  | Peq_test -> Pnoteq_test
  | Pnoteq_test -> Peq_test
  | Pint_test t -> Pint_test (invert_prim_test t)
  | Pfloat_test t -> Pfloat_test (invert_prim_test t)
  | Pstring_test t -> Pstring_test (invert_prim_test t)
  | Pnoteqtag_test _ -> failwith "Back.invert_bool_test"

(* To keep track of function bodies that remain to be compiled. *)
let still_to_compile = (Stack.create () : (lambda * int) Stack.t)

(* The translator from lambda terms to lists of instructions. *)
let compile_expr _staticfail =
  let rec compexp expr code =
    (* Printf.printf "\nCompile lambda:\n%s\n" (Lambda.show_lambda expr); *)
    match expr with
    | Lvar n -> Kaccess n :: code
    | Lconst c -> begin
        match code with
        | (Kquote _ | Kget_global _ | Kaccess _ | Kpushmark) :: _ -> code
        | _ -> Kquote c :: code
      end
    | Lprim (Pget_global qualid, []) -> Kget_global qualid :: code
    | Lprim (Pset_global qualid, [ exp ]) ->
        compexp exp (Kset_global qualid :: code)
    | Lprim (Pmakeblock tag, explist) ->
        compexplist explist (Kmakeblock (tag, List.length explist) :: code)
    | Lprim (Pnot, [ exp ]) -> begin
        match code with
        | Kbranchif lbl :: code' -> compexp exp (Kbranchifnot lbl :: code')
        | Kbranchifnot lbl :: code' -> compexp exp (Kbranchif lbl :: code')
        | _ -> compexp exp (Kprim Pnot :: code)
      end
    | Lprim (Psequand, [ exp1; exp2 ]) -> begin
        match code with
        | Kbranch lbl :: _ ->
            compexp exp1 (Kstrictbranchifnot lbl :: compexp exp2 code)
        | Kbranchifnot lbl :: _ ->
            compexp exp1 (Kbranchifnot lbl :: compexp exp2 code)
        | Kbranchif lbl :: code' ->
            let lbl1, code1 = label_code code' in
            compexp exp1
              (Kbranchifnot lbl1 :: compexp exp2 (Kbranchif lbl :: code1))
        | _ ->
            let lbl = new_label () in
            compexp exp1
              (Kstrictbranchifnot lbl :: compexp exp2 (Klabel lbl :: code))
      end
    | Lprim (Psequor, [ exp1; exp2 ]) -> begin
        match code with
        | Kbranch lbl :: _ ->
            compexp exp1 (Kstrictbranchif lbl :: compexp exp2 code)
        | Kbranchif lbl :: _ -> compexp exp1 (Kbranchif lbl :: compexp exp2 code)
        | Kbranchifnot lbl :: code' ->
            let lbl1, code1 = label_code code' in
            compexp exp1
              (Kbranchif lbl1 :: compexp exp2 (Kbranchifnot lbl :: code1))
        | _ ->
            let lbl = new_label () in
            compexp exp1
              (Kstrictbranchif lbl :: compexp exp2 (Klabel lbl :: code))
      end
    | Lprim ((Ptest tst as p), explist) -> begin
        match code with
        | Kbranchif lbl :: code' ->
            compexplist explist (Ktest (tst, lbl) :: code')
        | Kbranchifnot lbl :: code' ->
            compexplist explist (Ktest (invert_bool_test tst, lbl) :: code')
        | _ -> compexplist explist (Kprim p :: code)
      end
    | Lprim (Praise, explist) ->
        compexplist explist (Kprim Praise :: discard_dead_code code)
    | Lprim (p, explist) -> compexplist explist (Kprim p :: code)
    | Lhandle (body, handler) ->
        let branch1, code1 = make_branch code in
        let lbl2 = new_label () in
        let code2 = if is_return code1 then code1 else Kendlet 1 :: code1 in
        Kpushtrap lbl2
        :: compexp body
             (Kpoptrap :: branch1 :: Klabel lbl2 :: compexp handler code2)
    | Lapply (body, args) ->
        if is_return code then
          compexplist args
            (Kpush :: compexp body (Ktermapply :: discard_dead_code code))
        else
          Kpushmark :: compexplist args (Kpush :: compexp body (Kapply :: code))
    | Lfunction body ->
        if is_return code then Kgrab :: compexp body code
        else begin
          let lbl = new_label () in
          Stack.push (body, lbl) still_to_compile;
          Kclosure lbl :: code
        end
    | Llet (args, body) ->
        let code1 =
          if is_return code then code else Kendlet (List.length args) :: code
        in
        let rec comp_args = function
          | [] -> compexp body code1
          | exp :: rest -> compexp exp (Klet :: comp_args rest)
        in
        comp_args args
    | Lletrec ([ (Lfunction f, _) ], body) ->
        let code1 = if is_return code then code else Kendlet 1 :: code in
        let lbl = new_label () in
        Stack.push (f, lbl) still_to_compile;
        Kletrec1 lbl :: compexp body code1
    | Lletrec (args, body) ->
        let size = List.length args in
        let code1 = if is_return code then code else Kendlet size :: code in
        let rec comp_args i = function
          | [] -> compexp body code1
          | (exp, _sz) :: rest ->
              compexp exp
                (Kpush :: Kaccess i :: Kprim Pupdate :: comp_args (i - 1) rest)
        in
        List.fold_right
          (fun (_, sz) code -> Kprim (Pdummy sz) :: Klet :: code)
          args
          (comp_args (size - 1) args)
    | Lshared (expr, lbl_ref) ->
        if !lbl_ref == Instruct.nolabel then begin
          let lbl = new_label () in
          lbl_ref := lbl;
          Klabel lbl :: compexp expr code
        end
        else begin
          Kbranch !lbl_ref :: discard_dead_code code
        end
    | Lifthenelse (cond, ifso, ifnot) -> comp_test2 cond ifso ifnot code
    | Lsequence (exp1, exp2) -> compexp exp1 (compexp exp2 code)
    | Lwhile (cond, body) ->
        let lbl1 = new_label () in
        let lbl2 = new_label () in
        Kbranch lbl1 :: Klabel lbl2 :: Kcheck_signals
        :: compexp body
             (Klabel lbl1
             :: compexp cond (Kbranchif lbl2 :: Kquote Const.const_unit :: code)
             )
    | Lcond _ -> failwith "here"
    | x ->
        Printf.printf "%s\n" (Lambda.show_lambda x);
        failwith "not implemented: Back.compile_expr"
  and compexplist explist code =
    match explist with
    | [] -> code
    | [ exp ] -> compexp exp code
    | exp :: rest -> compexplist rest (Kpush :: compexp exp code)
  and comp_test2 cond ifso ifnot code =
    let branch1, code1 = make_branch code in
    let lbl2 = new_label () in
    compexp cond
      (Kbranchifnot lbl2
      :: compexp ifso (branch1 :: Klabel lbl2 :: compexp ifnot code1))
  in
  compexp

let rec compile_rest code =
  try
    let exp, lbl = Stack.pop still_to_compile in
    compile_rest (Klabel lbl :: compile_expr nolabel exp (Kreturn :: code))
  with Stack.Empty -> code

let compile_lambda rec_flag expr =
  (* Printf.printf "\n%s\n" (Lambda.show_lambda expr); *)
  Stack.clear still_to_compile;
  reset_label ();
  let init_code = compile_expr nolabel expr [] in
  let function_code = compile_rest [] in
  { kph_rec = rec_flag; kph_init = init_code; kph_fcts = function_code }
