(* Translation of lambda terms to lists of instructions *)

open Instruct
open Lambda

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

(* Generate a branch to the given list of instructions *)
let make_branch = function
  | Kreturn :: _ as code -> (Kreturn, code)
  | (Kbranch _ as branch) :: _ as code -> (branch, code)
  | code ->
      let lbl = new_label () in
      (Kbranch lbl, Klabel lbl :: code)

(* Discard all instructions up to the next label *)
let rec discard_dead_code = function
  | [] -> []
  | Klabel _ :: (_ as code) -> code
  | Kset_global _ :: (_ as code) -> code
  | _ :: rest -> discard_dead_code rest

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
    | Lprim (p, explist) -> compexplist explist (Kprim p :: code)
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
      :: compexp ifso
           (branch1 :: Klabel lbl2 :: compexp ifnot (Kreturn :: code1)))
    (* TODO: I added a Kreturn :: code1 here. *)
  in
  compexp

let rec compile_rest code =
  try
    let exp, lbl = Stack.pop still_to_compile in
    compile_rest (Klabel lbl :: compile_expr nolabel exp (Kreturn :: code))
  with Stack.Empty -> code

let compile_lambda rec_flag expr =
  Printf.printf "\n%s\n" (Lambda.show_lambda expr);
  Stack.clear still_to_compile;
  reset_label ();
  let init_code = compile_expr nolabel expr [] in
  let function_code = compile_rest [] in
  { kph_rec = rec_flag; kph_init = init_code; kph_fcts = function_code }
