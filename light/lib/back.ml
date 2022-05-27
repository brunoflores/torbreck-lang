(* Translation of lambda terms to lists of instructions *)

open Instruct
open Lambda

(* Label generation *)

let label_counter = ref 0

let reset_label () = label_counter := 0

and new_label () =
  incr label_counter;
  !label_counter

(* To keep track of function bodies that remain to be compiled. *)
let still_to_compile = (Stack.create () : (lambda * int) Stack.t)

(* The translator from lambda terms to lists of instructions. *)
let compile_expr _staticfail =
  let compexp expr code =
    match expr with
    | Lvar n -> Kaccess n :: code
    | Lconst c -> begin
        match code with
        | (Kquote _ | Kget_global _ | Kaccess _ | Kpushmark) :: _ -> code
        | _ -> Kquote c :: code
      end
    | x ->
        failwith
        @@ Printf.sprintf "not implemented: Back.compile_expr: %s"
             (Lambda.show_lambda x)
  in
  compexp

let rec compile_rest code =
  try
    let exp, lbl = Stack.pop still_to_compile in
    compile_rest (Klabel lbl :: compile_expr nolabel exp (Kreturn :: code))
  with Stack.Empty -> code

let compile_lambda rec_flag expr =
  Stack.clear still_to_compile;
  reset_label ();
  let init_code = compile_expr nolabel expr [] in
  let function_code = compile_rest [] in
  { kph_rec = rec_flag; kph_init = init_code; kph_fcts = function_code }
