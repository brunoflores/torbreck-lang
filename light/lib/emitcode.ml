(* Generation of bytecode for .zo files *)

open Instruct
open Buffcode
open Opcodes

let out_int_const i =
  out constbyte;
  out (i + i + 1)

let rec emit = function
  | [] -> ()
  | Kquote (SCatom (ACint i)) :: code ->
      out_int_const i;
      emit code
  | xs ->
      failwith
      @@ Printf.sprintf "not implemented: Emitcode.emit:\n%s"
           (List.fold_left
              (fun acc x ->
                Printf.sprintf "%s\t%s\n" acc (show_zam_instruction x))
              "" xs)