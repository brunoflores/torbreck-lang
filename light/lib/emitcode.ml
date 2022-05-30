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
  | Kquote sc :: code ->
      out Opcodes.getglobal;
      Reloc.slot_for_literal sc;
      emit code
  | xs ->
      Printf.printf "Instructions list:\n%s"
        (List.fold_left
           (fun acc x -> Printf.sprintf "%s\t%s\n" acc (show_zam_instruction x))
           "" xs);
      failwith "not implemented: Emitcode.emit"
