(* Generation of bytecode for .zo files *)

open Instruct
open Buffcode
open Opcodes
open Labels

let out_int_const i =
  out constbyte;
  (* out (i + i + 1) *)
  (* TODO: why was it like this? *)
  out i

let rec emit instructions =
  match instructions with
  | [] -> ()
  | Kquote (SCatom (ACint i)) :: code ->
      Printf.printf "out_int_const %d\n" i;
      out_int_const i;
      emit code
  | Kquote sc :: code ->
      Printf.printf "out %s\n" "getglobal";
      out Opcodes.getglobal;
      Reloc.slot_for_literal sc;
      emit code
  | Kaccess n :: code ->
      if n < 6 then begin
        Printf.printf "out access %d\n" n;
        out (acc0 + n)
      end
      else begin
        Printf.printf "out %s\n" "access";
        out access;
        Printf.printf "out n %d\n" n;
        out n
      end;
      emit code
  | Kbranchifnot lbl :: code ->
      Printf.printf "out branchifnot to label %d\n" lbl;
      out branchifnot;
      out_label lbl;
      emit code
  | Kletrec1 lbl :: code ->
      Printf.printf "out letrec1 with label %d\n" lbl;
      out letrec1;
      out_label lbl;
      emit code
  | Kendlet n :: Kendlet p :: code -> emit (Kendlet (n + p) :: code)
  | Kendlet 1 :: code ->
      Printf.printf "out %s\n" "endlet1";
      out endlet1;
      emit code
  | Kendlet n :: code ->
      Printf.printf "out %s\n" "endlet";
      out endlet;
      Printf.printf "out n %d\n" n;
      out n;
      emit code
  | Kprim p :: code ->
      begin
        match p with
        | Pdummy n ->
            out dummy;
            out n
        | Pccall (name, arity) ->
            if arity <= 5 then begin
              Printf.printf "out ccall with arity %d and name %s\n" arity name;
              out (Opcodes.c_call1 + arity - 1)
            end
            else begin
              Printf.printf "out %s\n" "ccalln";
              out Opcodes.c_calln;
              Printf.printf "out arity %d\n" arity;
              out arity
            end;
            Reloc.slot_for_c_prim name
        | Pupdate -> out update
        | _ ->
            Printf.printf "%s\n" (Prim.show_primitive p);
            failwith "not implemented: Emitcode.emit"
      end;
      emit code
  | Kbranch lbl :: code ->
      Printf.printf "out branch to label %d\n" lbl;
      out branch;
      out_label lbl;
      emit code
  | Klabel lbl :: code ->
      if lbl == Instruct.nolabel then failwith "Emitcode.emit: undefined label"
      else begin
        define_label lbl;
        emit code
      end
  | Kclosure lbl :: code ->
      out cur;
      out_label lbl;
      emit code
  | Kpush :: Kget_global _ :: Kapply :: _ ->
      failwith "Emitcode.emit: Kpush::Kget_global::Kapply: not implemented"
  | Kpush :: Kget_global _ :: Ktermapply :: _ ->
      failwith "Emitcode.emit: Kpush::Kget_global::Ktermapply: not implemented"
  | instr :: code ->
      Printf.printf "out %s\n" @@ Instruct.show_zam_instruction instr;
      out
        begin
          match instr with
          | Kreturn -> return
          | Kgrab -> grab
          | Kpush -> push
          | Kpushmark -> pushmark
          | Klet -> let_
          | Kapply -> apply
          | Ktermapply -> appterm
          | Kpoptrap -> poptrap
          | Kcheck_signals -> check_signals
          | _ as instruct ->
              Printf.printf "\n%s\n"
                (List.fold_left
                   (fun acc instr ->
                     acc ^ Instruct.show_zam_instruction instr ^ "\n")
                   "" instructions);
              failwith
              @@ Format.sprintf "Emitcode.emit: should not happen: %s"
                   (Instruct.show_zam_instruction instruct)
        end;
      emit code
