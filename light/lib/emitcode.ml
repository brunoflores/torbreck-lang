(* Generation of bytecode for .zo files *)

open Instruct
open Buffcode
open Opcodes
open Labels
open Const

let out_int_const i =
  out constbyte;
  (* out (i + i + 1) *)
  (* TODO: why was it like this? *)
  out i

let out_tag = function
  | ConstrRegular (t, _) -> out t
  | ConstrExtensible (name, stamp) -> Reloc.slot_for_tag name stamp

let out_header (n, tag) =
  out_tag tag;
  out (Int.shift_left n 2);
  out (Int.shift_right_logical n 6);
  out (Int.shift_right_logical n 14)

let rec emit instructions =
  match instructions with
  | [] -> ()
  | Kquote (SCatom (ACint i)) :: code ->
      out_int_const i;
      emit code
  | Kquote sc :: code ->
      out Opcodes.getglobal;
      Reloc.slot_for_literal sc;
      emit code
  | Kget_global qualid :: code ->
      out getglobal;
      Reloc.slot_for_get_global qualid;
      emit code
  | Kset_global qualid :: code ->
      out setglobal;
      Reloc.slot_for_set_global qualid;
      emit code
  | Kaccess n :: code ->
      if n < 6 then begin
        out (acc0 + n)
      end
      else begin
        out access;
        out n
      end;
      emit code
  | Kbranchif lbl :: code ->
      out branchif;
      out_label lbl;
      emit code
  | Kbranchifnot lbl :: code ->
      out branchifnot;
      out_label lbl;
      emit code
  | Kletrec1 lbl :: code ->
      out letrec1;
      out_label lbl;
      emit code
  | Kmakeblock (tag, n) :: code ->
      if n <= 0 then failwith "Emitcode.emit: Kmakeblock"
      else if n < 5 then begin
        out (makeblock1 + n - 1);
        out_tag tag
      end
      else begin
        out makeblock;
        out_header (n, tag)
      end;
      emit code
  | Kendlet n :: Kendlet p :: code -> emit (Kendlet (n + p) :: code)
  | Kendlet 1 :: code ->
      out endlet1;
      emit code
  | Kendlet n :: code ->
      out endlet;
      out n;
      emit code
  | Kprim Pidentity :: code -> emit code
  | Kprim p :: code ->
      begin
        match p with
        | Pdummy n ->
            out dummy;
            out n
        | Pfield n ->
            if n < 4 then out (getfield0 + n)
            else begin
              out getfield;
              out n
            end
        | Psetfield n ->
            if n < 4 then out (setfield0 + n)
            else begin
              out setfield;
              out n
            end
        | Pccall (name, arity) ->
            if arity <= 5 then begin
              out (Opcodes.c_call1 + arity - 1)
            end
            else begin
              out Opcodes.c_calln;
              out arity
            end;
            Reloc.slot_for_c_prim name
        | Pupdate -> out update
        | Pvectlength -> out vectlength
        | Pgetvectitem -> out getvectitem
        | Praise -> out raise
        | Paddint -> out addint
        | Psubint -> out subint
        | _ ->
            Printf.printf "%s\n" (Prim.show_primitive p);
            failwith "not implemented: Emitcode.emit"
      end;
      emit code
  | Kpushtrap lbl :: code ->
      out pushtrap;
      out_label lbl;
      emit code
  | Kbranch lbl :: code ->
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
  | Kpush :: Kget_global qualid :: Kapply :: code ->
      out push_getglobal_apply;
      Reloc.slot_for_get_global qualid;
      emit code
  | Kpush :: Kget_global qualid :: Ktermapply :: code ->
      out push_getglobal_appterm;
      Reloc.slot_for_get_global qualid;
      emit code
  | instr :: code ->
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
