open Syntax

let do_directive _loc = function
  | Zdir ("open", name) -> Modules.open_module name
  | Zdir (d, _) -> failwith @@ Format.sprintf "unknown directive: %s\n" d

let compile_intf_phrase (phr : Syntax.intf_phrase) =
  match phr.in_desc with
  | Zvaluedecl decl -> Ty_decl.type_valuedecl phr.in_loc decl
  | Ztypedecl decl ->
      let _ = Ty_decl.type_typedecl phr.in_loc decl in
      ()
  | Zintfdirective dir -> do_directive phr.in_loc dir
  | Zexcdecl decl ->
      let _ = Ty_decl.type_excdecl phr.in_loc decl in
      ()

let compile_impl_phrase oc (phr : Syntax.impl_phrase) =
  (* reset_type_expression_vars(); *)
  match phr.im_desc with
  | Zexpr expr ->
      let _ty = Ty_decl.type_expression phr.im_loc expr in
      Emit_phr.emit_phrase oc (Syntax.expr_is_pure expr)
        (Back.compile_lambda false (Front.translate_expression expr))
  | Zletdef (rec_flag, pat_expr_list) ->
      let _env = Ty_decl.type_letdef phr.im_loc rec_flag pat_expr_list in
      Emit_phr.emit_phrase oc
        (Syntax.letdef_is_pure pat_expr_list)
        (if rec_flag then
         Back.compile_lambda true
           (Front.translate_letdef_rec phr.im_loc pat_expr_list)
        else
          Back.compile_lambda false
            (Front.translate_letdef phr.im_loc pat_expr_list))
  | Zimpldirective dir -> do_directive phr.im_loc dir
  | Zexcdef decl ->
      let _ = Ty_decl.type_excdecl phr.im_loc decl in
      ()
