let compile_intf_phrase (phr : Syntax.intf_phrase) =
  match phr.in_desc with
  | Zvaluedecl decl -> Ty_decl.type_valuedecl phr.in_loc decl
  | Ztypedecl decl ->
      let _ = Ty_decl.type_typedecl phr.in_loc decl in
      ()

let compile_impl_phrase oc (phr : Syntax.impl_phrase) =
  (* reset_type_expression_vars(); *)
  match phr.im_desc with
  | Zexpr expr ->
      let _ty = Ty_decl.type_expression phr.im_loc expr in
      Emit_phr.emit_phrase oc (Syntax.expr_is_pure expr)
        (Back.compile_lambda false (Front.translate_expression expr))
  | x ->
      Printf.printf "%s\n" (Syntax.show_impl_desc x);
      failwith "Compiler.compile_impl_phrase: not implemented"
