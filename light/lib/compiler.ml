let compile_intf_phrase (phr : Syntax.intf_phrase) =
  match phr.in_desc with
  | Zvaluedecl decl -> Ty_decl.type_valuedecl phr.in_loc decl

let compile_impl_phrase (phr : Syntax.impl_phrase) =
  (* reset_type_expression_vars(); *)
  match phr.im_desc with
  | Zexpr expr ->
      let _ty = Ty_decl.type_expression phr.im_loc expr in
      Back.compile_lambda false (Front.translate_expression expr)
  | x ->
      Printf.printf "%s\n" (Syntax.show_impl_desc x);
      failwith "not implemented"
