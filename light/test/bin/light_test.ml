module Parser = LightLib.Parser
module Modules = LightLib.Modules
module Compiler = LightLib.Compiler
module Paraux = LightTestLib.Paraux

let () =
  let modname = "one" in
  let mli = "value print_string : string -> unit = 1 \"print_string \";;" in
  let _ml = "print_string \"42\";;" in
  let content = mli in
  let lexbuf = content |> Lexing.from_string in
  begin
    try
      Modules.default_used_modules := [ "builtin" ];
      Modules.start_compiling_interface modname;
      Paraux.parse Parser.Incremental.interface
        (fun phr -> Compiler.compile_intf_phrase phr)
        lexbuf content
    with Sys_error s | Failure s -> failwith s
  end;
  let md = !Modules.defined_module in
  match md with
  | {
   mod_name = "one";
   mod_values;
   mod_types;
   mod_constrs;
   mod_type_stamp = 0;
   mod_exc_stamp = 0;
   mod_persistent = false;
  } -> begin
      begin
        try
          let md = Hashtbl.find mod_values "print_string" in
          match md.info.val_typ.typ_desc with
          | Tarrow
              ( { typ_desc = Tconstr ({ info = { ty_stamp = 7; _ }; _ }, []); _ },
                {
                  typ_desc = Tconstr ({ info = { ty_stamp = 2; _ }; _ }, []);
                  _;
                } ) ->
              ()
          | _ -> failwith "not the expected arrow type"
        with Not_found -> failwith "symbol not found"
      end;
      if Hashtbl.length mod_types > 0 then failwith "expected no types";
      if Hashtbl.length mod_constrs > 0 then failwith "expected no constructors"
    end
  | _ -> failwith "test failed: Modules.defined_module: module not found"
