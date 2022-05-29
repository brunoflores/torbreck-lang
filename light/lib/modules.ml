(* Handling of modules and global symbol tables *)

open Misc
open Globals

(* Information associated with module names *)

type module' = {
  mod_name : string; (* name of the module *)
  mod_values : (string, value_desc global) Hashtbl.t; (* table of values *)
  mod_types : (string, type_desc global) Hashtbl.t;
      (* table of type constructors *)
  mod_constrs : (string, constr_desc global) Hashtbl.t;
      (* table of constructors *)
  mutable mod_type_stamp : int; (* stamp for type constructors *)
  mutable mod_exc_stamp : int; (* stamp for exceptions *)
  mutable mod_persistent : bool;
      (* true if this interface comes from a .zi file *)
}

let values_of_module md = md.mod_values
and types_of_module md = md.mod_types
and constrs_of_module md = md.mod_constrs

let module_table = (Hashtbl.create 37 : (string, module') Hashtbl.t)

let new_module name =
  let md =
    {
      mod_name = name;
      mod_values = Hashtbl.create 17;
      mod_types = Hashtbl.create 7;
      mod_constrs = Hashtbl.create 42;
      mod_type_stamp = 0;
      mod_exc_stamp = 0;
      mod_persistent = false;
    }
  in
  Hashtbl.add module_table name md;
  md

(* To load an interface from a file *)

let read_module _basename filename =
  let ic = open_in_bin filename in
  try
    let md = (input_value ic : module') in
    close_in ic;
    md.mod_persistent <- true;
    md
  with End_of_file | Failure _ ->
    close_in ic;
    failwith @@ Printf.sprintf "Corrupted compiled interface file %s." filename

let load_module name =
  let fullname = name ^ ".zi" in
  let _ =
    if Sys.file_exists fullname = false then
      failwith
      @@ Printf.sprintf "Cannot find the compiled interface file %s." fullname
  in
  read_module name fullname

(* To find an interface by its name *)

let find_module filename =
  let modname = Filename.basename filename in
  try Hashtbl.find module_table modname
  with Not_found ->
    let md = load_module filename in
    Hashtbl.add module_table modname md;
    md

(* The table of all opened modules *)

let opened_modules =
  ref
    {
      mod_name = "";
      mod_values = Hashtbl.create 1;
      mod_types = Hashtbl.create 1;
      mod_constrs = Hashtbl.create 42;
      mod_type_stamp = 0;
      mod_exc_stamp = 0;
      mod_persistent = false;
    }

let opened_modules_names = ref ([] : string list)
let used_opened_modules = ref (Hashtbl.create 1 : (string, bool ref) Hashtbl.t)

let reset_opened_modules () =
  opened_modules :=
    {
      mod_name = "";
      mod_values = Hashtbl.create 1;
      mod_types = Hashtbl.create 1;
      mod_constrs = Hashtbl.create 42;
      mod_type_stamp = 0;
      mod_exc_stamp = 0;
      mod_persistent = false;
    };
  opened_modules_names := [];
  used_opened_modules := Hashtbl.create 1

(* Open a module and add its definitions to the table of opened modules *)

let add_table t1 t2 = Hashtbl.iter (Hashtbl.add t2) t1

let open_module name =
  let m = find_module name in
  add_table m.mod_values !opened_modules.mod_values;
  add_table m.mod_constrs !opened_modules.mod_constrs;
  add_table m.mod_types !opened_modules.mod_types;
  opened_modules_names := name :: !opened_modules_names;
  Hashtbl.add !used_opened_modules name (ref false)

(* The current state of the compiler *)

let default_used_modules = ref ([] : string list)
let defined_module = ref (new_module "")

let start_compiling_interface name =
  defined_module := new_module name;
  reset_opened_modules ();
  List.iter open_module !default_used_modules

let start_compiling_implementation name intf =
  start_compiling_interface name;
  !defined_module.mod_type_stamp <- intf.mod_type_stamp;
  !defined_module.mod_exc_stamp <- intf.mod_exc_stamp;
  ()

let compiled_module_name () = !defined_module.mod_name

let defined_global name desc =
  { qualid = { qual = compiled_module_name (); id = name }; info = desc }

(* Additions to the module being compiled *)

let add_global_info sel_fct glob =
  let tbl = sel_fct !defined_module in
  (* if !toplevel then begin *)
  add_rollback (fun () -> Hashtbl.remove tbl glob.qualid.id);
  (* end *)
  Hashtbl.add tbl glob.qualid.id glob

let add_value = add_global_info values_of_module
and add_constr = add_global_info constrs_of_module
and add_type = add_global_info types_of_module

(* Find the descriptor for a reference to a global identifier.
    If the identifier is qualified (mod__name), just look into module mod.
    If the identifier is not qualified, look inside the current module,
   then inside the table of opened modules. *)

exception Desc_not_found

let find_desc sel_fn = function
  | GRmodname q -> begin
      try Hashtbl.find (sel_fn (find_module q.qual)) q.id
      with Not_found -> raise Desc_not_found
    end
  | GRname s -> (
      try Hashtbl.find (sel_fn !defined_module) s
      with Not_found -> (
        try
          let res = Hashtbl.find (sel_fn !opened_modules) s in
          (* Record the module as actually used *)
          Hashtbl.find !used_opened_modules res.qualid.qual := true;
          res
        with Not_found -> raise Desc_not_found))

let find_value_desc = find_desc values_of_module
and find_constr_desc = find_desc constrs_of_module
and find_type_desc = find_desc types_of_module

(* To write the interface of the module currently compiled *)
let write_compiled_interface oc = output_value oc !defined_module
