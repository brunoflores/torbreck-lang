(** Handling of modules and global symbol tables. *)

(* open Misc *)
open Globals

type 'a globals_table = (string, 'a global) Hashtbl.t
type values_table = value_desc globals_table
type types_table = type_desc globals_table
type constrs_table = constr_desc globals_table

module Module = struct
  type t = {
    name : string;  (** Name of the module *)
    values : values_table;  (** Table of values *)
    types : types_table;  (** Table of type constructors *)
    constrs : constrs_table;  (** Table of constructors *)
    mutable type_stamp : int;  (** Stamp for type constructors *)
    mutable exc_stamp : int;  (** Stamp for exceptions *)
    mutable persistent : bool;
        (** [true] if this interface comes from a .zi file *)
  }
  (** Information associated with module names *)

  (* let get_value { values; _ } s = Hashtbl.find_opt values s *)
  let values m = m.values
  let types m = m.types
  let constrs m = m.constrs

  let defaults name =
    {
      name;
      values = Hashtbl.create 17;
      types = Hashtbl.create 7;
      constrs = Hashtbl.create 42;
      type_stamp = 0;
      exc_stamp = 0;
      persistent = false;
    }

  let input_from_file ic =
    let m : t = input_value ic in
    m.persistent <- true;
    m
end

let table = (Hashtbl.create 37 : (string, Module.t) Hashtbl.t)

let add name =
  let m = Module.defaults name in
  Hashtbl.add table name m;
  m

(** Load an interface from a file. *)
let read_module filepath =
  try
    let ic = open_in_bin filepath in
    try
      let m = Module.input_from_file ic in
      close_in ic;
      m
    with _ ->
      close_in_noerr ic;
      failwith
      @@ Printf.sprintf "Corrupted compiled interface file: %s." filepath
  with _ -> failwith @@ Printf.sprintf "Could not open file: %s" filepath

let load_module name =
  let name = name ^ ".zi" in
  try
    let fullpath = Misc.find_in_path name in
    read_module fullpath
  with Misc.Cannot_find_file _ ->
    failwith @@ Printf.sprintf "Cannot find compiled interface file: %s." name

(** Find an interface by its name. *)
let find_module filename =
  let modname = Filename.basename filename in
  try Hashtbl.find table modname
  with Not_found ->
    let md = load_module filename in
    Hashtbl.add table modname md;
    md

module State = struct
  let opened_names = ref []
  let values : values_table = Hashtbl.create 1
  let constrs : constrs_table = Hashtbl.create 1
  let types : types_table = Hashtbl.create 1
  let used_names : (string, bool ref) Hashtbl.t = Hashtbl.create 1
  let default_used_modules = ref ([] : string list)
  let defined_module = ref (Module.defaults "")
  let add_table t1 t2 = Hashtbl.iter (Hashtbl.add t2) t1
  let add_value v = Hashtbl.add !defined_module.values v.qualid.id v
  let add_constr c = Hashtbl.add !defined_module.constrs c.qualid.id c
  let add_type t = Hashtbl.add !defined_module.types t.qualid.id t

  exception Desc_not_found

  let find_desc :
      (Module.t -> 'a globals_table) ->
      'a globals_table ->
      global_reference ->
      'a global =
   fun fn table -> function
    | GRmodname q -> begin
        try Hashtbl.find (fn (find_module q.qual)) q.id
        with Not_found -> raise Desc_not_found
      end
    | GRname s -> begin
        try Hashtbl.find (fn !defined_module) s
        with Not_found ->
          begin
            try
              let res = Hashtbl.find table s in
              (* Record the module as actually used *)
              Hashtbl.find used_names res.qualid.qual := true;
              res
            with Not_found -> raise Desc_not_found
          end
      end

  let find_value_desc = find_desc Module.values values
  and find_constr_desc = find_desc Module.constrs constrs
  and find_type_desc = find_desc Module.types types

  let open_module name =
    let m = find_module name in
    add_table m.values values;
    add_table m.constrs constrs;
    add_table m.types types;
    opened_names := name :: !opened_names;
    Hashtbl.add used_names name (ref false)

  let reset_opened () =
    Hashtbl.clear values;
    Hashtbl.clear constrs;
    Hashtbl.clear types;
    Hashtbl.clear used_names;
    opened_names := []

  let reset () =
    defined_module := Module.defaults "";
    reset_opened ();
    List.iter open_module !default_used_modules

  let start_compiling_implementation (intf : Module.t) =
    !defined_module.type_stamp <- intf.type_stamp;
    !defined_module.exc_stamp <- intf.exc_stamp

  let type_descr_of_type_constr cstr =
    let rec select_type_descr = function
      | [] -> raise Desc_not_found
      | desc :: rest ->
          if desc.info.ty_constr.info.ty_stamp = cstr.info.ty_stamp then desc
          else select_type_descr rest
    in
    select_type_descr
      (Hashtbl.find_all (find_module cstr.qualid.qual).types cstr.qualid.qual)

  let new_exc_stamp () =
    let s = succ !defined_module.exc_stamp in
    !defined_module.exc_stamp <- s;
    s

  let new_type_stamp () =
    let s = succ !defined_module.type_stamp in
    !defined_module.type_stamp <- s;
    s

  let defined_global name desc =
    { qualid = { qual = !defined_module.name; id = name }; info = desc }
end

(* let compiled_module_name () = !defined_module.mod_name *)

(* Additions to the module being compiled *)

(* let add_global_info sel_fct glob = *)
(*   let tbl = sel_fct !defined_module in *)
(*   (\* if !toplevel then begin *\) *)
(*   add_rollback (fun () -> Hashtbl.remove tbl glob.qualid.id); *)
(*   (\* end *\) *)
(*   Hashtbl.add tbl glob.qualid.id glob *)

(* let add_value = add_global_info values_of_module *)
(* let add_constr = add_global_info constrs_of_module *)
(* let add_type = add_global_info types_of_module *)

(* Find the descriptor for a reference to a global identifier.
    If the identifier is qualified (mod__name), just look into module mod.
    If the identifier is not qualified, look inside the current module,
   then inside the table of opened modules. *)

(* exception Desc_not_found *)

(* let find_desc sel_fn = function *)
(*   | GRmodname q -> begin *)
(*       try Hashtbl.find (sel_fn (find_module q.qual)) q.id *)
(*       with Not_found -> raise Desc_not_found *)
(*     end *)
(*   | GRname s -> begin *)
(*       try Hashtbl.find (sel_fn !defined_module) s *)
(*       with Not_found -> ( *)
(*         try *)
(*           let res = Hashtbl.find (sel_fn !opened_modules) s in *)
(*           (\* Record the module as actually used *\) *)
(*           Hashtbl.find !used_opened_modules res.qualid.qual := true; *)
(*           res *)
(*         with Not_found -> raise Desc_not_found) *)
(*     end *)

(* let find_value_desc = find_desc values_of_module *)
(* and find_constr_desc = find_desc constrs_of_module *)
(* and find_type_desc = find_desc types_of_module *)

(* To write the interface of the module currently compiled *)
let write_compiled_interface oc = output_value oc !State.defined_module
