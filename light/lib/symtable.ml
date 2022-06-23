(* Assign numbers to global variables *)

open Const

let object_name = ref ""

type 'a numtable = {
  mutable num_cnt : int; (* The current number *)
  mutable num_tbl : ('a, int) Hashtbl.t; (* The table *)
}

let new_numtable size = { num_cnt = 0; num_tbl = Hashtbl.create size }
let find_in_numtable nt = Hashtbl.find nt.num_tbl

let enter_in_numtable nt key =
  let c = nt.num_cnt in
  nt.num_cnt <- succ nt.num_cnt;
  Hashtbl.add nt.num_tbl key c;
  c

let custom_runtime = ref false
let c_prim_table = ref (new_numtable 1 : string numtable)

let reserve_in_numtable nt key =
  let _ = enter_in_numtable nt key in
  ()

(* Global variables *)

let global_table = ref (new_numtable 1 : qualified_ident numtable)
let literal_table = ref ([] : (int * struct_constant) list)
let number_of_globals () = !global_table.num_cnt

let get_slot_for_variable qualid =
  try find_in_numtable !global_table qualid
  with Not_found ->
    if String.length !object_name > 0 then
      Printf.eprintf
        "The global value %s__%s is referenced (from %s) before being defined. \
         Please link %s.zo before %s.\n"
        qualid.qual qualid.id !object_name qualid.qual !object_name
    else
      Printf.eprintf
        "The global value %s__%s is referenced before being defined. Please \
         load an implementation of module %s first.\n"
        qualid.qual qualid.id qualid.qual;
    failwith "get_slot_for_variable"

let reserve_slot_for_variable qualid =
  let _ = get_slot_for_variable qualid in
  ()

let get_slot_for_defined_variable qualid =
  enter_in_numtable !global_table qualid

let reserve_slot_for_defined_variable qualid =
  let _ = get_slot_for_defined_variable qualid in
  ()

let get_slot_for_literal cst =
  let c = !global_table.num_cnt in
  !global_table.num_cnt <- succ !global_table.num_cnt;
  literal_table := (c, cst) :: !literal_table;
  c

(* The exception tags *)

let exn_tag_table = ref (new_numtable 1 : (qualified_ident * int) numtable)
let tag_exn_table = ref ([||] : (qualified_ident * int) array)
let unknown_exn_name = ({ qual = "?"; id = "?" }, 0)

let get_num_of_exn (name, stamp) =
  try Hashtbl.find !exn_tag_table.num_tbl (name, stamp)
  with Not_found ->
    let c = enter_in_numtable !exn_tag_table (name, stamp) in
    if c >= Array.length !tag_exn_table then begin
      let new_tag_exn_table =
        Array.make (2 * Array.length !tag_exn_table) unknown_exn_name
      in
      Array.blit !tag_exn_table 0 new_tag_exn_table 0
        (Array.length !tag_exn_table);
      tag_exn_table := new_tag_exn_table
    end;
    !tag_exn_table.(c) <- (name, stamp);
    c

let get_num_of_tag = function
  | ConstrRegular (n, _) -> n
  | ConstrExtensible (id, stamp) -> get_num_of_exn (id, stamp)

(* The C primitives *)

let get_num_of_prim name =
  try find_in_numtable !c_prim_table name
  with Not_found ->
    if !custom_runtime then enter_in_numtable !c_prim_table name
    else begin
      Printf.eprintf "C primitive not available: %s\n" name;
      failwith "Symtable.get_num_of_prim"
    end

let set_c_primitives prim_vect =
  c_prim_table := new_numtable 10;
  List.iter (fun name -> reserve_in_numtable !c_prim_table name) prim_vect;
  ()

(* Initialization *)

let reset_linker_tables () =
  global_table := new_numtable 1;
  literal_table := [];
  set_c_primitives
    [
      "print_endline";
      "lessthan";
      "string_of_int";
      "greaterequal";
      "sys_exit";
      "open_descriptor";
      "print_string";
      "input_char";
      "equal";
    ];
  (* https://github.com/brunoflores/camllight/blob/master/sources/src/linker/Makefile#L36 *)
  List.iter reserve_slot_for_defined_variable
    [ { qual = "sys"; id = "command_line" } ];
  exn_tag_table := new_numtable 31;
  tag_exn_table := Array.make 50 unknown_exn_name
