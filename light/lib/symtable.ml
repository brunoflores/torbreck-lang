(* Assign numbers to global variables *)

open Const

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

let get_slot_for_literal cst =
  let c = !global_table.num_cnt in
  !global_table.num_cnt <- succ !global_table.num_cnt;
  literal_table := (c, cst) :: !literal_table;
  c

let number_of_globals () = !global_table.num_cnt

(* The C primitives *)

let get_num_of_prim name =
  try find_in_numtable !c_prim_table name
  with Not_found ->
    if !custom_runtime then enter_in_numtable !c_prim_table name
    else begin
      Printf.eprintf "C primitive not available: %s\n" name;
      failwith "Symtable.get_num_of_prim"
    end

(* Initialization *)

let reset_linker_tables () =
  c_prim_table := new_numtable 10;
  List.iter
    (fun name -> reserve_in_numtable !c_prim_table name)
    [ "print_endline"; "lessthan"; "-int"; "+int"; "string_of_int" ];
  ()
