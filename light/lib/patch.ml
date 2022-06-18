(* Relocate a block of object bytecode *)

open Symtable

let patch_short buff pos value =
  Bytes.set buff pos (char_of_int value);
  Bytes.set buff (succ pos) (char_of_int (value lsr 8))

let patch_object (buff : bytes) (offset : int) : (Reloc.info * int) list -> unit
    =
  List.iter (function
    | Reloc.Reloc_literal sc, pos ->
        patch_short buff (pos + offset) (get_slot_for_literal sc)
    | Reloc_getglobal id, pos ->
        let v = get_slot_for_variable id in
        patch_short buff (pos + offset) v
    | Reloc_setglobal id, pos ->
        let v = get_slot_for_defined_variable id in
        patch_short buff (pos + offset) v
    | Reloc_tag (id, stamp), pos ->
        Bytes.set buff (pos + offset) (char_of_int (get_num_of_exn (id, stamp)))
    | Reloc_primitive name, pos ->
        patch_short buff (pos + offset) (get_num_of_prim name))
