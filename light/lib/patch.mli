(* Relocate a block of object bytecode *)

val patch_object : bytes -> int -> (Reloc.info * int) list -> unit
