(* Relocation information *)

open Const

type info =
  | Reloc_literal of struct_constant  (** structured constant *)
  | Reloc_getglobal of qualified_ident  (** reference to a global *)
  | Reloc_setglobal of qualified_ident  (** definition of a global *)
  | Reloc_tag of qualified_ident * int  (** exception tag*)
  | Reloc_primitive of string  (** C primitive number *)

let reloc_info = ref ([] : (info * int) list)
let reset () = reloc_info := []
