open Parsing

type 'a ast_kind = Structure : Parsetree.structure ast_kind

val parse_implementation : string -> Parsetree.structure
