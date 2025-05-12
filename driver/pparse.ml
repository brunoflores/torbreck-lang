open Utils
open Parsing

type 'a ast_kind = Structure : Parsetree.structure ast_kind

let preprocess sourcefile = sourcefile
let remove_preprocessed _inputfile = ()

let file_aux ~sourcefile inputfile parse_fun invariant_fun kind =
  (* TODO *)
  let _ = sourcefile in
  let _ = invariant_fun in
  let _ = kind in

  let set_input_lexbuf ic =
    let source = In_channel.input_all ic in
    let lexbuf = Lexing.from_string source in
    (* Location.input_lexbuf := Some lexbuf; *)
    lexbuf
  in
  let ic = open_in_bin inputfile in
  let close_ic () = close_in ic in
  let lexbuf =
    Fun.protect ~finally:close_ic @@ fun () ->
    seek_in ic 0;
    set_input_lexbuf ic
  in
  (* let _ = Location.init lexbuf sourcefile in *)
  let ast = parse_fun lexbuf in
  ast

let parse_file invariant_fun parse kind sourcefile =
  let inputfile = preprocess sourcefile in
  Misc.try_finally
    (fun () -> file_aux ~sourcefile inputfile parse invariant_fun kind)
    ~always:(fun () -> remove_preprocessed inputfile)

let parse (type a) (kind : a ast_kind) lexbuf : a =
  match kind with Structure -> Parser.implementation Lexer.token lexbuf

let parse_implementation sourcefile =
  let ast_invariants = fun () -> () in
  parse_file ast_invariants (parse Structure) Structure sourcefile
