open Utils

type t = Warnings.loc = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
}

let none =
  let loc = Lexing.dummy_pos in
  { loc_start = loc; loc_end = loc }

type 'a loc = { txt : 'a; loc : t }
