open Utils

type t = Warnings.loc
type 'a loc = { txt : 'a; loc : t }
