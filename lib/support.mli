module Error : sig
  type info =
    | INFO of {
        pos_fname : string;
        pos_lnum : int;
        pos_bol : int;
        pos_cnum : int;
      }
    | DUMMY
  [@@deriving show]
end
