val try_finally :
  ?always:(unit -> unit) -> ?exceptionally:(unit -> unit) -> (unit -> 'a) -> 'a
