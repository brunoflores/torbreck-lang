(* open Asttypes *)
open Parsetree

type loc = Location.t

let default_loc = ref Location.none

module Const = struct
  let mk ?(loc = !default_loc) d = { pconst_desc = d; pconst_loc = loc }
  let integer ?loc i = mk ?loc (Pconst_integer i)
  let int ?loc i = integer ?loc (Int.to_string i)
  let int32 ?loc i = integer ?loc (Int32.to_string i)
  let int64 ?loc i = integer ?loc (Int64.to_string i)
  let nativeint ?loc i = integer ?loc (Nativeint.to_string i)
  let float ?loc f = mk ?loc (Pconst_float f)
  let char ?loc c = mk ?loc (Pconst_char c)
  let string ?(loc = !default_loc) s = mk ~loc (Pconst_string (s, loc))
end
