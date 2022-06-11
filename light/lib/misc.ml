let rollback_buffer = ref ([] : (unit -> unit) list)
let add_rollback f = rollback_buffer := f :: !rollback_buffer
let load_path = ref ([] : string list)

exception Cannot_find_file of string

let find_in_path filename =
  if Sys.file_exists filename then filename
  else if Filename.is_relative filename = false then
    raise (Cannot_find_file filename)
  else
    let rec find = function
      | [] -> raise (Cannot_find_file filename)
      | a :: rest ->
          let b = Filename.concat a filename in
          if Sys.file_exists b then b else find rest
    in
    find !load_path
