let rollback_buffer = ref ([] : (unit -> unit) list)
let add_rollback f = rollback_buffer := f :: !rollback_buffer
