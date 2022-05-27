(* To buffer bytecode during emission *)

let out_buffer = ref (Bytes.create 64)
and out_position = ref 0

let realloc_out_buffer () =
  let len = Bytes.length !out_buffer in
  let new_buffer = Bytes.create (2 * len) in
  Bytes.blit !out_buffer 0 new_buffer 0 len;
  out_buffer := new_buffer;
  ()

let init_out_code () =
  out_position := 0;
  ()

let out (b : int) =
  if !out_position >= Bytes.length !out_buffer then realloc_out_buffer ();
  Bytes.set !out_buffer !out_position (char_of_int b);
  incr out_position

let out_short (s : int) =
  if s >= 32768 || s < -32768 then Error.displacement_overflow ()
  else begin
    out s;
    out @@ Int.shift_right_logical s 8
  end
