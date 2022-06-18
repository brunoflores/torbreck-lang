(* Buffer bytecode in memory during emission *)

(* Link to the unsafe char_of_int in the OCaml runtime.
   The safe [Stdlib.char_of_int n] is a partial function where [n]
   is >= 0 and <= 255.
   We need positive and negative offsets so we do our own checking. *)
external unsafe_char_of_int : int -> char = "%identity"

let out_buffer = ref (Bytes.create 64)
let out_position = ref 0

let realloc_out_buffer () =
  let len = Bytes.length !out_buffer in
  let new_buffer = Bytes.create (2 * len) in
  Bytes.blit !out_buffer 0 new_buffer 0 len;
  out_buffer := new_buffer

let init_out_code () = out_position := 0

let out (b : int) : unit =
  if !out_position >= Bytes.length !out_buffer then realloc_out_buffer ();
  Bytes.set !out_buffer !out_position (unsafe_char_of_int b);
  incr out_position

let out_short (s : int) : unit =
  if s >= 32768 || s < -32768 then Error.displacement_overflow ()
  else begin
    out s;
    out (Int.shift_right_logical s 8)
  end
