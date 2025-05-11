(* type ref_and_reset = *)
(*   | Table : { ref : 'a ref; init : unit -> 'a } -> ref_and_reset *)
(*   | Ref : { ref : 'a ref; mutable snapshot : 'a } -> ref_and_reset *)

(* type bindings = { *)
(*   mutable refs : ref_and_reset list; *)
(*   mutable frozen : bool; *)
(*   mutable is_bound : bool; *)
(* } *)

(* let global_bindings = { refs = []; is_bound = false; frozen = false } *)

let s_ref _k = failwith "not implemented"
(* let ref = ref k in *)
(* assert (not global_bindings.frozen); *)
(* global_bindings.refs <- Ref { ref; snapshot = k } :: global_bindings.refs; *)
(* ref *)
