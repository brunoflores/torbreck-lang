type location =
  | Loc of
      int (* Position of the first character *)
      * int (* Position of the next character following the last one *)
[@@deriving show]

val get_current_location : unit -> location
val output_location : out_channel -> location -> unit
val output_input_name : out_channel -> unit
