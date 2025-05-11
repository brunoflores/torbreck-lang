(** Helpers to produce Parsetree fragments. *)

open Asttypes
open Parsetree

type loc = Location.t

(** {1 Constants} *)

module Const : sig
  val mk : ?loc:loc -> constant_desc -> constant
  val char : ?loc:loc -> char -> constant
  val string : ?loc:loc -> string -> constant
  val integer : ?loc:loc -> string -> constant
  val int : ?loc:loc -> int -> constant
  val int32 : ?loc:loc -> int32 -> constant
  val int64 : ?loc:loc -> int64 -> constant
  val nativeint : ?loc:loc -> nativeint -> constant
  val float : ?loc:loc -> string -> constant
end

module Exp : sig
  val mk : ?loc:loc -> expression_desc -> expression

  (* val ident : ?loc:loc -> lid -> expression *)
  val constant : ?loc:loc -> constant -> expression
end

module Str : sig
  val mk : ?loc:loc -> structure_item_desc -> structure_item
  val eval : ?loc:loc -> expression -> structure_item
  val value : ?loc:loc -> rec_flag -> value_binding list -> structure_item
end
