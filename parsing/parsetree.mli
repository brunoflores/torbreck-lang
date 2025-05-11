(** Abstract syntax tree produced by parsing. *)

open Asttypes
open Location

type constant = { pconst_desc : constant_desc; pconst_loc : Location.t }

and constant_desc =
  | Pconst_integer of string
  | Pconst_char of char
  | Pconst_string of string * Location.t
  | Pconst_float of string

type location_stack = Location.t list

(** {1 Core language} *)

(** {2 Type expressions} *)

type core_type = {
  ptyp_desc : core_type_desc;
  ptyp_loc : Location.t;
  ptyp_loc_stack : location_stack;
}

and core_type_desc =
  | Ptyp_any  (** [_] *)
  | Ptyp_var of string  (** A type variable such as ['a] *)
  | Ptyp_arrow of core_type * core_type  (** [T1 -> T2] *)

(** {2 Patterns} *)

and pattern = {
  ppat_desc : pattern_desc;
  ppat_loc : Location.t;
  ppat_loc_stack : location_stack;
}

and pattern_desc = Ppat_any  (** The pattern [_] *)

(** {2 Value expressions} *)

and expression = {
  pexp_desc : expression_desc;
  pexp_loc : Location.t;
  pexp_loc_stack : location_stack;
}

and expression_desc =
  | Pexp_ident of Longident.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression

(** {2 Type declarations} *)

and type_declaration = { ptype_name : string loc; ptype_loc : Location.t }

(** {2 } *)

and structure = structure_item list

and structure_item = { pstr_desc : structure_item_desc; pstr_loc : Location.t }

and structure_item_desc =
  | Pstr_eval of expression
  | Pstr_value of rec_flag * value_binding list

and value_constraint =
  | Pvc_constraint of { typ : core_type }
      (** A simple type constraint on a value binding: [ let x : typ ] *)

and value_binding = {
  pvb_pat : pattern;
  pvb_expr : expression;
  pvb_constraint : value_constraint option;
  pvb_loc : Location.t;
}
(** [let pat : type_constraint = exp] *)
