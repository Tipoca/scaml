open Spotlib.Spot

type ('desc, 'attrs) with_loc_and_type =
  { desc  : 'desc
  ; loc   : Location.t
  ; typ   : Michelson.Type.t
  ; attrs : 'attrs
  }
(** AST nodes with type, location and attributes *)

module IdTys : Set.S with type elt = Ident.t * Michelson.Type.t
(** Set of idents and their types *)

module Pat : sig
  type var = (Ident.t, unit) with_loc_and_type
  (** Pattern variable *)
  
  val pp_var : Format.t -> var -> unit
end

module Attr : sig
  type t = Comment of string
  type ts = t list
end

type t = (desc, Attr.ts) with_loc_and_type

and desc =
  | Const of Michelson.Opcode.constant
  | Nil
  | Cons of t * t
  | IML_None
  | IML_Some of t
  | Left of t
  | Right of t
  | Unit
  | Var of Ident.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of Pat.var * t
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * (Michelson.Opcode.t list -> Michelson.Opcode.t list) * t list
  | Let of Pat.var * t * t
  | Switch_or of t * Pat.var * t * Pat.var * t
  | Switch_cons of t * Pat.var * Pat.var * t * t
  | Switch_none of t * t * Pat.var * t
  | Contract_create_raw of Michelson.Opcode.module_ * t * t * t
  | Seq of t * t

val pp : Format.t -> t -> unit

val implementation : string -> Typedtree.structure -> Michelson.Type.t * Michelson.Type.t * t

val freevars : t -> IdTys.t

val optimize : t -> t

val save : string -> t -> unit
(** Print out IML AST to a file.  For debugging. *)
