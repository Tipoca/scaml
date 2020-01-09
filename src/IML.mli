(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2019  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

module PatVar : sig
  type t = (Ident.t, unit) with_loc_and_type
  (** Pattern variable *)
  
  val pp : Format.t -> t -> unit
end

module Attr : sig
  type t = Comment of string
  type ts = t list
      
  val add : t -> ('a, ts) with_loc_and_type -> ('a, ts) with_loc_and_type
  val adds : ts -> ('a, ts) with_loc_and_type -> ('a, ts) with_loc_and_type
end

type contract_source =
  | Tz_code of string (* String literal of Michelson code *)
  | Tz_file of string (* String literal of Michelson file *)

type t = (desc, Attr.ts) with_loc_and_type

and desc =
  | Const of Michelson.Constant.t
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
  | Fun of PatVar.t * t
  | IfThenElse of t * t * t option
  | App of t * t list
  | Prim of string * (Michelson.Opcode.t list -> Michelson.Opcode.t list) * t list
  | Let of PatVar.t * t * t
  | Switch_or of t * PatVar.t * t * PatVar.t * t
  | Switch_cons of t * PatVar.t * PatVar.t * t * t
  | Switch_none of t * t * PatVar.t * t
  | Contract_create of contract_source * Location.t * t * t * t
  | Seq of t * t
  | Set of t list
  | Map of (t * t) list

val pp : Format.t -> t -> unit

module P : sig
  val pp : Format.t -> t -> unit
end
  
val save : string -> t -> unit
(** Print out IML AST to a file.  For debugging. *)

val freevars : t -> IdTys.t
val subst : (Ident.t * t) list -> t -> t
val alpha_conv : (Ident.t * Ident.t) list -> t -> t

val check_unstorable : t -> (unit, Ident.t * Michelson.Type.t) Result.t
