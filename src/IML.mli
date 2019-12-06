open Spotlib.Spot

type ('desc, 'attrs) with_loc_and_type =
  { desc  : 'desc
  ; loc   : Location.t
  ; typ   : Michelson.Type.t
  ; attrs : 'attrs
  }

module Constructor : sig
  type t = Left | Right | Some | None | Cons | Nil | Unit | Bool of bool | Pair | Constant of Michelson.Constant.t
  (* Glitch: Unit, true, false, None, [] are not Constant *) 
   
  val to_string : t -> string
end

module IdTys : Set.S with type elt = Ident.t * Michelson.Type.t

module Pat : sig
  type desc =
    | Var of Ident.t
    | Constr of Constructor.t * t list
    | Wild
    | Alias of t * Ident.t * Location.t (* location of ident *)
    | Or of t * t

  and t = (desc, unit) with_loc_and_type

  val pp : Format.t -> t -> unit
      
  type var = (Ident.t, unit) with_loc_and_type
  
  val pp_var : Format.t -> var -> unit

  val vars : t -> IdTys.t
end

type attr = 
  | Comment of string

type attrs = attr list

type t = (desc, attrs) with_loc_and_type

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
  | Contract_create_raw of string

val pp : Format.t -> t -> unit

val mke : Michelson.Type.t -> desc -> t
val mkfst : t -> t
val mksnd : t -> t
val mkeq : t -> t -> t
  
val implementation : string -> Typedtree.structure -> Michelson.Type.t * Michelson.Type.t * t

val freevars : t -> IdTys.t

val optimize : t -> t

val save : string -> t -> unit
(* Print out IML AST to a file.  For debugging. *)
