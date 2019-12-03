open Spotlib.Spot

type ('desc, 'attr) with_loc_and_type =
  { desc : 'desc
  ; loc  : Location.t
  ; typ  : Michelson.Type.t
  ; attr : 'attr
  }

val dummy_loc : Location.t
                 
type var = Ident.t

type constr = CLeft | CRight | CSome | CNone | CCons | CNil | CUnit | CBool of bool | CPair | CConstant of Michelson.Constant.t
(* Glitch: Unit, true, false, None, [] are not CConstant *) 
   
val string_of_constr : constr -> string

module Pat : sig
  type desc =
    | Var of var
    | Constr of constr * t list
    | Wild
    | Alias of t * Ident.t * Location.t (* location of ident *)
    | Or of t * t

  and t = (desc, unit) with_loc_and_type

  val pp : Format.t -> t -> unit
      
  type patvar = (Ident.t, unit) with_loc_and_type
  
  val pp_patvar : Format.t -> patvar -> unit
end

type attr = 
  | Comment of string

type attrs = attr list

type t = (desc, attrs) with_loc_and_type

and desc =
  | Const of Michelson.Opcode.constant
  | Nil of Michelson.Type.t
  | Cons of t * t
  | IML_None of Michelson.Type.t
  | IML_Some of t
  | Left of Michelson.Type.t * t
  | Right of Michelson.Type.t * t
  | Unit
  | Var of Tools.Ident.t * Michelson.Type.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of Michelson.Type.t * Michelson.Type.t * Pat.patvar * t
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * (Michelson.Opcode.t list -> Michelson.Opcode.t list) * t list
  | Let of Pat.patvar * t * t
  | Switch_or of t * Pat.patvar * t * Pat.patvar * t
  | Switch_cons of t * Pat.patvar * Pat.patvar * t * t
  | Switch_none of t * t * Pat.patvar * t
  | Match of t * (Pat.t * t option * t) list

val pp : Format.t -> t -> unit

val mke : Michelson.Type.t -> desc -> t
val mkfst : t -> t
val mksnd : t -> t
val mkeq : t -> t -> t
  
val implementation : string -> Typedtree.structure -> Michelson.Type.t * Michelson.Type.t * t

module IdTys : Set.S with type elt = Ident.t * Michelson.Type.t

val patvars : Pat.t -> IdTys.t
                       
val freevars : t -> IdTys.t

val optimize : t -> t

val save : string -> t -> unit
(* Print out IML AST to a file.  For debugging. *)
