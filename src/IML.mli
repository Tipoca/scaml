type 'desc with_loc_and_type =
  { desc : 'desc
  ; loc : Location.t
  ; typ : Michelson.Type.t
  }

type pat = Ident.t with_loc_and_type

type t = desc with_loc_and_type

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
  | Tuple of t * t
  | Assert of t
  | AssertFalse
  | Fun of Michelson.Type.t * Michelson.Type.t * pat * t *
      (Tools.Ident.t * Michelson.Type.t) list
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * (Michelson.Opcode.t list -> Michelson.Opcode.t list) * t list
  | Let of pat * t * t
  | Switch_or of t * pat * t * pat * t
  | Switch_cons of t * pat * pat * t * t
  | Switch_none of t * t * pat * t

val pp : Format.formatter -> t -> unit

val implementation : string -> Typedtree.structure -> Michelson.Type.t * Michelson.Type.t * t
