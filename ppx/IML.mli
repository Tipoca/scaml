type pat = {
  loc : Location.t;
  id : Tools.Ident.t;
  typ : Michelson.Type.t;
}

type t = { loc : Location.t; desc : desc; typ : Michelson.Type.t; }

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

val structure : parameter: Michelson.Type.t ->
  (Tools.Ident.t * Michelson.Type.t) list -> Typedtree.structure -> t

val fix_entrypoint_type :
  string -> Typedtree.structure -> Michelson.Type.t * Michelson.Type.t
