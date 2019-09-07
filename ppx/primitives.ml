module M = Michelson
open M.Opcode
open M.Type

(*
type code = 
  | Nullary of (M.Type.t -> M.Opcode.t list)
  | Unary   of (M.Type.t -> M.Opcode.t list -> M.Opcode.t list)
  | Binary  of (M.Type.t -> M.Opcode.t list -> M.Opcode.t list -> M.Opcode.t list)
  | Ternary of (M.Type.t -> M.Opcode.t list -> M.Opcode.t list -> M.Opcode.t list -> M.Opcode.t list)

let arity = function
  | Nullary _ -> 0
  | Unary _ -> 1
  | Binary _ -> 2
  | Ternary _ -> 3
*)

let simple os = fun _ty pre -> pre @ os

(* We can omit the types here, since they are coded in SCaml.ml *)

let primitives = 
  [ "fst"     , (1, simple [CAR])
  ; "snd"     , (1, simple [CDR])
  ; "compare" , (2, simple [COMPARE])
  ; "="       , (2, simple [COMPARE; EQ])
  ; "<>"      , (2, simple [COMPARE; NEQ])
  ; "<"       , (2, simple [COMPARE; LT])
  ; ">"       , (2, simple [COMPARE; GT])
  ; "<="      , (2, simple [COMPARE; LE])
  ; ">="      , (2, simple [COMPARE; GE])
  ; "+"       , (2, simple [ADD])
  ; "+^"      , (2, simple [ADD])
  ; "+$"      , (2, simple [ADD])
  ; "-"       , (2, simple [SUB])
  ; "-^"      , (2, simple [SUB])
  ; "-$"      , (2, simple [SUB])
  ; "*"       , (2, simple [MUL])
  ; "*^"      , (2, simple [MUL])
  ; "*$"      , (2, simple [MUL])
  ; "&&"      , (2, simple [AND])
  ; "||"      , (2, simple [OR])
  ; "xor"     , (2, simple [XOR])
  ; "not"     , (1, simple [NOT])
  ; "abs"     , (1, simple [ABS])
  ; "~-"      , (1, simple [NEG])

  ; "Set.empty", (0, (fun typ xs ->
        assert (xs = []);
        match typ with
        | TySet ty -> [EMPTY_SET ty]
        | _ -> assert false))

  ; "Set.length"  , (1, simple [SIZE])
  ; "Set.mem"     , (2, simple [MEM])
  ; "Set.update"  , (3, simple [UPDATE])
  ]
