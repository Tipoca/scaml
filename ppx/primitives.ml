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

  ; "Set.empty", (0, fun typ xs ->
        assert (xs = []);
        match typ with
        | TySet ty -> [EMPTY_SET ty]
        | _ -> assert false)

  ; "Set.length"  , (1, simple [SIZE])
  ; "Set.mem"     , (2, simple [MEM])
  ; "Set.update"  , (3, simple [UPDATE])
                    
  ; "Set.fold"    , (3, fun typ xs -> 
        let exec = function
          | false -> [ EXEC ]
          | true -> [ DIP [ DUP; CDR; DIP [ CAR ] ]; PAIR; EXEC ]
        in
        let is_closure1, is_closure2 =
          match typ with
          | TyLambda (typ, _, _) ->
              begin match typ with
              | TyLambda (_, TyLambda (_, _, cli2), cli1) ->
                  (match (repr_closure_info cli1).closure_desc with
                   | CLList xs -> xs <> []
                   | _ -> assert false),
                  (match (repr_closure_info cli2).closure_desc with
                   | CLList xs -> xs <> []
                   | _ -> assert false)
              | _ -> assert false
              end
          | _ -> 
              Format.eprintf "Set.fold: %a@." M.Type.pp typ;
              assert false
        in
(* I do not know this is optimal or not

  lam : set : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  elt : acc : lam : s                   DIP { DIP { DUP } SWAP }
  elt : lam : acc : lam : s             EXECx
  lam2 : acc : lam : s                  SWAP
  acc : lam2 : lam : s                  EXECx
  acc : lam : s                         }

  set : acc : lam : s                   ITER {

  empty : acc : lam : s                 ITER {
  acc : lam : s                         DIP { DROP }
  acc : s
   
*)           

        xs @
        [ SWAP ; DIP [ SWAP ];
          ITER ([ DIP [ DIP [ DUP ]; SWAP ] ]
                @ exec is_closure1
                @ [ SWAP ]
                @ exec is_closure2);
          DIP [ DROP ]
        ])
  ]
