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

(* EXEC for pure lambda and closure *)
let exec = function
  | false -> [ EXEC ]
  | true -> [ DIP [ DUP; CDR; DIP [ CAR ] ]; PAIR; EXEC ]

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
  ; "lsl"     , (2, simple [LSL])
  ; "lsr"     , (2, simple [LSR])
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

  ; "Map.empty", (0, fun typ xs ->
        assert (xs = []);
        match typ with
        | TyMap (ty1,ty2) -> [EMPTY_MAP (ty1, ty2)]
        | _ -> assert false)

  ; "Set.length"  , (1, simple [SIZE])
  ; "Map.length"  , (1, simple [SIZE])

  ; "Set.mem"     , (2, simple [MEM])
  ; "Set.update"  , (3, simple [UPDATE])
                    
  ; "Set.fold"    , (3, fun typ xs -> 
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
(*
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
      
  ; "Loop.left"    , (2, fun typ xs -> 
        let is_closure =
          match typ with
          | TyLambda (typ, _, _) ->
              begin match typ with
              | TyLambda (_, _, cli1) ->
                  (match (repr_closure_info cli1).closure_desc with
                   | CLList xs -> xs <> []
                   | _ -> assert false)
              | _ -> assert false
              end
          | _ -> 
              Format.eprintf "Loop.left %a@." M.Type.pp typ;
              assert false
        in
        let rty =
          match typ with
          | TyLambda (_, TyLambda(_, rty, _), _) -> rty
          | _ -> 
              Format.eprintf "Loop.left %a@." M.Type.pp typ;
              assert false
        in
(* lambda : acc : S                 SWAP ; LEFT ;
   Left acc : lambda : S            LOOP_LEFT {
   acc : lambda : S                   DUP DIP
   acc : lambda : lambda : S          EXECx
   LorR : lambda : S
   
   Left acc : lambda : S            LOOP_LEFT { ..
   
   Right res : lambda : S           LOOP_LEFT { .. }
   res : lambda : S                 DIP DROP

*)
        xs @
        [ SWAP ; LEFT rty;
          LOOP_LEFT (  DIP [ DUP ] :: exec is_closure );
          DIP [ DROP ] ])
      
  ; "String.concat",   (2, simple [CONCAT])
  ; "Bytes.concat",    (2, simple [CONCAT]) (* XXX no test available *)
                       
  ; "Contract.self",   (0, simple [SELF])
                       
  ; "Map.map", (2, fun typ xs ->
        (* XXX dup *)
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
(* lambda : map : S                 SWAP ;
   { (k,v); <tl> } : lambda : S     MAP {
     (k, v) : lambda : S              DIP DUP
     (k, v) : lambda : lambda : S     DUP CAR DIP { CDR ; SWAP }
     k : lambda : v : lambda : S      EXECx
     lambda' : v : lambda : S         SWAP EXECx
     w : : lambda : S
   
   { <tl> } : lambda : S            MAP { ..
   
   {} : lambda : S                  Map {

   map' : lambda : S                  DIP DROP
   map' : S 
   
*)
        xs @
        [ SWAP ; 
          MAP ( 
            [ DIP [ DUP ];
              DUP; CAR; DIP [ CDR; SWAP ] ]
            @ exec is_closure1
            @ [ SWAP ]
            @ exec is_closure2
          ) ;
          DIP [ DROP ]
        ])

  ; "Map.get", (2, simple [ GET ] )
  ]
