(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* We can omit the types here, since they are coded in SCaml.ml *)
(* XXX We should consider to merge SCaml.ml and primitives.ml into one *)
                                                
module M = Michelson
open M.Opcode
open M.Type
open Tools
open Spotlib.Spot

let simple ~loc:_ os = fun _ty pre -> pre @ os

let rec args ty = function
  | 0 -> []
  | n ->
      match ty.desc with
      | TyLambda (ty1, ty2) -> ty1 :: args ty2 (n-1)
      | _ -> assert false
  
let comparison ~loc os ty pre = 
  match args ty 2 with
  | [ty1; _ty2] -> (* ty1 == ty2 *)
      if not & M.Type.is_comparable ty1 then
        errorf_primitive ~loc "Comparison operator takes a non comparable type %a"
          M.Type.pp ty1;
      pre @ os
  | _ -> assert false

let primitives = 
  [ "fst"     , (1, simple [CAR])
  ; "snd"     , (1, simple [CDR])

  ; "compare" , (2, comparison [COMPARE])
  ; "="       , (2, comparison [COMPARE; EQ])
  ; "<>"      , (2, comparison [COMPARE; NEQ])
  ; "<"       , (2, comparison [COMPARE; LT])
  ; ">"       , (2, comparison [COMPARE; GT])
  ; "<="      , (2, comparison [COMPARE; LE])
  ; ">="      , (2, comparison [COMPARE; GE])

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
  ; "isnat"   , (1, simple [ISNAT])
  ; "~-"      , (1, simple [NEG])
  ; "~-^"     , (1, simple [NEG])

  ; "lor"          , (2, simple [OR])
  ; "land"         , (2, simple [AND])
  ; "land_int_nat" , (2, simple [AND])
  ; "lxor"         , (2, simple [XOR])
  ; "lnot_nat"     , (1, simple [NOT])
  ; "lnot"         , (1, simple [NOT])

  ; "List.length"  , (1, simple [SIZE])
  ; "List.map"     , (2, fun ~loc:_ _typ xs ->
(* lambda : map : S              SWAP ;
   { hd; <tl> } : lambda : S     MAP {
     hd : lambda : S              DIP DUP
     hd : lambda : lambda : S     EXECx
     hd' : lambda : S
   
   {} : lambda : S               MAP {..}

   list' : lambda : S             DIP DROP
   list' : S 
*)
        xs @
        [ SWAP ; 
          MAP ( 
            [ DIP (1, [ DUP ]) ]
            @ [ EXEC ]
          ) ;
          DIP (1, [ DROP 1 ])
        ])


  ; "List.fold_left"    , (3, fun ~loc:_ _typ xs -> 
(*
  lam : acc : list : s                  SWAP; DIP { SWAP } SWAP
  list : acc : lam : s                  ITER {
  hd : acc : lam : s                       DIP { DIP { DUP } SWAP }
  hd : lam : acc : lam : s                 EXEC
  lam' : acc : lam : s                     SWAP EXEC
  acc' : lam : s                        }

  [] : acc : lam : s                    ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)           
        xs @
        [ SWAP ; DIP (1, [ SWAP ]); SWAP;
          ITER ([ DIP (1, [ DIP (1, [ DUP ]); SWAP ]) ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]);
          DIP (1, [ DROP 1])
        ])

  ; "List.fold_left'"    , (3, fun ~loc:_ _typ xs -> 
(*
  lam : acc : list : s                  SWAP; DIP { SWAP } SWAP
  list : acc : lam : s                  ITER {
  hd : acc : lam : s                       SWAP PAIR
  (acc, hd) : lam : s                      DIP { DUP }  
  (acc, hd) : lam : lam : s                EXEC
  acc : lam : s                         }

  [] : acc : lam : s                    ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)           
        xs @
        [ SWAP ; DIP (1, [ SWAP ]); SWAP;
          ITER [ SWAP ; PAIR ; DIP (1, [ DUP ]); EXEC ];
          DIP (1, [ DROP 1])
        ])

  ; "List.rev", (1, fun ~loc:_ ty xs -> 
        match ty.desc with
        | TyLambda ({ desc= TyList ty }, { desc= TyList _ty' }) ->
            (* ty = _ty' *)
            xs @ [DIP (1, [NIL ty]); ITER [CONS]]
        | _ -> assert false)

  ; "Set.empty", (0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TySet ty -> [EMPTY_SET ty]
        | _ -> assert false)

  ; "Set.length"  , (1, simple [SIZE])
  ; "Set.mem"     , (2, simple [MEM])
  ; "Set.update"  , (3, simple [UPDATE])

  ; "Set.fold"    , (3, fun ~loc:_ _typ xs -> 
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
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER ([ DIP (1, [ DIP (1, [ DUP ]); SWAP ]) ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]);
          DIP (1, [ DROP 1 ])
        ])
      
  ; "Set.fold'"    , (3, fun ~loc:_ _typ xs -> 
(*
  lam : set : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  elt : acc : lam : s                     PAIR
  (elt, acc) : lam : s                    DIP DUP
  (elt, acc) : lam : lam : s              EXEC
  acc : lam : s                         }

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)           

        xs @
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER [ PAIR; DIP (1, [ DUP ]); EXEC ];
          DIP (1, [ DROP 1 ])
        ])
      
  ; "Loop.left"    , (2, fun ~loc:_ typ xs -> 
        let rty =
          match typ.desc with
          | TyLambda (_, { desc= TyLambda(_, rty) }) -> rty
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
          LOOP_LEFT (  DIP (1, [ DUP ]) :: [ EXEC ] );
          DIP (1, [ DROP 1 ]) ])
      
  ; "String.concat",   (2, simple [CONCAT])
  ; "^",               (2, simple [CONCAT])
  ; "String.length",   (1, simple [SIZE])
  ; "Bytes.concat",    (2, simple [CONCAT])
  ; "Bytes.length",    (1, simple [SIZE]) 

  ; "Map.empty", (0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TyMap (ty1,ty2) -> [EMPTY_MAP (ty1, ty2)]
        | _ -> assert false)

  ; "Map.length"  , (1, simple [SIZE])
  ; "Map.get", (2, simple [ GET ] )
  ; "Map.mem", (2, simple [MEM])
  ; "Map.update", (3, simple [UPDATE])

  ; "Map.map", (2, fun ~loc:_ _typ xs ->
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
            [ DIP (1, [ DUP ]);
              DUP; CAR; DIP (1, [ CDR; SWAP ]) ]
            @ [ EXEC ]
            @ [ SWAP ]
            @ [ EXEC ]
          ) ;
          DIP (1, [ DROP 1 ])
        ])

  ; "Map.map'", (2, fun ~loc:_ _typ xs ->
(* lambda : map : S                 SWAP ;
   { (k,v); <tl> } : lambda : S     MAP {
     (k, v) : lambda : S              DIP DUP
     (k, v) : lambda : lambda : S     EXEC
     w : : lambda : S
   
   {} : lambda : S                  Map {..}

   map' : lambda : S                  DIP DROP
   map' : S 
   
*)
        xs @
        [ SWAP ; 
          MAP [ DIP (1, [ DUP ]);
                EXEC ];
          DIP (1, [ DROP 1 ])
        ])

  ; "Map.fold"    , (3, fun ~loc:_ _typ xs -> 
(*
  lam : map : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  (k,v) : acc : lam : s                 DUP CAR DIP { CDR }
  k : v : acc : lam : s                 DIP { DIP { DIP { DUP } SWAP } SWAP
  k : lam : v : acc : lam : s           EXECx
  lam2 : v : acc : lam : s              SWAP
  v : lam2 : acc : lam : s              EXECx
  lam3 : acc : lam : s                  SWAP
  acc : lam3 : lam : s                  EXECx
  acc' : lam : s

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)           

        xs @
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER ([ DUP; CAR; DIP (1, [ CDR ]);
                  DIP (1, [ DIP (1, [ DIP (1, [ DUP ]); SWAP ]); SWAP ]) ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]
                @ [ SWAP ]
                @ [ EXEC ]);
          DIP (1, [ DROP 1 ])
        ])
      

  ; "Map.fold'"    , (3, fun ~loc:_ _typ xs -> 
(*
  lam : map : acc : s                   SWAP DIP { SWAP }
  set : acc : lam : s                   ITER {
  (k,v) : acc : lam : s                   DUP CAR DIP { CDR }
  k : v : acc : lam : s                   DIP { PAIR } PAIR
  (k, (v, acc)) : lam : s                 DIP DUP
  (k, (v, acc)) : lam : lam : s           EXEC
  acc : lam : s                         } 

  empty : acc : lam : s                 ITER {..}
  acc : lam : s                         DIP { DROP }
  acc : s
*)           

        xs @
        [ SWAP ; DIP (1, [ SWAP ]);
          ITER [ DUP; CAR; DIP (1, [ CDR ]);
                 DIP (1, [ PAIR ]); PAIR;
                 DIP (1, [ DUP ]);
                 EXEC ];
          DIP (1, [ DROP 1 ])
        ])
      

  (* big map *)

  ; "BigMap.empty", (0, fun ~loc:_ typ xs ->
        assert (xs = []);
        match typ.desc with
        | TyBigMap (ty1,ty2) -> [EMPTY_BIG_MAP (ty1, ty2)]
        | _ -> assert false)
  ; "BigMap.get", (2, simple [ GET ] )
  ; "BigMap.mem", (2, simple [MEM])
  ; "BigMap.update", (3, simple [UPDATE])
               
  ; "Obj.pack", (1, fun ~loc ty pre ->
        match args ty 1 with
        | [ aty ] ->
            if not & M.Type.is_packable ~legacy:true aty then
              errorf_primitive ~loc "Obj.pack cannot take a non packable type %a"
                M.Type.pp aty;
            pre @ [ PACK ]
        | _ -> assert false)

  ; "Obj.unpack", (1, fun ~loc ty xs ->
      match ty.desc with
      | TyLambda (_, { desc= TyOption ty }) ->
          if not & M.Type.is_packable ~legacy:false ty then
            errorf_primitive ~loc "Obj.unpack cannot unpack to a non packable type %a"
              M.Type.pp ty;
          xs @ [ UNPACK ty ]
      | _ -> assert false)
      
  ; "String.slice", (3, simple [ SLICE ])
  ; "Bytes.slice", (3, simple [ SLICE ]) (* XXX not tested *)
                   
  ; "Contract.contract", (1, fun ~loc:_ ty xs ->
        match ty.desc with
        | TyLambda (_, { desc= TyOption ({ desc= TyContract ty }) }) ->
            xs @ [ CONTRACT ty ]
        | _ -> assert false)

  ; "Contract.implicit_account", (1, simple [ IMPLICIT_ACCOUNT ])
  ; "Contract.address", (1, simple [ ADDRESS ])
  ; "Contract.self",   (0, simple [SELF])

  ; "Operation.transfer_tokens", (3, simple [ TRANSFER_TOKENS ])
  ; "Operation.set_delegate", (1, simple [ SET_DELEGATE ])

  ; "Global.get_now", (1, simple [ DROP 1; NOW ])
  ; "Global.get_amount", (1, simple [ DROP 1; AMOUNT ])
  ; "Global.get_balance", (1, simple [ DROP 1; BALANCE ])
  ; "Global.get_source", (1, simple [ DROP 1; SOURCE ])
  ; "Global.get_sender", (1, simple [ DROP 1; SENDER ])
  ; "Global.get_steps_to_quota", (1, simple [ DROP 1; STEPS_TO_QUOTA ])
  ; "Global.get_chain_id", (1, simple [ DROP 1; CHAIN_ID ])

  ; "Crypto.check_signature", (3, simple [ CHECK_SIGNATURE ])
  ; "Crypto.blake2b", (1, simple [ BLAKE2B ])
  ; "Crypto.sha256", (1, simple [ SHA256 ])
  ; "Crypto.sha512", (1, simple [ SHA512 ])
  ; "Crypto.hash_key", (1, simple [ HASH_KEY ])

  ; "Error.failwith", (1, simple [ FAILWITH ]) (* deprecated *)
  ; "failwith", (1, simple [ FAILWITH ])
                      
  ; "Timestamp.add", (2, simple [ADD])
  ; "Timestamp.sub", (2, simple [SUB])
  ; "Timestamp.diff", (2, simple [SUB])

  ; "ediv_int_int", (2, simple [EDIV])
  ; "ediv_int_nat", (2, simple [EDIV])
  ; "ediv_nat_int", (2, simple [EDIV])
  ; "ediv_nat_nat", (2, simple [EDIV])
  ; "ediv_tz_tz", (2, simple [EDIV])
  ; "ediv_tz_nat", (2, simple [EDIV])

  ; "/", (2, simple [EDIV; IF_NONE( [PUSH (tyInt, Int Z.zero); FAILWITH], 
                                    [CAR])])
  ; "/^", (2, simple [EDIV; IF_NONE( [PUSH (tyNat, Int Z.zero); FAILWITH], 
                                     [CAR])])
  ; "/$", (2, simple [EDIV; IF_NONE( [PUSH (tyMutez, Int Z.zero); FAILWITH], 
                                     [CAR])])
  ; "/$^", (2, simple [EDIV; IF_NONE( [PUSH (tyNat, Int Z.zero); FAILWITH], 
                                      [CAR])])
  ]
    
