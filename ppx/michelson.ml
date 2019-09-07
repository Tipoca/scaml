open Spotlib.Spot
open Tools

module Type = struct
  (* Michelson type.  This is shamelessly used as types 
     for IML, too.
  *)
  type t = 
    | TyString
    | TyNat
    | TyInt
    | TyBytes
    | TyBool
    | TyUnit
    | TyList of t
    | TyPair of t * t
    | TyOption of t
    | TyOr of t * t
    | TySet of t (* comparable *)
    | TyMap of t (* comparable *) * t
    | TyBigMap of t (* comparable *) * t
  
    | TyMutez
    | TyKeyHash
    | TyTimestamp
    | TyAddress
  
    | TyKey
    | TySignature
    | TyOperation
    | TyContract of t
    | TyLambda of t * t * closure_info
                  (* If closure_info is non nil, the real Michelson type 
                     is not the lambda, but (lambda * env) *)
  
  and closure_info = { mutable closure_desc : closure_desc }
  and closure_desc = 
    | CLList of (Ident.t * t) list
    | CLLink of closure_info
  
  let rec repr_closure_info ({ closure_desc } as i) = 
    match closure_desc with
    | CLList _ -> i
    | CLLink cl -> repr_closure_info cl
  
  let rec pp ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | TyString -> p "string"
    | TyNat -> p "nat"
    | TyInt -> p "int"
    | TyBytes -> p "bytes"
    | TyBool -> p "bool"
    | TyUnit -> p "unit"
    | TyList t -> f "list (%a)" pp t
    | TyPair (t1, t2) -> f "pair (%a) (%a)" pp t1 pp t2
    | TyOption t -> f "option (%a)" pp t
    | TyOr (t1, t2) -> f "or (%a) (%a)" pp t1 pp t2
    | TySet t -> f "set (%a)" pp t
    | TyMap (t1, t2) -> f "map (%a) (%a)" pp t1 pp t2
    | TyBigMap (t1, t2) -> f "bigmap (%a) (%a)" pp t1 pp t2
  
    | TyMutez -> p "mutez"
    | TyKeyHash -> p "key_hash"
    | TyTimestamp -> p "timestamp"
    | TyAddress -> p "address"
  
    | TyKey -> p "key"
    | TySignature -> p "signature"
    | TyOperation -> p "operation"
    | TyContract t -> f "contract (%a)" pp t
    | TyLambda (t1, t2, cli) -> 
        f "@[<2>lambda (%a%a) (%a)@]" 
          pp t1 
          (fun _ppf cli ->
             match (repr_closure_info cli).closure_desc with
             | CLLink _ -> assert false
             | CLList [] -> ()
             | CLList xs -> 
                 f " /* %a */"
                   Format.(list ";@ " (fun ppf (id, ty) ->
                       fprintf ppf "%s : %a" (Ident.name id) pp ty)) xs) cli
          pp t2
  
  let rec merge env1 env2 =
    let ids = List.map fst env1 @ List.map fst env2 in
    let ids = List.sort_uniq compare ids in
    List.map (fun id ->
        (id,
         match List.assoc_opt id env1, List.assoc_opt id env2 with
         | None, None -> assert false
         | Some ty, None -> ty
         | None, Some ty -> ty
         | Some ty1, Some ty2 -> unify ty1 ty2)) ids
  
  and unify t1 t2 = 
    if t1 == t2 then t1
    else match t1, t2 with
      | TyList t1, TyList t2 -> TyList (unify t1 t2)
      | TyPair (t11, t12), TyPair (t21, t22) ->
          TyPair (unify t11 t21, unify t12 t22)
      | TyOption t1, TyOption t2 -> TyOption (unify t1 t2)
      | TyOr (t11, t12), TyOr (t21, t22) ->
          TyOr (unify t11 t21, unify t12 t22)
      | TySet t1, TySet t2 -> TySet (unify t1 t2)
      | TyMap (t11, t12), TyMap (t21, t22) ->
          TyMap (unify t11 t21, unify t12 t22)
      | TyBigMap (t11, t12), TyBigMap (t21, t22) ->
          TyBigMap (unify t11 t21, unify t12 t22)
      | TyContract t1, TyContract t2 ->  TyContract (unify t1 t2)
      | TyLambda (t11, t12, cli1), TyLambda (t21, t22, cli2) ->
          let cli1 = repr_closure_info cli1 in
          let cli2 = repr_closure_info cli2 in
          let get_list x = match x.closure_desc with CLList xs -> xs | _ -> assert false in
          let env = merge (get_list cli1) (get_list cli2) in
          let newcli = { closure_desc= CLList env } in
          cli1.closure_desc <- CLLink newcli;
          cli2.closure_desc <- CLLink newcli;
          TyLambda (unify t11 t21, unify t12 t22, cli1)
      | _ -> 
          Format.eprintf "Unifying %a and %a!!@." pp t1 pp t2;
          assert false
end

module Opcode = struct
  type constant = 
    | True | False
    | Unit
    | Int of int
    | Nat of int
    | String of string

  type t = 
    | DUP
    | DIP of t list
    | DROP
    | SWAP
    | PAIR
    | ASSERT
    | CAR | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | PUSH of Type.t * constant
    | NIL of Type.t
    | CONS
    | NONE of Type.t
    | SOME
    | COMPARE
    | EQ | LT | LE | GT | GE | NEQ
    | IF of t list * t list
    | ADD
    | SUB
    | AND
    | EXEC
    | IF_SOME of t list * t list (* or IF_NONE *)
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAIL (* FAILWITH ? *)
    | COMMENT of string * t list
    | UNIT

(*
    | SIZE
    | EMPTY_SET of Type.t
    | EMPTY_MAP of Type.t * Type.t
    | MAP of t list
    | ITER of t list
    | MEM
    | GET
    | UPDATE
    | LOOP of t list
    | LOOP_LEFT of t list
    | CAST
    | RENAME
    | CONCAT
    | SLICE
    | PACK
    | UNPACK
    | MUL
    | EDIV
    | ABS
    | NEG
    | LSL
    | LSR
    | OR
    | AND
    | XOR
    | NOT
    | COMPARE
    | SELF
    | CONTRACT of Type.t
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT
    | CREATE_CONTRACT of t list
    | IMPLICIT_ACCOUNT
    | NOW
    | AMOUNT
    | BALANCE
    | CHECK_SIGNATURE
    | BLAKE2B
    | SHA256
    | SHA512
    | HASH_KEY
    | STEPS_TO_QUOTA
    | SOURCE
    | SENDER
    | ADDRESS
*)
                              
    
  let pp_constant ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | True     -> p "True"
    | False    -> p "False"
    | Unit     -> p "Unit"
    | Int n    -> f "%d" n
    | Nat n    -> f "%d" n
    | String s -> f "%S" s

  let rec pp ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | DUP -> p "DUP"
    | DIP code -> f "DIP @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | SWAP -> p "SWAP"
    | PAIR -> p "PAIR"
    | PUSH (ty, const) -> f "PUSH (%a) %a" Type.pp ty pp_constant const
    | ASSERT -> p "ASSERT"
    | CAR -> p "CAR"
    | CDR -> p "CDR"
    | LEFT ty -> f "LEFT (%a)" Type.pp ty
    | RIGHT ty -> f "RIGHT (%a)" Type.pp ty
    | LAMBDA (ty1, ty2, code) -> f "@[<2>LAMBDA @[(%a) (%a)@ @[<2>{ %a }@]@]@]" Type.pp ty1 Type.pp ty2 (Format.list " ;@ " pp) code
    | CONS -> p "CONS"
    | NIL ty -> f "NIL (%a)" Type.pp ty
    | SOME -> p "SOME"
    | NONE ty -> f "NONE (%a)" Type.pp ty
    | DROP -> p "DROP"
    | COMPARE -> p "COMPARE"
    | EQ -> p "EQ"
    | LT -> p "LT"
    | LE -> p "LE"
    | GT -> p "GT"
    | GE -> p "GE"
    | NEQ -> p "NEQ"
    | IF (t,e) -> f "IF @[<0>{ @[%a@] }@ { @[%a@] }@]" 
                    (Format.list " ;@ " pp) t
                    (Format.list " ;@ " pp) e

    | IF_SOME (t,e) -> f "IF_SOME @[<0>{ @[%a@] }@ { @[%a@] }@]" 
                         (Format.list " ;@ " pp) t
                         (Format.list " ;@ " pp) e
    | ADD -> p "ADD"
    | SUB -> p "SUB"
    | AND -> p "AND"
    | EXEC -> p "EXEC"
    | FAIL -> p "FAIL"
    | COMMENT (s, ts) ->
        f "{ @[/* %s */@ @[%a@]@] }" s (Format.list " ;@ " pp) ts
    | IF_LEFT (t1, t2) ->
        f "IF_LEFT @[<0>{ @[%a@] }@ { @[%a@] }@]" 
          (Format.list " ;@ " pp) t1
          (Format.list " ;@ " pp) t2
    | IF_CONS (t1, t2) ->
        f "IF_CONS @[<0>{ @[%a@] }@ { @[%a@] }@]" 
          (Format.list " ;@ " pp) t1
          (Format.list " ;@ " pp) t2
    | UNIT -> p "UNIT"
          
  let rec clean_fail = function
    | [] -> []
    | FAIL::_ -> [FAIL]
    | x::xs -> aux x :: clean_fail xs
  and aux = function
    | DIP ts -> DIP (clean_fail ts)
    | LAMBDA (ty1, ty2, ts) -> LAMBDA (ty1, ty2, clean_fail ts)
    | IF (t1, t2) -> IF (clean_fail t1, clean_fail t2)
    | IF_SOME (t1, t2) -> IF_SOME (clean_fail t1, clean_fail t2)
    | IF_LEFT (t1, t2) -> IF_LEFT (clean_fail t1, clean_fail t2)
    | IF_CONS (t1, t2) -> IF_CONS (clean_fail t1, clean_fail t2)
    | COMMENT (s, t) -> COMMENT (s, clean_fail t)
    | (DUP
      | DROP
      | SWAP
      | PAIR
      | ASSERT
      | CAR | CDR
      | LEFT _ | RIGHT _
      | PUSH _
      | NIL _
      | CONS
      | NONE _
      | SOME
      | COMPARE
      | EQ | LT | LE | GT | GE | NEQ
      | ADD
      | SUB
      | AND
      | EXEC
      | FAIL 
      | UNIT as t) -> t
end

module Module = struct
  type t = { parameter : Type.t ; storage : Type.t ; code : Opcode.t list }
           
  let pp ppf { parameter ; storage ; code } =
    Format.fprintf ppf "@[<2>{ parameter (%a) ;@ storage (%a) ;@ code @[<2>{ %a }@] }@]"
      Type.pp parameter 
      Type.pp storage
      (Format.list ";@ " Opcode.pp ) code
end



