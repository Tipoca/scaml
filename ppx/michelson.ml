open Spotlib.Spot
open Tools

module Type = struct
  (* Michelson type.  This is shamelessly used as types for IML, too. *)
  type t = { desc : desc } 
  and desc = 
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
    | CLEmpty (* never unified with a proper closure info! *)
    | CLList of (Ident.t * t) list
    | CLLink of closure_info

  let mk desc = { desc }

  let tyString              = mk TyString
  let tyNat                 = mk TyNat
  let tyInt                 = mk TyInt
  let tyBytes               = mk TyBytes
  let tyBool                = mk TyBool
  let tyUnit                = mk TyUnit
  let tyList t              = mk & TyList t
  let tyPair (t1, t2)       = mk & TyPair (t1, t2)
  let tyOption t            = mk & TyOption t
  let tyOr (t1, t2)         = mk & TyOr (t1, t2)
  let tySet t               = mk & TySet t
  let tyMap (t1, t2)        = mk & TyMap (t1, t2)
  let tyBigMap (t1, t2)     = mk & TyBigMap (t1, t2)
  let tyMutez               = mk TyMutez
  let tyKeyHash             = mk TyKeyHash
  let tyTimestamp           = mk TyTimestamp
  let tyAddress             = mk TyAddress
  let tyKey                 = mk TyKey
  let tySignature           = mk TySignature
  let tyOperation           = mk TyOperation
  let tyContract t          = mk & TyContract t
  let tyLambda (t1, t2, ci) = mk & TyLambda (t1, t2, ci)

  let rec repr_closure_info ({ closure_desc } as i) = 
    match closure_desc with
    | CLEmpty -> i
    | CLList _ -> i
    | CLLink cl -> repr_closure_info cl
  
  let rec pp ppf t =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    match t.desc with
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
        f "lambda (%a%a) (%a)" 
          pp t1 
          (fun _ppf cli ->
             match (repr_closure_info cli).closure_desc with
             | CLLink _ -> assert false
             | CLEmpty -> p " /* EMPTY! */"
             | CLList [] -> ()
             | CLList xs -> 
                 f " /* %a */"
                   Format.(list ";@ " (fun ppf (id, ty) ->
                       fprintf ppf "%s : %a" (Ident.name id) pp ty)) xs) cli
          pp t2

  exception Unification_error of t * t

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
    if t1.desc == t2.desc then t1
    else match t1.desc, t2.desc with
      | TyList t1, TyList t2 -> tyList (unify t1 t2)
      | TyPair (t11, t12), TyPair (t21, t22) ->
          tyPair (unify t11 t21, unify t12 t22)
      | TyOption t1, TyOption t2 -> tyOption (unify t1 t2)
      | TyOr (t11, t12), TyOr (t21, t22) ->
          tyOr (unify t11 t21, unify t12 t22)
      | TySet t1, TySet t2 -> tySet (unify t1 t2)
      | TyMap (t11, t12), TyMap (t21, t22) ->
          tyMap (unify t11 t21, unify t12 t22)
      | TyBigMap (t11, t12), TyBigMap (t21, t22) ->
          tyBigMap (unify t11 t21, unify t12 t22)
      | TyContract t1, TyContract t2 ->  tyContract (unify t1 t2)
      | TyLambda (t11, t12, cli1), TyLambda (t21, t22, cli2) ->
          let cli1 = repr_closure_info cli1 in
          let cli2 = repr_closure_info cli2 in
          begin match cli1.closure_desc, cli2.closure_desc with
            | CLLink _, _ | _, CLLink _ -> assert false
            | CLEmpty, CLEmpty -> 
                cli2.closure_desc <- CLLink cli1
            | CLEmpty, _ ->
                cli1.closure_desc <- CLLink cli2
            | _, CLEmpty ->
                cli2.closure_desc <- CLLink cli1
            | CLList env1, CLList env2 ->
                let env = merge env1 env2 in
                let newcli = { closure_desc= CLList env } in
                cli1.closure_desc <- CLLink newcli;
                cli2.closure_desc <- CLLink newcli
          end;
          tyLambda (unify t11 t21, unify t12 t22, cli1)
      | _ -> 
          raise (Unification_error (t1,t2))
end

module Opcode = struct
  type constant = 
    | Unit
    | Bool of bool
    | Int of Z.t
    | Nat of Z.t
    | String of string
    | Bytes of string
    | Option of constant option
    | List of constant list
    | Set of constant list
    | Map of (constant * constant) list
    | Big_map of (constant * constant) list
    | Pair of constant * constant
    | Left of constant
    | Right of constant
    | Timestamp of Z.t

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
    | ADD | SUB | MUL | EDIV | ABS | NEG | LSL | LSR 
    | AND | OR | XOR | NOT
    | EXEC
    | IF_NONE of t list * t list
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAILWITH
    | COMMENT of string * t list
    | UNIT
    | EMPTY_SET of Type.t
    | EMPTY_MAP of Type.t * Type.t
    | SIZE
    | MEM
    | UPDATE
    | ITER of t list
    | MAP of t list
    | LOOP of t list (* It is not really useful for SCaml *)
    | LOOP_LEFT of t list 
    | CONCAT
    | SELF
    | GET
    | RENAME of string (* for debugging *)
    | PACK
    | UNPACK of Type.t
    | SLICE
    | CAST (* to remove type name. *)
    | CONTRACT of Type.t
    | TRANSFER_TOKENS
    | SET_DELEGATE
    | CREATE_ACCOUNT
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
(*
    | CREATE_CONTRACT of t list
*)
    
  let rec pp_constant ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | Bool true  -> p "True"
    | Bool false -> p "False"
    | Unit     -> p "Unit"
    | Int n    -> Z.pp_print ppf n
    | Nat n    -> Z.pp_print ppf n
    (*    | Mutez n  -> f "%d" n *)
    | String s -> f "%S" s
    | Bytes s (* in hex *) ->  f "0x%s" s
    | Option None -> p "None"
    | Option (Some t) -> f "Some (%a)" pp_constant t
    | Pair (t1, t2) -> f "(Pair (%a) (%a))" pp_constant t1 pp_constant t2
    | Left t -> f "(Left (%a))" pp_constant t
    | Right t -> f "(Right (%a))" pp_constant t
    | List ts -> f "{ %a }" (list " ; " pp_constant) ts
    | Set ts -> f "{ %a }" (list " ; " pp_constant) (List.sort compare ts)
    | Map xs -> f "{ %a }" (list " ; " (fun ppf (x,y) -> fprintf ppf "Elt %a %a" pp_constant x pp_constant y)) (List.sort (fun (k1,_) (k2,_) -> compare k1 k2) xs)
    | Big_map xs -> f "{ %a }" (list " ; " (fun ppf (x,y) -> fprintf ppf "Elt %a %a" pp_constant x pp_constant y)) (List.sort (fun (k1,_) (k2,_) -> compare k1 k2) xs)
    | Timestamp z -> 
        begin match Ptime.of_float_s @@ Z.to_float z with
          | None -> assert false
          | Some t -> f "\"%s\"" (Ptime.to_rfc3339 ~space:false ~frac_s:0 t)
        end

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
    | LAMBDA (ty1, ty2, code) -> 
        f "@[<v2>LAMBDA@ (%a)@ (%a)@ @[<2>{ %a }@]@]" 
          Type.pp ty1 Type.pp ty2 (Format.list " ;@ " pp) code
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

    | IF_NONE (t,e) -> f "IF_NONE @[<0>{ @[%a@] }@ { @[%a@] }@]" 
                         (Format.list " ;@ " pp) t
                         (Format.list " ;@ " pp) e
    | ADD  -> p "ADD"
    | SUB  -> p "SUB"
    | MUL  -> p "MUL"
    | EDIV -> p "EDIV"
    | ABS  -> p "ABS"
    | NEG  -> p "NEG"
    | LSL  -> p "LSL"
    | LSR  -> p "LSR"
    | AND  -> p "AND"
    | OR   -> p "OR"
    | XOR  -> p "XOR"
    | NOT  -> p "NOT"

    | EXEC -> p "EXEC"
    | FAILWITH -> p "FAILWITH"
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
    | EMPTY_SET ty -> f "EMPTY_SET (%a)" Type.pp ty
    | EMPTY_MAP (ty1, ty2) -> f "EMPTY_MAP (%a) (%a)" Type.pp ty1 Type.pp ty2
    | SIZE -> p "SIZE"
    | MEM -> p "MEM"
    | UPDATE -> p "UPDATE"
    | ITER code -> f "ITER @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | MAP code -> f "MAP @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | LOOP code -> f "LOOP @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | LOOP_LEFT code -> f "LOOP_LEFT @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | CONCAT -> p "CONCAT"
    | SELF -> p "SELF"
    | GET -> p "GET"
    | RENAME s -> f "RENAME @%s" s
    | PACK -> p "PACK"
    | UNPACK ty -> f "UNPACK (%a)" Type.pp ty
    | SLICE -> p "SLICE"
    | CAST -> p "CAST"
    | CONTRACT ty -> f "CONTRACT (%a)" Type.pp ty
    | TRANSFER_TOKENS -> p "TRANSFER_TOKENS"
    | SET_DELEGATE -> p "SET_DELEGATE"
    | CREATE_ACCOUNT -> p "CREATE_ACCOUNT"
    | IMPLICIT_ACCOUNT -> p "IMPLICIT_ACCOUNT"
    | NOW -> p "NOW"
    | AMOUNT -> p "AMOUNT"
    | BALANCE -> p "BALANCE"
    | CHECK_SIGNATURE -> p "CHECK_SIGNATURE"
    | BLAKE2B -> p "BLAKE2B"
    | SHA256 -> p "SHA256"
    | SHA512 -> p "SHA512"
    | HASH_KEY -> p "HASH_KEY"
    | STEPS_TO_QUOTA -> p "STEPS_TO_QUOTA"
    | SOURCE -> p "SOURCE"
    | SENDER -> p "SENDER"
    | ADDRESS -> p "ADDRESS"

  let rec clean_failwith = function
    | [] -> []
    | FAILWITH::_ -> [FAILWITH]
    | x::xs -> aux x :: clean_failwith xs
  and aux = function
    | DIP ts -> DIP (clean_failwith ts)
    | ITER ts -> ITER (clean_failwith ts)
    | MAP ts -> MAP (clean_failwith ts)
    | LAMBDA (ty1, ty2, ts) -> LAMBDA (ty1, ty2, clean_failwith ts)
    | IF (t1, t2) -> IF (clean_failwith t1, clean_failwith t2)
    | IF_NONE (t1, t2) -> IF_NONE (clean_failwith t1, clean_failwith t2)
    | IF_LEFT (t1, t2) -> IF_LEFT (clean_failwith t1, clean_failwith t2)
    | IF_CONS (t1, t2) -> IF_CONS (clean_failwith t1, clean_failwith t2)
    | COMMENT (s, t) -> COMMENT (s, clean_failwith t)
    | LOOP t -> LOOP (clean_failwith t)
    | LOOP_LEFT t -> LOOP_LEFT (clean_failwith t)
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
      | ADD | SUB | MUL | EDIV | ABS | NEG | LSL | LSR
      | AND | OR | XOR | NOT 
      | EXEC
      | FAILWITH
      | UNIT 
      | EMPTY_SET _ | EMPTY_MAP _
      | SIZE
      | MEM
      | UPDATE
      | CONCAT
      | SELF
      | GET
      | RENAME _
      | PACK | UNPACK _
      | SLICE
      | CAST

      | CONTRACT _
      | TRANSFER_TOKENS
      | SET_DELEGATE
      | CREATE_ACCOUNT
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

      as t) -> t
end

module Module = struct
  type t = { parameter : Type.t ; storage : Type.t ; code : Opcode.t list }
           
  let pp ppf { parameter ; storage ; code } =
    Format.fprintf ppf "@[<2>{ parameter (%a) ;@ storage (%a) ;@ code @[<2>{ %a }@] }@]"
      Type.pp parameter 
      Type.pp storage
      (Format.list ";@ " Opcode.pp ) code
end
