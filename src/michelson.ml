open Spotlib.Spot
open Tools

module Mline = struct
  open Tezos_micheline.Micheline
  open Tezos_micheline.Micheline_printer

  type t = node
    
  let no_comment = { comment = None }

  let add_comment c n = match c, n with
    | None, _ -> n
    | Some _, Int ({comment=None}, x) -> Int ({comment= c}, x)
    | Some _, String ({comment=None}, x) -> String ({comment= c}, x)
    | Some _, Bytes ({comment=None}, x) -> Bytes ({comment= c}, x)
    | Some _, Prim ({comment=None}, x, y, z) -> Prim ({comment= c}, x, y, z)
    | Some _, Seq ({comment=None}, x) -> Seq ({comment= c}, x)
    | Some s1, Int ({comment=Some s2}, x) -> 
        Int ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, String ({comment=Some s2}, x) ->
        String ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, Bytes ({comment=Some s2}, x) ->
        Bytes ({comment= Some (s1 ^ ", " ^ s2)}, x)
    | Some s1, Prim ({comment=Some s2}, x, y, z) ->
        Prim ({comment= Some (s1 ^ ", " ^ s2)}, x, y, z)
    | Some s1, Seq ({comment=Some s2}, x) -> 
        Seq ({comment= Some (s1 ^ ", " ^ s2)}, x)

  let string s = String (no_comment, s)
  let bytes s = String (no_comment, s)
  let int n = Int (no_comment, n)
  let prim s ts = Prim (no_comment, s, ts, [])
  let seq ts = Seq (no_comment, ts)
      
  let pp = print_expr
end

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

  let rec to_micheline t = 
    let prim = Mline.prim in
    match t.desc with
    | TyString -> prim "string" []
    | TyNat    -> prim "nat" []
    | TyInt    -> prim "int" []
    | TyBytes  -> prim "bytes" []
    | TyBool   -> prim "bool" []
    | TyUnit   -> prim "unit" []
    | TyList t -> prim "list" [to_micheline t]
    | TyPair (t1, t2) -> prim "pair" [to_micheline t1; to_micheline t2]
    | TyOption t -> prim "option" [to_micheline t]
    | TyOr (t1, t2) -> prim "or" [to_micheline t1; to_micheline t2]
    | TySet t -> prim "set" [to_micheline t]
    | TyMap (t1, t2) -> prim "map" [to_micheline t1; to_micheline t2]
    | TyBigMap (t1, t2) -> prim "bigmap" [to_micheline t1; to_micheline t2]
  
    | TyMutez     -> prim "mutez" []
    | TyKeyHash   -> prim "key_hash" []
    | TyTimestamp -> prim "timestamp" []
    | TyAddress   -> prim "address" []
  
    | TyKey       -> prim "key" []
    | TySignature -> prim "signature" []
    | TyOperation -> prim "operation" []
    | TyContract t -> prim "contract" [to_micheline t]
    | TyLambda (t1, t2, cli) -> 
        let comment =
          match (repr_closure_info cli).closure_desc with
          | CLLink _ -> assert false
          | CLEmpty -> Some "EMPTY!"
          | CLList [] -> None
          | CLList xs -> 
              Some (Format.sprintf "%a" 
                      Format.(list ";@ " (fun ppf (id, ty) ->
                          fprintf ppf "%s : %a" (Ident.name id) pp ty)) xs)
        in
        Mline.add_comment comment
        & prim "lambda" [to_micheline t1; to_micheline t2]

  and pp fmt t = Mline.pp fmt & to_micheline t

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

module Constant = struct
  type t = 
    | Unit
    | Bool of bool
    | Int of Z.t
    | Nat of Z.t
    | String of string
    | Bytes of string
    | Option of t option
    | List of t list
    | Set of t list
    | Map of (t * t) list
    | Big_map of (t * t) list
    | Pair of t * t
    | Left of t
    | Right of t
    | Timestamp of Z.t

  let rec to_micheline = 
    let open Mline in
    function
    | Bool true  -> prim "True" []
    | Bool false -> prim "False" []
    | Unit     -> prim "Unit" []
    | Int n    -> int n
    | Nat n    -> int n
    (*    | Mutez n  -> f "%d" n *)
    | String s -> string s
    | Bytes s (* in hex *) ->  bytes s
    | Option None -> prim "None" []
    | Option (Some t) -> prim "Some" [to_micheline t]
    | Pair (t1, t2) -> prim "Pair" [to_micheline t1; to_micheline t2]
    | Left t -> prim "Left" [to_micheline t]
    | Right t -> prim "Right" [to_micheline t]
    | List ts -> seq (List.map to_micheline ts)
    | Set ts -> seq (List.map to_micheline & List.sort compare ts)
    | Map xs -> 
        seq (List.map (fun (k,v) ->
            prim "Elt" [to_micheline k; to_micheline v]) xs)
    | Big_map xs -> 
        seq (List.map (fun (k,v) ->
            prim "Elt" [to_micheline k; to_micheline v]) xs)
    | Timestamp z -> 
        begin match Ptime.of_float_s @@ Z.to_float z with
          | None -> assert false
          | Some t -> string (Ptime.to_rfc3339 ~space:false ~frac_s:0 t)
        end

  let pp fmt t = Mline.pp fmt & to_micheline t
end

module Opcode = struct

  type constant = Constant.t

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
    
  let to_micheline t =
    let open Mline in
    let rec f = function
      | DUP -> prim "DUP" []
      | DIP code -> prim "DIP" [seq (List.map f code)]
      | SWAP -> prim "SWAP" []
      | PAIR -> prim "PAIR" []
      | PUSH (ty, const) -> prim "PUSH" [Type.to_micheline ty; Constant.to_micheline const]
      | ASSERT -> prim "ASSERT" []
      | CAR -> prim "CAR" []
      | CDR -> prim "CDR" [] 
      | LEFT ty -> prim "LEFT" [Type.to_micheline ty]
      | RIGHT ty -> prim "RIGHT" [Type.to_micheline ty]
      | LAMBDA (ty1, ty2, code) -> 
          prim "LAMBDA" [Type.to_micheline (Type.tyLambda (ty1, ty2, { Type.closure_desc= Type.CLEmpty })); (* XXX clist? *)
                         seq (List.map f code)]
      | CONS -> prim "CONS" []
      | NIL ty -> prim "NIL" [ Type.to_micheline ty ]
      | SOME -> prim "SOME" []
      | NONE ty -> prim "NONE" [ Type.to_micheline ty ]
      | DROP -> prim "DROP" []
      | COMPARE -> prim "COMPARE" []
      | EQ -> prim "EQ" []
      | LT -> prim "LT" []
      | LE -> prim "LE" []
      | GT -> prim "GT" []
      | GE -> prim "GE" []
      | NEQ -> prim "NEQ" []
      | IF (t,e) -> 
          prim "IF" [ seq & List.map f t;
                      seq & List.map f e ]

      | IF_NONE (t,e) -> 
          prim "IF_NONE" [ seq & List.map f t;
                           seq & List.map f e ]
      | ADD  -> prim "ADD" []
      | SUB  -> prim "SUB" []
      | MUL  -> prim "MUL" []
      | EDIV -> prim "EDIV" []
      | ABS  -> prim "ABS" []
      | NEG  -> prim "NEG" []
      | LSL  -> prim "LSL" []
      | LSR  -> prim "LSR" []
      | AND  -> prim "AND" []
      | OR   -> prim "OR"  []
      | XOR  -> prim "XOR" []
      | NOT  -> prim "NOT" []

      | EXEC -> prim "EXEC" []
      | FAILWITH -> prim "FAILWITH" []
      | COMMENT (s, ts) ->
          add_comment (Some s) & seq (List.map f ts)
      | IF_LEFT (t1, t2) ->
          prim "IF_LEFT" [ seq & List.map f t1;
                           seq & List.map f t2 ]
      | IF_CONS (t1, t2) ->
          prim "IF_CONS" [ seq & List.map f t1;
                           seq & List.map f t2 ]
      | UNIT -> prim "UNIT" []
      | EMPTY_SET ty -> prim "EMPTY_SET" [ Type.to_micheline ty ]
      | EMPTY_MAP (ty1, ty2) -> prim "EMPTY_MAP" [ Type.to_micheline ty1; Type.to_micheline ty2 ]
      | SIZE -> prim "SIZE" []
      | MEM -> prim "MEM" []
      | UPDATE -> prim "UPDATE" []
      | ITER code -> prim "ITER" [ seq & List.map f code ]
      | MAP code -> prim "MAP" [ seq & List.map f code ]
      | LOOP code -> prim "LOOP" [ seq & List.map f code ]
      | LOOP_LEFT code -> prim "LOOP_LEFT" [ seq & List.map f code ]
      | CONCAT -> prim "CONCAT" []
      | SELF -> prim "SELF" []
      | GET -> prim "GET" []
      | RENAME s -> prim "RENAME" [string s]
      | PACK -> prim "PACK" []
      | UNPACK ty -> prim "UNPACK" [Type.to_micheline ty]
      | SLICE -> prim "SLICE" []
      | CAST -> prim "CAST" []
      | CONTRACT ty -> prim "CONTRACT" [Type.to_micheline ty]
      | TRANSFER_TOKENS -> prim "TRANSFER_TOKENS" []
      | SET_DELEGATE -> prim "SET_DELEGATE" []
      | CREATE_ACCOUNT -> prim "CREATE_ACCOUNT" []
      | IMPLICIT_ACCOUNT -> prim "IMPLICIT_ACCOUNT" []
      | NOW -> prim "NOW" []
      | AMOUNT -> prim "AMOUNT" []
      | BALANCE -> prim "BALANCE" []
      | CHECK_SIGNATURE -> prim "CHECK_SIGNATURE" []
      | BLAKE2B         -> prim "BLAKE2B" []
      | SHA256          -> prim "SHA256" []
      | SHA512          -> prim "SHA512" []
      | HASH_KEY        -> prim "HASH_KEY" []
      | STEPS_TO_QUOTA  -> prim "STEPS_TO_QUOTA" []
      | SOURCE          -> prim "SOURCE" []
      | SENDER          -> prim "SENDER" []
      | ADDRESS         -> prim "ADDRESS" []
    in
    f t

  let pp fmt t = Mline.pp fmt & to_micheline t

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
           
  let to_micheline { parameter ; storage ; code } =
    let open Mline in
    seq ( prim "parameter" [ Type.to_micheline parameter ]
          ::  prim "storage" [ Type.to_micheline storage ]
          :: List.map Opcode.to_micheline code )
      
  let pp ppf t = Mline.pp ppf & to_micheline t
end
