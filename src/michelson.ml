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
  let bytes s = 
    Bytes (no_comment, Tezos_stdlib.MBytes.of_string & Hex.to_string (`Hex s))
  let int n = Int (no_comment, n)
  let prim s ts annots = Prim (no_comment, s, ts, annots)
  let seq ts = Seq (no_comment, ts)
      
  let pp = print_expr_unwrapped
end

module Type = struct
  (* Michelson type.  This is shamelessly used as types for IML, too. *)
  type t = { desc : desc 
           ; attrs : string list
           } 
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
    | TyChainID
  
    | TyKey
    | TySignature
    | TyOperation
    | TyContract of t
    | TyLambda of t * t
  
  let mk desc = { desc ; attrs= [] }

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
  let tyChainID             = mk TyChainID
  let tyKey                 = mk TyKey
  let tySignature           = mk TySignature
  let tyOperation           = mk TyOperation
  let tyContract t          = mk & TyContract t
  let tyLambda (t1, t2)     = mk & TyLambda (t1, t2)

  let rec to_micheline t = 
    let prim n args = Mline.prim n args t.attrs in
    let (!) x = prim x [] in
    match t.desc with
    | TyString -> !"string"
    | TyNat    -> !"nat"
    | TyInt    -> !"int"
    | TyBytes  -> !"bytes"
    | TyBool   -> !"bool"
    | TyUnit   -> !"unit"
    | TyList t -> prim "list" [to_micheline t]
    | TyPair (t1, t2)   -> prim "pair" [to_micheline t1; to_micheline t2]
    | TyOption t        -> prim "option" [to_micheline t]
    | TyOr (t1, t2)     -> prim "or" [to_micheline t1; to_micheline t2]
    | TySet t           -> prim "set" [to_micheline t]
    | TyMap (t1, t2)    -> prim "map" [to_micheline t1; to_micheline t2]
    | TyBigMap (t1, t2) -> prim "big_map" [to_micheline t1; to_micheline t2]
  
    | TyMutez     -> !"mutez"
    | TyKeyHash   -> !"key_hash"
    | TyTimestamp -> !"timestamp"
    | TyAddress   -> !"address"
    | TyChainID   -> !"chain_id"
  
    | TyKey       -> !"key"
    | TySignature -> !"signature"
    | TyOperation -> !"operation"
    | TyContract t -> prim "contract" [to_micheline t]
    | TyLambda (t1, t2) -> prim "lambda" [to_micheline t1; to_micheline t2]

  and pp fmt t = Mline.pp fmt & to_micheline t

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
    | Bool true  -> prim "True" [] []
    | Bool false -> prim "False" [] []
    | Unit     -> prim "Unit" [] []
    | Int n    -> int n
    | Nat n    -> int n
    (*    | Mutez n  -> f "%d" n *)
    | String s -> string s
    | Bytes s (* in hex *) ->  bytes s
    | Option None -> prim "None" [] []
    | Option (Some t) -> prim "Some" [to_micheline t] []
    | Pair (t1, t2) -> prim "Pair" [to_micheline t1; to_micheline t2] []
    | Left t -> prim "Left" [to_micheline t] []
    | Right t -> prim "Right" [to_micheline t] []
    | List ts -> seq (List.map to_micheline ts)
    | Set ts -> seq (List.map to_micheline & List.sort compare ts)
    | Map xs -> 
        seq (List.map (fun (k,v) ->
            prim "Elt" [to_micheline k; to_micheline v] []) xs)
    | Big_map xs -> 
        seq (List.map (fun (k,v) ->
            prim "Elt" [to_micheline k; to_micheline v] []) xs)
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
    | DIP of int * t list
    | DIG of int
    | DUG of int
    | DROP of int
    | SWAP
    | PAIR
    | ASSERT
    | CAR | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | APPLY
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
    | EMPTY_BIG_MAP of Type.t * Type.t
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
    | CREATE_ACCOUNT (* deprecated *)
    | CREATE_CONTRACT of module_
(*
    | CREATE_CONTRACT of t list

    :: Xkey_hash : Xoption key_hash : Xbool : Xbool : mutez : 'g : 'S
       -> operation : address : 'S

CREATE_CONTRACT - removed manager, delegate, delegatable and spendable arguments from the CREATE_CONTRACT instruction for newly created contracts. There is no change for existing contracts.

*)
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
    | CHAIN_ID

  and module_ = { parameter : Type.t ; storage : Type.t ; code : t list }
    
  let to_micheline t =
    let open Mline in
    let prim x args = Mline.prim x args [] in
    let (!) x = prim x [] in
    let rec f = function
      | DUP -> !"DUP"
      | DIP (1, code) -> prim "DIP" [seq (List.map f code)]
      | DIP (n, code) -> prim "DIP" [int & Z.of_int n; seq (List.map f code)]
      | DIG n -> prim "DIG" [int & Z.of_int n]
      | DUG n -> prim "DUG" [int & Z.of_int n]
      | SWAP -> !"SWAP"
      | PAIR -> !"PAIR"
      | PUSH (ty, const) -> prim "PUSH" [Type.to_micheline ty; Constant.to_micheline const]
      | ASSERT -> !"ASSERT"
      | CAR -> !"CAR"
      | CDR -> !"CDR" 
      | LEFT ty -> prim "LEFT" [Type.to_micheline ty]
      | RIGHT ty -> prim "RIGHT" [Type.to_micheline ty]
      | LAMBDA (ty1, ty2, code) -> 
          prim "LAMBDA" [Type.to_micheline ty1;
                         Type.to_micheline ty2;
                         seq (List.map f code)]
      | APPLY -> !"APPLY"
      | CONS -> !"CONS"
      | NIL ty -> prim "NIL" [ Type.to_micheline ty ]
      | SOME -> !"SOME"
      | NONE ty -> prim "NONE" [ Type.to_micheline ty ]
      | DROP 1 -> !"DROP"
      | DROP n -> prim "DROP" [int & Z.of_int n]
      | COMPARE -> !"COMPARE"
      | EQ  -> !"EQ"
      | LT  -> !"LT"
      | LE  -> !"LE"
      | GT  -> !"GT"
      | GE  -> !"GE"
      | NEQ -> !"NEQ"
      | IF (t,e) -> 
          prim "IF" [ seq & List.map f t;
                      seq & List.map f e ]

      | IF_NONE (t,e) -> 
          prim "IF_NONE" [ seq & List.map f t;
                           seq & List.map f e ]
      | ADD  -> !"ADD"
      | SUB  -> !"SUB"
      | MUL  -> !"MUL"
      | EDIV -> !"EDIV"
      | ABS  -> !"ABS"
      | NEG  -> !"NEG"
      | LSL  -> !"LSL"
      | LSR  -> !"LSR"
      | AND  -> !"AND"
      | OR   -> !"OR" 
      | XOR  -> !"XOR"
      | NOT  -> !"NOT"

      | EXEC -> !"EXEC"
      | FAILWITH -> !"FAILWITH"
      | COMMENT (s, ts) ->
          add_comment (Some s) & seq (List.map f ts)
      | IF_LEFT (t1, t2) ->
          prim "IF_LEFT" [ seq & List.map f t1;
                           seq & List.map f t2 ]
      | IF_CONS (t1, t2) ->
          prim "IF_CONS" [ seq & List.map f t1;
                           seq & List.map f t2 ]
      | UNIT -> !"UNIT"
      | EMPTY_SET ty -> prim "EMPTY_SET" [ Type.to_micheline ty ]
      | EMPTY_MAP (ty1, ty2) -> prim "EMPTY_MAP" [ Type.to_micheline ty1; Type.to_micheline ty2 ]
      | EMPTY_BIG_MAP (ty1, ty2) -> prim "EMPTY_BIG_MAP" [ Type.to_micheline ty1; Type.to_micheline ty2 ]
      | SIZE   -> !"SIZE"
      | MEM    -> !"MEM"
      | UPDATE -> !"UPDATE"
      | ITER code -> prim "ITER" [ seq & List.map f code ]
      | MAP code -> prim "MAP" [ seq & List.map f code ]
      | LOOP code -> prim "LOOP" [ seq & List.map f code ]
      | LOOP_LEFT code -> prim "LOOP_LEFT" [ seq & List.map f code ]
      | CONCAT -> !"CONCAT"
      | SELF   -> !"SELF"
      | GET    -> !"GET"
      | RENAME s -> prim "RENAME" [string s]
      | PACK -> !"PACK"
      | UNPACK ty -> prim "UNPACK" [Type.to_micheline ty]
      | SLICE -> !"SLICE"
      | CAST  -> !"CAST"
      | CONTRACT ty -> prim "CONTRACT" [Type.to_micheline ty]
      | TRANSFER_TOKENS  -> !"TRANSFER_TOKENS"
      | SET_DELEGATE     -> !"SET_DELEGATE"
      | CREATE_ACCOUNT   -> !"CREATE_ACCOUNT"
      | IMPLICIT_ACCOUNT -> !"IMPLICIT_ACCOUNT"
      | NOW              -> !"NOW"
      | AMOUNT           -> !"AMOUNT"
      | BALANCE          -> !"BALANCE"

      | CHECK_SIGNATURE -> !"CHECK_SIGNATURE"
      | BLAKE2B         -> !"BLAKE2B"
      | SHA256          -> !"SHA256"
      | SHA512          -> !"SHA512"
      | HASH_KEY        -> !"HASH_KEY"

      | STEPS_TO_QUOTA  -> !"STEPS_TO_QUOTA"
      | SOURCE          -> !"SOURCE"
      | SENDER          -> !"SENDER"
      | ADDRESS         -> !"ADDRESS"
      | CHAIN_ID        -> !"CHAIN_ID"
      | CREATE_CONTRACT m ->
          prim "CREATE_CONTRACT" 
            [ prim "parameter" [Type.to_micheline m.parameter]
            ; prim "storage" [Type.to_micheline m.storage]
            ; prim "code" (List.map f m.code  )
            ]
    in
    f t

  let pp ppf t = Mline.pp ppf & to_micheline t

  let pp_module ppf { parameter ; storage ; code } = 
    let open Mline in
    Format.fprintf ppf "%a ;@." Mline.pp & prim "parameter" [ Type.to_micheline parameter ] [];
    Format.fprintf ppf "%a ;@." Mline.pp & prim "storage" [ Type.to_micheline storage ] [];
    Format.fprintf ppf "%a ;@." Mline.pp & prim "code" [ seq (List.map to_micheline code) ] []

  let rec clean_failwith = function
    | [] -> []
    | FAILWITH::_ -> [FAILWITH]
    | x::xs -> aux x :: clean_failwith xs
  and aux = function
    | DIP (n, ts) -> DIP (n, clean_failwith ts)
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
    | CREATE_CONTRACT m -> CREATE_CONTRACT { m with code= clean_failwith m.code }
    | (DUP
      | DIG _ | DUG _ | DROP _
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
      | EMPTY_SET _ | EMPTY_MAP _ | EMPTY_BIG_MAP _
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
      | APPLY
      | CHAIN_ID
      as t) -> t
end

module Module = struct
  type t = Opcode.module_ = { parameter : Type.t ; storage : Type.t ; code : Opcode.t list }
           
(*
  let to_micheline { parameter ; storage ; code } =
    let open Mline in
    seq [ prim "parameter" [ Type.to_micheline parameter ]
        ; prim "storage" [ Type.to_micheline storage ]
        ; prim "code" (List.map Opcode.to_micheline code) 
        ] 
*)
      
  let pp = Opcode.pp_module
end