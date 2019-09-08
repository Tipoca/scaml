module Type : sig
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
    | TySet of t
    | TyMap of t * t
    | TyBigMap of t * t
    | TyMutez
    | TyKeyHash
    | TyTimestamp
    | TyAddress
    | TyKey
    | TySignature
    | TyOperation
    | TyContract of t
    | TyLambda of t * t * closure_info

  and closure_info = { mutable closure_desc : closure_desc; }

  and closure_desc =
    | CLEmpty (* never unified with a proper closure info! *)
    | CLList of (Ident.t * t) list
    | CLLink of closure_info

  val repr_closure_info : closure_info -> closure_info

  val pp : Format.formatter -> t -> unit

  val unify : t -> t -> t
end

module Opcode : sig
  type constant =
    | Unit
    | Bool of bool
    | Int of int
    | Nat of int
    (* | Mutez of int *)
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

  type t =
    | DUP
    | DIP of t list
    | DROP
    | SWAP
    | PAIR
    | ASSERT
    | CAR
    | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | PUSH of Type.t * constant
    | NIL of Type.t
    | CONS
    | NONE of Type.t
    | SOME
    | COMPARE
    | EQ
    | LT
    | LE
    | GT
    | GE
    | NEQ
    | IF of t list * t list
    | ADD | SUB | MUL | EDIV | ABS | NEG | LSL | LSR 
    | AND | OR | XOR | NOT
    | EXEC
    | IF_NONE of t list * t list
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAIL
    | COMMENT of string * t list
    | UNIT
    | EMPTY_SET of Type.t
    | SIZE
    | MEM
    | UPDATE

  val pp_constant : Format.formatter -> constant -> unit
  val pp : Format.formatter -> t -> unit
  val clean_fail : t list -> t list
end

module Module : sig
  type t = { parameter : Type.t; storage : Type.t; code : Opcode.t list; }
  val pp : Format.formatter -> t -> unit
end
