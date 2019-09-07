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
      CLList of (Ident.t * t) list
    | CLLink of closure_info

  val repr_closure_info : closure_info -> closure_info

  val pp : Format.formatter -> t -> unit

  val unify : t -> t -> t
end

module Opcode : sig
  type constant =
    | True
    | False
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
    | ADD
    | SUB
    | AND
    | EXEC
    | IF_SOME of t list * t list
    | IF_LEFT of t list * t list
    | IF_CONS of t list * t list
    | FAIL
    | COMMENT of string * t list
    | UNIT
  val pp_constant : Format.formatter -> constant -> unit
  val pp : Format.formatter -> t -> unit
  val clean_fail : t list -> t list
end

module Module : sig
  type t = { parameter : Type.t; storage : Type.t; code : Opcode.t list; }
  val pp : Format.formatter -> t -> unit
end
