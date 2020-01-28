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

open Spotlib.Spot

module Mline : sig
  type t = Tezos_micheline.Micheline_printer.node
  val pp : Format.t -> t -> unit
end

module Type : sig
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
    | TySet of t
    | TyMap of t * t
    | TyBigMap of t * t
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

  val mk : desc -> t

  val tyString : t
  val tyNat : t
  val tyInt : t
  val tyBytes : t
  val tyBool : t
  val tyUnit : t
  val tyList : t -> t
  val tyPair : (t * t) -> t
  val tyOption : t -> t
  val tyOr : (t * t) -> t
  val tySet : t -> t
  val tyMap : (t * t) -> t
  val tyBigMap : (t * t) -> t
  val tyMutez : t
  val tyKeyHash : t
  val tyTimestamp : t
  val tyAddress : t
  val tyChainID : t
  val tyKey : t
  val tySignature : t
  val tyOperation : t
  val tyContract : t -> t
  val tyLambda : (t * t) -> t

  val pp : Format.formatter -> t -> unit
  val to_micheline : t -> Mline.t
                            
  val validate : t -> (unit, (t * string)) Result.t
  val is_comparable : t -> bool
  val is_packable : t -> bool
  val is_parameterable : t -> bool
    
  val attribute : string list -> t -> t
end

module rec Constant : sig
  type t =
    | Unit
    | Bool of bool
    | Int of Z.t
    | String of string
    | Bytes of string
    | Option of t option
    | List of t list
    | Set of t list
    | Map of (t * t) list
    | Pair of t * t
    | Left of t
    | Right of t
    | Timestamp of Z.t
    | Code of Opcode.t list

  val pp : Format.formatter -> t -> unit
  val to_micheline : t -> Mline.t
end

and Opcode : sig
  type module_ = 
    | Raw of Tezos_micheline.Micheline_printer.node list

  type t =
    | DUP
    | DIP of int * t list
    | DIG of int
    | DUG of int
    | DROP of int
    | SWAP
    | PAIR
    | ASSERT
    | CAR
    | CDR
    | LEFT of Type.t
    | RIGHT of Type.t
    | LAMBDA of Type.t * Type.t * t list
    | APPLY
    | PUSH of Type.t * Constant.t
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
    | ADD | SUB | MUL | EDIV | ABS | ISNAT | NEG | LSL | LSR 
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
    | CREATE_ACCOUNT
    | CREATE_CONTRACT of module_
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

  val pp : Format.formatter -> t -> unit
  val to_micheline : t -> Mline.t
  val clean_failwith : t list -> t list
end

module Module : sig
  type t = { parameter : Type.t; storage : Type.t; code : Opcode.t list; }
  val pp : Format.formatter -> t -> unit
end
