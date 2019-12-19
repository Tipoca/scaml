(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2019  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The actual implementation of these API functions are given in
   primitives.ml as Michelson code.
*)

type ocaml_int = int

type nat = Nat of ocaml_int
(** Arbitrary length nat *)

type int = Int of ocaml_int
(** Arbitrary length int *)

type tz = Tz of float
(** Tezzies.  The smallest unit is micro tz, [Tz 0.000001]. *)

type ('a, 'b) sum = Left of 'a | Right of 'b
(** Basic sum type corresponds with Michelson's [or] type. *)

(** Arithmetics *)

val (+) : int -> int -> int
val (+^) : nat -> nat -> nat
val (+$) : tz -> tz -> tz

val (-) : int -> int -> int
val (-^) : nat -> nat -> int
val (-$) : tz -> tz -> tz

val ( * ) : int -> int -> int
val ( *^ ) : nat -> nat -> nat
val ( *$ ) : tz -> nat -> tz

val (~-) : int -> int
val (~-^) : nat -> int

val ediv_int_int : int -> int -> (int * nat) option
val ediv_int_nat : int -> nat -> (int * nat) option
val ediv_nat_int : nat -> int -> (int * nat) option
val ediv_nat_nat : nat -> nat -> (nat * nat) option
val ediv_tz_tz : tz -> tz -> (nat * tz) option
val ediv_tz_nat : tz -> nat -> (tz * tz) option

val (lsl) : nat -> nat -> nat
val (lsr) : nat -> nat -> nat
val (lor) : nat -> nat -> nat
val (land) : nat -> nat -> nat
val land_int_nat : int -> nat -> nat (* not a binop *) 
val (lxor) : nat -> nat -> nat
val lnot_nat : nat -> int (* not a binop *)
val lnot : int -> int (* not a binop *)

val abs : int -> nat
val isnat : int -> nat option

(** Comparisons 
    Just like OCaml they are fully polymorphic, 
    but they only work for limited set of Michelson types.
    
    Use Michelson type-checker to find invalid uses of comparisons
    for now.
*)

val compare : 'a -> 'a -> int
val (=)  : 'a -> 'a -> bool
val (<>) : 'a -> 'a -> bool
val (<)  : 'a -> 'a -> bool
val (<=) : 'a -> 'a -> bool
val (>)  : 'a -> 'a -> bool
val (>=) : 'a -> 'a -> bool

(** Logical operators *)
           
val (&&) : bool -> bool -> bool
val (||) : bool -> bool -> bool
val xor : bool -> bool -> bool
val not : bool -> bool

(** Tuples *)
  
val fst : ('a * 'b) -> 'a
val snd : ('a * 'b) -> 'b

(** Errors *)
module Error : sig
  val failwith : 'a -> 'b
  (** Deprecated.  Use [failwith] without [Error] *)
end

val failwith : 'a -> 'b
(** Fail the execution of the smart contract.
    You can also use [assert] 
*)

(** Loops *)
module Loop : sig
  val left : ('a -> ('a, 'b) sum) -> 'a -> 'b
  (** Keep calling the given function over values of type ['a]
      while the function returns [Left a].
      
      The loop stops when the function returns [Right b] and returns [b].
  *)
end
  
(** Data tyeps *)

(** Lists *)
module List : sig
  type 'a t = 'a list
  val length : 'a t -> nat
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
  val rev : 'a t -> 'a t
end

(** Sets 

    Set literal can be written using [Set [ x; .. ; x ]] expression.
*)
type 'a set = Set of 'a list

module Set : sig
  type 'a t = 'a set
  val empty : 'a t
  val length : 'a t -> nat
  val mem : 'a -> 'a t -> bool

  val update : 'a -> bool -> 'a t -> 'a t
  (** [update x b set] adds [x] when [b = true]
      and removes [x] when [b = false].
      
      Adding an element already exists in the set
      and removing an element  non existent in the set
      do not change the set.
  *)
      
  val fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc
end


(** Maps
    
    Map literal can be writen using [Map [ (k, v); .. ; (k, v) ]] expression.
*)

type ('k, 'v) map = Map of ('k * 'v) list

module Map : sig
  type ('k, 'v) t = ('k, 'v) map
  val empty : ('k, 'v) t
  val length : ('k, 'v) t -> nat
  val map : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
  (** [update k vopt map] adds [k=v] when [vopt = Some v]
      and removes [k] when [vopt = None].
      
      Adding a binding already exists in the set overrides
      the existing binding.
      
      Removing a binding non existent in the map does not change
      the map.
  *)

  val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
end

(** Big maps

    No way to write a literal of big maps.
*)

type ('k, 'v) big_map

module BigMap : sig
  type ('k, 'v) t = ('k, 'v) big_map
  val empty : ('k, 'v) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
  (** [update k vopt map] adds [k=v] when [vopt = Some v]
      and removes [k] when [vopt = None].
      
      Adding a binding already exists in the set overrides
      the existing binding.
      
      Removing a binding non existent in the map does not change
      the big map.
  *)
end

(** Strings *)

module String : sig
  val length : string -> nat
  val concat : string -> string -> string
  (* XXX (^) *)
  val slice : nat -> nat -> string -> string option
  (** Substring. [slice n1 n2 s] returns a substring of length [n2]
      from the position [n1] (zero based). 
  
      If the specified region by [n1] and [n2] exceeds the string [s],
      it returns [None].
  *)
end

(** Bytes 

    Bytes literals are written like [Bytes "0123456789abcdef"].
    The string must be even number of hex characters.
*)

type bytes = Bytes of string

module Bytes : sig
  type t = bytes
  val length : t -> nat
  val concat : t -> t -> t
  val slice : nat -> nat -> t -> t option
  (** Subbytes. [slice n1 n2 s] returns a subbytes of length [n2]
      from the position [n1] (zero based). 
  
      If the specified region by [n1] and [n2] exceeds the bytes [s],
      it returns [None].
  *)
end

(** Addresses *)
type address = Address of string

module Address : sig
  type t = address
end

(** Key hashes *)
type key_hash = Key_hash of string

module Key_hash : sig
  type t = key_hash
end

(** Contract, entry points, and operation *)

type 'a contract
type operation
type operations = operation list

type ('param, 'storage) entry = 'param -> 'storage -> operations * 'storage
  
(** Contracts *)
module Contract : sig
  type 'a t = 'a contract
  (** Contract whose parameter is ['a] *)

  val self : 'a t
  (** The contract of the code itself.  The type parameter of [self]
      must agree with the actual contract parameter.
      
      Unlike Michelson's [SELF] operator, [self] can appear inside a function.
      Even if the function value is sent to another contract, [self] still
      points to the original contract which uses [self] in its code.
  *)

  val contract : address -> 'a t option
  val implicit_account : key_hash -> unit t
  (** [tz1], [tz2], [tz3] accounts *)
      
  val address : 'a t -> address

  val create_from_tz_code : string -> key_hash option -> tz -> 'storage -> operation * address
  (** Raw interface for CREATE_CONTRACT.
  
      Michelson code must be given as a string LITERAL.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked 
      by SCaml.
  *)

  val create_raw : string -> key_hash option -> tz -> 'storage -> operation * address
  (** Same as [create_from_tz_code] *)

  val create_from_tz_file : string -> key_hash option -> tz -> 'storage -> operation * address
  (** CREATE_CONTRACT from a michelson source file.
  
      Michelson file name must be given as a string literal.
      In Tezos you cannot generate contract code programically in a contract.

      The types of the contract and the initial storage are NOT checked 
      by SCaml.
  *)
end

(** Operations *)
module Operation : sig
  type t = operation
  val transfer_tokens : 'a -> tz -> 'a contract -> t
  val set_delegate : key_hash option -> t
end

(** Timestamps 

    Timestamp literals are [Timestamp s] where [s] is a valid
    RFC3339 string. ex. [Timestamp "2019-09-11T08:30:23Z"].
*)
type timestamp = Timestamp of string

module Timestamp : sig
  type t = timestamp
  val add : t -> int -> t
  val sub : t -> int -> t
  val diff : t -> t -> int
end

(** Chain ids *)
type chain_id = Chain_id of string

module Chain_id : sig
  type t = chain_id
end

(** Global values 

    They are constants but have functional types in order to provide
    semantics in the native OCaml compilation in future.
*)
module Global : sig
  val get_now      : unit -> timestamp
  val get_amount   : unit -> tz
  val get_balance  : unit -> tz
  val get_source   : unit -> address
  val get_sender   : unit -> address
  val get_chain_id : unit -> chain_id
  val get_steps_to_quota : unit -> nat
end

(** Keys *)
type key = Key of string

module Key : sig
  type t = key
end

(** Signatures *)
type signature = Signature of string

module Signature : sig
  type t = signature
end

(** Cryptographic algorithms *)
module Crypto : sig
  val check_signature : key -> signature -> bytes -> bool
  val blake2b : bytes -> bytes
  val sha256 : bytes -> bytes
  val sha512 : bytes -> bytes
  val hash_key  : key -> key_hash
end

(** Serialization *)
module Obj : sig
  val pack : 'a -> bytes
  val unpack : bytes -> 'a option
end
