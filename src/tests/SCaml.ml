(* The actual implementation of these API functions are given in
   primitives.ml as Michelson code.  This file is to give their ML types.
*)

type ocaml_int = int
type nat = Nat of ocaml_int
type int = Int of ocaml_int
type tz = Tz of float

type ('a, 'b) sum = Left of 'a | Right of 'b

let (+) : int -> int -> int = fun _ -> assert false
let (+^) : nat -> nat -> nat = fun _ -> assert false
let (+$) : tz -> tz -> tz = fun _ -> assert false

let (-) : int -> int -> int = fun _ -> assert false
let (-^) : nat -> nat -> int = fun _ -> assert false
let (-$) : tz -> tz -> tz = fun _ -> assert false

let ( * ) : int -> int -> int = fun _ -> assert false
let ( *^ ) : nat -> nat -> nat = fun _ -> assert false
let ( *$ ) : tz -> nat -> tz = fun _ -> assert false

let (~-) : int -> int = fun _ -> assert false
let (~-^) : nat -> int = fun _ -> assert false

let ediv_int_int : int -> int -> (int * nat) option = fun _ -> assert false
let ediv_int_nat : int -> nat -> (int * nat) option = fun _ -> assert false
let ediv_nat_int : nat -> int -> (int * nat) option = fun _ -> assert false
let ediv_nat_nat : nat -> nat -> (nat * nat) option = fun _ -> assert false
let ediv_tz_tz : tz -> tz -> (nat * tz) option = fun _ -> assert false
let ediv_tz_nat : tz -> nat -> (tz * tz) option = fun _ -> assert false

let (lsl) : nat -> nat -> nat = fun _ -> assert false
let (lsr) : nat -> nat -> nat = fun _ -> assert false
let (lor) : nat -> nat -> nat = fun _ -> assert false
let (land) : nat -> nat -> nat = fun _ -> assert false
let land_int_nat : int -> nat -> nat = fun _ -> assert false
let (lxor) : nat -> nat -> nat = fun _ -> assert false
let lnot_nat : nat -> int = fun _ -> assert false
let (lnot) : int -> int = fun _ -> assert false

let abs : int -> nat = fun _ -> assert false
let isnat : int -> nat option = fun _ -> assert false

let fst = fst
let snd = snd  
let compare : 'a -> 'a -> int = fun _ -> assert false
let (=) = (=)
let (<>) = (<>)
let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (&&) = (&&)
let (||) = (||)
let xor : bool -> bool -> bool = fun _ -> assert false
let not = not
  
module Error = struct
  let failwith : 'a -> 'b = fun _ -> assert false
end

module List = struct
  type 'a t = 'a list
  let length : 'a t -> nat = fun _ -> assert false
  let map : ('a -> 'b) -> 'a t -> 'b t = fun _ -> assert false
  let fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc = fun _ -> assert false
  let rev : 'a t -> 'a t = fun _ -> assert false
end

type 'a set = Set of 'a list

module Set = struct
  type 'a t = 'a set
  let empty : 'a t = Set []
  let length (Set xs) = Nat (Stdlib.List.length xs)
  let mem : 'a -> 'a t -> bool = fun _ -> assert false 
  let update : 'a -> bool -> 'a t -> 'a t = fun _ -> assert false
  let fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc = fun _ -> assert false
end

type ('k, 'v) map = Map of ('k * 'v) list

module Map = struct
  type ('k, 'v) t = ('k, 'v) map
  let empty : ('k, 'v) t = Map []
  let length : ('k, 'v) t -> nat = fun _ -> assert false
  let map : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t = fun _ -> assert false
  let get : 'k -> ('k, 'v) t -> 'v option = fun _ -> assert false
  let mem : 'k -> ('k, 'v) t -> bool = fun _ -> assert false
  let update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t = fun _ -> assert false
  let fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc = fun _ -> assert false
end

type ('k, 'v) big_map

module BigMap : sig
  type ('k, 'v) t = ('k, 'v) big_map
  val empty : ('k, 'v) t
  val get : 'k -> ('k, 'v) t -> 'v option
  val mem : 'k -> ('k, 'v) t -> bool
  val update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t
end = struct
  type ('k, 'v) t = ('k, 'v) big_map
  let empty : ('k, 'v) t = assert false
  let get : 'k -> ('k, 'v) t -> 'v option = fun _ -> assert false
  let mem : 'k -> ('k, 'v) t -> bool = fun _ -> assert false
  let update : 'k -> 'v option -> ('k, 'v) t -> ('k, 'v) t = fun _ -> assert false
end

module Loop = struct
  let left : ('a -> ('a, 'b) sum) -> 'a -> 'b = fun _ -> assert false
end
  
module String = struct
  let concat : string -> string -> string = fun _ -> assert false
  let slice : nat -> nat -> string -> string option = fun _ -> assert false
  let length : string -> nat = fun _ -> assert false
end

type bytes = Bytes of string

module Bytes = struct
  type t = bytes
  let concat : t -> t -> t = fun _ -> assert false
  let slice : nat -> nat -> t -> t option = fun _ -> assert false
  let length : t -> nat = fun _ -> assert false
end

type address = Address of string
module Address = struct
  type t = address
end

type key_hash = Key_hash of string
module Key_hash = struct
  type t = key_hash
end

type 'a contract
type operation
type operations = operation list

module Contract : sig
  type 'a t = 'a contract
  val self : 'a t
  val contract : address -> 'a t option
  val implicit_account : key_hash -> unit t
  val address : 'a t -> address

  val create_raw : string -> key_hash option -> tz -> 'storage -> operation * address
  (** Very raw interface for CREATE_CONTRACT.
  
      Michelson code must be given as a string literal.
      The types of the contract and the initial storage are not checked 
      by SCaml.
      
      Note that the Michelson code must be in string LITERAL.  
      In Tezos you cannot generate contract code programically in a contract.
  *)
end = struct
  type 'a t = 'a contract
  let self = assert false
  let contract = fun _ -> assert false
  let implicit_account = fun _ -> assert false
  let address _ = assert false
  let create_raw _ = assert false
end

module Operation = struct
  type t = operation
  let transfer_tokens : 'a -> tz -> 'a contract -> t = fun _ -> assert false
  let set_delegate : key_hash option -> t = fun _ -> assert false
end

type timestamp = Timestamp of string

module Timestamp = struct
  type t = timestamp
  let add : t -> int -> t = fun _ -> assert false
  let sub : t -> int -> t = fun _ -> assert false
  let diff : t -> t -> int = fun _ -> assert false
end

type chain_id = Chain_id of string

module Chain_id = struct
  type t = chain_id
end

(* maybe the place is not good *)
module Global : sig
  val get_now : unit -> timestamp
  val get_amount : unit -> tz
  val get_balance : unit -> tz
  val get_source : unit -> address
  val get_sender : unit -> address
  val get_steps_to_quota : unit -> nat
  val get_chain_id : unit -> chain_id
end = struct
  let get_now _ = assert false
  let get_amount _ = assert false
  let get_balance _ = assert false
  let get_source _ = assert false
  let get_sender _ = assert false
  let get_steps_to_quota _ = assert false
  let get_chain_id _ = assert false
end

type key = Key of string

module Key = struct
  type t = key
end

type signature = Signature of string

module Signature = struct
  type t = signature
end

module Crypto = struct
  let check_signature : key -> signature -> bytes -> bool = fun _ -> assert false
  let blake2b : bytes -> bytes = fun _ -> assert false
  let sha256 : bytes -> bytes = fun _ -> assert false
  let sha512 : bytes -> bytes = fun _ -> assert false
  let hash_key  : key -> key_hash = fun _ -> assert false
end

module Obj = struct
  let pack : 'a -> bytes = fun _ -> assert false
  let unpack : bytes -> 'a option = fun _ -> assert false
end

