type ('a, 'b) sum = Left of 'a | Right of 'b

type ocaml_int = int

type nat = Nat of ocaml_int
type int = Int of ocaml_int
type tz = Tz of float

module Set = struct
  type 'a t = Set of 'a list
  let empty : 'a t = Set []
  let length (Set xs) = Nat (List.length xs)
  let mem : 'a -> 'a t -> bool = fun _ -> assert false 
  let update : 'a -> bool -> 'a t -> 'a t = fun _ -> assert false
  let fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc = fun _ -> assert false
end
type 'a set = 'a Set.t

module Map = struct
  type ('k, 'v) t = Map of ('k * 'v) list
  let empty : ('k, 'v) t = Map []
  let length : ('k, 'v) t -> nat = fun _ -> assert false
  let map : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t = fun _ -> assert false
  let get : 'k -> ('k, 'v) t -> 'k option = fun _ -> assert false
end
type ('k, 'v) map = ('k, 'v) Map.t

type ('a, 'b) big_map = BigMap of ('a * 'b) list

module Loop = struct
  let left : ('a -> ('a, 'b) sum) -> 'a -> 'b = fun _ -> assert false
end
  
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
  
module String = struct
  let concat : string -> string -> string = fun _ -> assert false
  let slice : nat -> nat -> string -> string option = fun _ -> assert false
end

module Bytes = struct
  type t = Bytes of string
  let concat : t -> t -> t = fun _ -> assert false
  let slice : nat -> nat -> t -> t option = fun _ -> assert false
  let length : t -> nat = fun _ -> assert false
end
type bytes = Bytes.t = Bytes of string

module Address = struct
  type t = Address of string
end
type address = Address.t = Address of string

module Key_hash = struct
  type t = Key_hash of string
end
type key_hash = Key_hash.t = Key_hash of string

module Contract : sig
  type 'a t
  val self : 'a t
  val contract : address -> 'a t option
  val implicit_account : key_hash -> unit t
  val address : 'a t -> address
end = struct
  type 'a t = Self
  let self = Self
  let contract = fun _ -> assert false
  let implicit_account = fun _ -> assert false
  let address _ = assert false
end
type 'a contract = 'a Contract.t

module Operation = struct
  type t
  let transfer_tokens : 'a -> tz -> 'a contract -> t = fun _ -> assert false
  let set_delegate : key_hash option -> t = fun _ -> assert false
  let create_account : key_hash -> key_hash option -> bool -> tz -> (t * address) = fun _ -> assert false
end

type operation = Operation.t
type operations = operation list

module Timestamp = struct
  type t = Timestamp of string
  let add : t -> int -> t = fun _ -> assert false
  let sub : t -> int -> t = fun _ -> assert false
  let diff : t -> t -> int = fun _ -> assert false
end
type timestamp = Timestamp.t = Timestamp of string

(* maybe the place is not good *)
module Global : sig
  val get_now : unit -> timestamp
  val get_amount : unit -> tz
  val get_balance : unit -> tz
  val get_source : unit -> address
  val get_sender : unit -> address
  val get_steps_to_quota : unit -> nat
end = struct
  let get_now _ = assert false
  let get_amount _ = assert false
  let get_balance _ = assert false
  let get_source _ = assert false
  let get_sender _ = assert false
  let get_steps_to_quota _ = assert false
end

module Key = struct
  type t = Key of string
end
type key = Key.t = Key of string

module Signature = struct
  type t = Signature of string
end
type signature = Signature.t = Signature of string

module Crypto = struct
  let check_signature : key -> signature -> bytes -> bool = fun _ -> assert false
  let blake2b : bytes -> bytes = fun _ -> assert false
  let sha256 : bytes -> bytes = fun _ -> assert false
  let sha512 : bytes -> bytes = fun _ -> assert false
  let hash_key  : key -> key_hash = fun _ -> assert false
(*
      | STEPS_TO_QUOTA
      | SOURCE
      | SENDER
      | ADDRESS
        *)
end

module Obj = struct
  let pack : 'a -> bytes = fun _ -> assert false
  let unpack : bytes -> 'a option = fun _ -> assert false
end

module Error = struct
  let failwith : 'a -> 'b = fun _ -> assert false
end
