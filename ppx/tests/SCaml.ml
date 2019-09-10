type ('a, 'b) sum = Left of 'a | Right of 'b

type operation
type operations = operation list

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

let (lsl) : nat -> nat -> nat = fun _ -> assert false
let (lsr) : nat -> nat -> nat = fun _ -> assert false

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
  let concat : bytes -> bytes -> bytes = fun _ -> assert false
  let slice : nat -> nat -> bytes -> bytes option = fun _ -> assert false
end

module Contract : sig
  type 'a t
  val self : 'a t
end = struct
  type 'a t = Self
  let self = Self
end
type 'a contract = 'a Contract.t

module Obj = struct
  let pack : 'a -> bytes = fun _ -> assert false
  let unpack : bytes -> 'a option = fun _ -> assert false
end
