type ('a, 'b) sum = Left of 'a | Right of 'b

type ('a, 'b) map = Map of ('a * 'b) list
type ('a, 'b) big_map = BigMap of ('a * 'b) list

type operation
type operations = operation list

type ocaml_int = int

type nat = Nat of ocaml_int
type int = Int of ocaml_int
type tz = Tz of float

type 'a set = Set of 'a list
module Set = struct
  let empty : 'a set = Set []
  let length (Set xs) = Nat (List.length xs)
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
  
