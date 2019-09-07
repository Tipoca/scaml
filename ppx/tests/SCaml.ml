type ('a, 'b) sum = Left of 'a | Right of 'b

type operation
type operations = operation list

type ocaml_int = int

type nat = Nat of ocaml_int
type int = Int of ocaml_int
type tz = TZ of float

let (+) : int -> int -> int = fun _ -> assert false
let (+^) : nat -> nat -> nat = fun _ -> assert false
let (+$) : tz -> tz -> tz = fun _ -> assert false

let (-) : int -> int -> int = fun _ -> assert false
let (-^) : nat -> nat -> int = fun _ -> assert false
let (-$) : tz -> tz -> tz = fun _ -> assert false

let fst = fst
let snd = snd  
let compare = compare
let (=) = (=)
let (<>) = (<>)
let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (&&) = (&&)

