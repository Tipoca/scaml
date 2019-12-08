[@@@SCaml iml_optimization=false]
open SCaml

type action = 
  | Vote of string * int
  | Init of nat
  | Foo of tz

let test () () =
  let x = Vote ("hello", Int 1) in
  [], ()
