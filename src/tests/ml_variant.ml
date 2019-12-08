[@@@SCaml iml_optimization=false]
open SCaml

type t = 
  | Foo of int
  | Bar of nat * tz
  | X
  | Boo of (string * string)
  | Y

let main (x:unit) (y:unit) =
  [],
  assert (
    match Foo (Int 1) with
    | Foo (Int 1) -> true
    | _ -> false
  )
