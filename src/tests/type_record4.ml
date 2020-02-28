[@@@SCaml iml_optimization=false]
open SCaml

type t = { a : int } (* single field had some problem of annotation *)

let main () () =
  let x = { a = Int 42 } in
  [], ()
