(* FAIL *)
open SCaml
let main (x:unit) y = if true then failwith (Int 12) else ([], ())

