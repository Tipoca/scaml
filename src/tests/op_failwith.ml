(* MUST_FAIL *)
open SCaml
let main (x:unit) y = if true then Error.failwith (Int 12) else ([], ())

