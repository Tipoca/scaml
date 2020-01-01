(* MUST_FAIL *)
open SCaml
let main x y = if true then Error.failwith (Int 12) else ([], ())

