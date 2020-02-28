(* MUST_FAIL *)
[@@@SCaml iml_optimization=false]
open SCaml
let main () () = 
  [],
  assert ((Int 10) / (Int 0) = Int 3) (* FAIL *)

