[@@@SCaml iml_optimization=false]
open SCaml
let main _ _ = [], ()

(* bug
   PUSH (pair (list operation) unit) (Pair {} Unit)
*)

