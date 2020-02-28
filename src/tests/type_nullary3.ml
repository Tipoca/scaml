open SCaml
type t = A | B of int | C
let main () _ = 
  match A with
  | A -> [], ()
  | B _x -> [], ()
  | C -> [], ()

            
