[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y = 
  [], 
  (* order of elements: Michelson does not care about it *)
  assert (Set.length (Set [Int 3 ; Int 2; Int 1]) = Nat 3)
                      
    

