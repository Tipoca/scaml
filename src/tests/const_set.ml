[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y = 
  [], 
  assert (Set.length (Set [Int (-1) ; Int 2; Int 3]) = Nat 3
          && Set.length (Set [] : int set) = Nat 0)

                      
    

