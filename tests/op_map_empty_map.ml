[@@@SCaml iml_optimization=false]
open SCaml
let main x y = 
  [], assert (Map.length (Map.empty : (int, nat) map) = Nat 0)

