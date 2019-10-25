open SCaml
let main (x:unit) y = 
  [], assert (Map.length (Map.empty : (int, nat) map) = Nat 0)

