open SCaml
let main (x:unit) y = 
  [], assert (Set.length (Set.update (Int 1) false (Set [ Int 1; Int 2; Int 3 ])) = Nat 2)
    
    

