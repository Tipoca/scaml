open SCaml
let main (x:unit) y = 
  [], assert (Set.mem (Int 1) (Set [ Int 1; Int 2; Int 3 ]))
    

