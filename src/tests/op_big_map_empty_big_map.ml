open SCaml
let main (x:unit) y = 
  [], assert (BigMap.mem (Int 1) (BigMap.empty : (int, nat) big_map) = false)
    
