open SCaml
let main (x:unit) y = 
  [], assert (
    BigMap.mem (Int 2) 
      (BigMap.update (Int 2) (Some (Int 2)) BigMap.empty)
  )

    
    

