open SCaml
open Map
let main (x:unit) y = 
  [], assert (
    mem "a" (update "a" (Some "b") (Map ["b", "c"; "d", "e"]) )
  )
      

    
    

