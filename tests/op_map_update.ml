[@@@SCaml iml_optimization=false]
open SCaml
open Map
let main x y = 
  [], assert (
    mem "a" (update "a" (Some "b") (Map ["b", "c"; "d", "e"]) )
  )
      

    
    

