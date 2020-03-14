[@@@SCaml iml_optimization=false; ]
open SCaml

let f (x, y) = x - y

let main x y = [], assert (f (Int 7, Int 4) = Int 3)
