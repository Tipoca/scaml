[@@@SCaml iml_optimization=false]
open SCaml
let x = Int 1
let f (unit:unit) = assert ( x = Int 1 )
let main x y = [], f ()
