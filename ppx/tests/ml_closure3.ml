open SCaml
let f x y z = x + y + z
let main () () = [], assert (f (Int 1) (Int 2) (Int 3) = Int 6)
