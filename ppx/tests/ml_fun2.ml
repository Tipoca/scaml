open SCaml

let f x y = x - y

let main (x:unit) y = [], assert (f (Int 7) (Int 4) = Int 3)
