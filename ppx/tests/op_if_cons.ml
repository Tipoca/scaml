open SCaml
let main (x:unit) y = 
  [], 
  assert 
    (match [Int 1; Int 2] with
     | [] -> false
     | x::xs -> 
         match xs with
         | [] -> false
         | y::ys ->
             match ys with
             | z::zs -> false
             | [] -> (x = Int 1) && (y = Int 2))

    

