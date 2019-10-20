open SCaml
let main (x:unit) y = 
  [], 
  assert (Map.length (Map [(Int (-1), "one") ; (Int 2, "two"); (Int 3, "three")]) = Nat 3
          && Map.length (Map [] : (int, string) map) = Nat 0)

                      
    

