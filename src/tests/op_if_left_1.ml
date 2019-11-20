open SCaml
let main (x:unit) y = 
  ([], 
   assert 
     (match (Left (Int 1) : (int, unit) sum) with
      | Left x -> x = Int 1
      | Right x ->false))


     
    

