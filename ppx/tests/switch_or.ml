open SCaml
let main (x:unit) y = 
  ([], 
   match (Left (Int 1) : (int, int) sum) with
   | Left x -> assert (x = Int 1)
   | Right x -> assert false)

     
    

