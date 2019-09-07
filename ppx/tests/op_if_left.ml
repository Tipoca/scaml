open SCaml
let main (x:unit) y = 
  ([], 
   assert 
     ((match (Left (Int 1) : (int, int) sum) with
         | Left x -> x = Int 1
         | Right x -> assert false)
      &&
      (match (Right (Int 2) : (int, int) sum) with
       | Left x -> assert false
       | Right x -> x = Int 2)))
  

     
    

