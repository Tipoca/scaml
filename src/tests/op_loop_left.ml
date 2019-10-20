open SCaml
let main (x:unit) y = 
  [],
  Loop.left (fun x -> if x = Int 0 then Right () else Left (x - Int 1)) (Int 10)
