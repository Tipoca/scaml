open SCaml
let main (x:unit) y = 
  [], assert (match Map.get (Int 2) (Map [ (Int 1, Int 1); (Int 2, Int 2) ]) with None -> false | Some x -> x = Int 2)

