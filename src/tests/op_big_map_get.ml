[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y = 
  [], assert (
    match BigMap.get (Int 2) 
            (BigMap.update (Int 2) (Some (Nat 2)) BigMap.empty)
    with 
    | None -> false 
    | Some x -> x = Nat 2
  )
