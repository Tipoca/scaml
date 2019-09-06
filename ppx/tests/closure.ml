open SCaml
let f b = 
  if b then fun (unit : unit) -> b
  else fun unit -> false
let main (x : unit) y = [], (assert ((f true) ()))
