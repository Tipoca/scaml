[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y = 
  [],
  assert (
    ~- (Int (-2)) = Int 2 
    && ~-^ (Nat 2) = Int (-2)
  )

