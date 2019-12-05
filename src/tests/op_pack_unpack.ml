[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y =
  [],
  assert (match ( Obj.unpack (Obj.pack (Int 1)) : int option) with
      | None -> false
      | Some x -> x = Int 1)
