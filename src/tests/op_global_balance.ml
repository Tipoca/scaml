[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y =
  [],
  assert ( Global.get_balance () = Global.get_balance ())
