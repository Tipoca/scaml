[@@@SCaml iml_optimization=false]
open SCaml
let main x y =
  [],
  assert ( Global.get_sender () = Global.get_sender ())
