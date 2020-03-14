[@@@SCaml iml_optimization=false]
open SCaml
let main x y =
  [],
  assert ("hello world" = String.concat "hello " "world"
         && "hello world" = "hello " ^ "world")
