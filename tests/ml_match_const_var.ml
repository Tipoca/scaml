[@@@SCaml iml_optimization=false]
open SCaml
let main param storage =
  [],
  assert (
    ( match Int 1 with
      | Int 2 -> false
      | x -> true
    )
  )
