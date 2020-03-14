[@@@SCaml iml_optimization=false]
open SCaml
let main param storage =
  [],
  assert (
    ( match (Left (Int 1) : (int, int) sum) with
      | Left x when x = Int 2 -> false
      | Right _ -> false
      | Left x -> true
    )
  )
