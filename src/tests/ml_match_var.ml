[@@@SCaml iml_optimization=false]
open SCaml
let main param storage =
  [],
  assert (
    (* XXX tuple is formed, which is redundant *)
    ( match Int 1 with
      | x -> x = Int 1
    )
  )
