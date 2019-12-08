[@@@SCaml iml_optimization=false]
open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    match (Left (Int 1) : (int, (int, unit) sum) sum) with
      | Left _ -> true
      | _ -> false
  )
