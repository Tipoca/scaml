open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    ( match (Left (Int 1) : (int, int) sum) with
      | Left x when x = Int 2 -> false
      | Right _ -> false
      | Left x -> true
    )
  )
