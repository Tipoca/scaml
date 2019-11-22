open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x -> Int 1 = x
      | Right y -> false 
    )
  )
