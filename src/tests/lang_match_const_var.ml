open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    ( match Int 1 with
      | Int 2 -> false
      | x -> true
    )
  )
