open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
   ( match true with
     | true -> true
     | _ -> false
   )
  )
