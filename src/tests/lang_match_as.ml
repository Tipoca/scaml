open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    (* XXX tuple is formed, which is redundant *)
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x as y -> (Left x : (_, unit) sum) = y
      | _ -> false
    )
  )
