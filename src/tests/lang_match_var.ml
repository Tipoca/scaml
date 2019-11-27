open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    (* XXX tuple is formed, which is redundant *)
    ( match Int 1 with
      | x -> x = Int 1
    )
  )
