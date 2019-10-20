open SCaml
let main () () =
  [],
  assert (
    Map.length (Map [ "a", "a"; "b", "b" ]) = Nat 2
  )
