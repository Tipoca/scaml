open SCaml

let [@entry] main () () =
  [],
  assert (Option.from_Some_int (Some (Int 1)) = Int 1)
