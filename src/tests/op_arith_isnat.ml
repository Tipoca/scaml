open SCaml
let main (x:unit) y = 
  [],
  assert (
      match isnat (Int (-1)) with
      | Some _ -> false
      | None -> match isnat (Int 1) with
                | None -> false
                | Some x -> x = Nat 1
    )
