open SCaml

let main (x:unit) (y:unit) =
  [],
  assert (
    (Int 1, Nat 1, Tz 1.) = (Int 1, Nat 1, Tz 1.)
                            
    &&
    
    match (Int 1, Nat 1, Tz 1.) with
    | (_, Nat 1, _) -> true
    | _ -> false
  )
