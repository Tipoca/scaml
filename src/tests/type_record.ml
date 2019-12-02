open SCaml

type t = { int : int ; nat : nat ; tz : tz }

let main (x:unit) (y:unit) =
  [],
  assert (
    { int= Int 1; nat= Nat 1; tz= Tz 1. } = { int= Int 1; nat= Nat 1; tz= Tz 1. }
                            
    &&
    
    match { int= Int 1; nat= Nat 1; tz= Tz 1. } with
    | { nat= Nat 1 ; int= Int 1 } -> true
    | _ -> false
  )
