open SCaml
let main (x:unit) y = 
  [],
  assert (Int 1 + Int 3 = Int 4
          && Nat 1 +^ Nat 3 = Nat 4
          && Tz 1.0 +$ Tz 3.0 = Tz 4.0)
