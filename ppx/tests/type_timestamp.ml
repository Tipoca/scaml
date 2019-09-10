open SCaml
open Timestamp
let main (x:unit) y =
  let now = Global.get_now () in
  [],
  assert (
    add now (Int 10)  = sub (add now (Int 20)) (Int 10)
    && diff (add now (Int 10)) now = Int 10
  )
