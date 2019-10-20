open SCaml
open Contract
let main (x:unit) y =
  [],
  assert ( 
    match (contract (address self) : unit contract option) with 
    | None -> false 
    | Some c -> address c = address self
  )
