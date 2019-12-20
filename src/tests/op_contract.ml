(* We must optmization on, otherwise a closure with self contract
   is created, which is illegal in Michelson
*)
[@@@SCaml iml_optimization=true]
open SCaml
open Contract
let main (x:unit) y =
  [],
  assert ( 
    match (contract (address self) : unit contract option) with 
    | None -> false 
    | Some c -> address c = address self
  )
