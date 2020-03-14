[@@@SCaml iml_optimization=true]
open SCaml
open Contract
let [@entry] default () () =
  [],
  assert ( 
    match (contract' (address self) "hello" : int contract option) with 
    | None -> false 
    | Some c -> address c = address c
  )

let [@entry] hello (i : int) () =
  [],()
