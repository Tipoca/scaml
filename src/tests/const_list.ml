[@@@SCaml iml_optimization=false]
open SCaml
(* Check it is compiled to PUSH (list string) { "a"; "b"; "c" } *)
let main () () = [], assert (List.length ["a"; "b"; "c"] = Nat 3)
