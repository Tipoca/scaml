open SCaml
let main (x:unit) y = 
  [],
  assert (match  String.slice (Nat 2) (Nat 3) "hello wolrd"  with
      | None -> false
      | Some x -> x = "llo"
    )