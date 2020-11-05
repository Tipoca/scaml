(* INPUT= (Int 3, Int 4) 
*)
open SCaml

let [@entry] f v _ =
  let x = 
    match v with
    | Int 1, Int 2 -> (Int 1, Int 2)
    | x -> x
  in
  assert (x = (Int 3, Int 4));
  [], ()
