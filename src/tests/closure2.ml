[@@@SCaml iml_optimization=false]
open SCaml
let a = Int 1
let f c = 
  let d = c + a in
  let g e = e + d in
  g

(*
   let f (a,c) =
     let d = c + a in
     let g (d,e) = e + d in
     fun e -> g (d,e)
*)
let main (x : unit) y = [], assert (f (Int 3) (Int 2) = Int 6)
