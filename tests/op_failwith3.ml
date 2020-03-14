(* MUST_FAIL *)
(* The compiler must remove the final stack cleaning code since the main part ends with FAILWITH *)
open SCaml

let main () () =
  let b = true in
  if b then
    failwith b
  else
    failwith b
