open SCaml

let from_Some_int = function
  | Some (x : int) -> x
  | None -> assert false

let from_Some_nat = function
  | Some (x : nat) -> x
  | None -> assert false
