(* INPUT= ()
   STORAGE= (None : (address * address) option)
*)
open SCaml
let main () _ =
  [],
  Some (Global.get_source (), Global.get_sender ())
