open SCaml

let [@entry] do_ f key_hash = 
  let _ = assert (Global.get_amount () = Tz 0.) in
  let _ = assert (Global.get_sender () 
                   = Contract.address (Contract.implicit_account key_hash)) in
  (f (), key_hash)
  
let [@entry] default (unit : unit) key_hash =
  ([], key_hash)
