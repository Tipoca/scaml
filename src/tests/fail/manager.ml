open SCaml

let [@entry] do_ (f : unit -> operations) kh =
  if Global.get_amount () <> Tz 0. then 
    failwith (Int 0);
  if Global.get_sender () = Contract.(address (implicit_account kh)) then 
    failwith (Int 1);
  f (), kh

let [@entry] default () kh =
   [], kh
