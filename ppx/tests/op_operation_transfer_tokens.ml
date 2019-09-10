open SCaml
let main (x:unit) y = 
  [ Operation.transfer_tokens () (Tz 10.) Contract.self ],
  ()
  
