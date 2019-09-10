open SCaml
let main (x:unit) y = 
  let ta = 
    Operation.create_account
      (Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
      None
      true
      (Tz 10.0)
  in
  [fst ta], ()

      
