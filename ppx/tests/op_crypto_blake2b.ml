open SCaml
let main (x:unit) y =
  [],
  assert (
    Crypto.blake2b "hello" =    Crypto.blake2b "hello"
  )
