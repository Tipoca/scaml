[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y =
  [],
  assert (
    Crypto.blake2b (Bytes "0123456789ABCDEF") =
    Crypto.blake2b (Bytes "0123456789ABCDEF")
  )
