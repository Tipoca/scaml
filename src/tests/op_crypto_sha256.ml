[@@@SCaml iml_optimization=false]
open SCaml
let main (x:unit) y =
  [],
  assert (
    Crypto.sha256 (Bytes "0123456789ABCDEF") =
    Crypto.sha256 (Bytes "0123456789ABCDEF")
  )
