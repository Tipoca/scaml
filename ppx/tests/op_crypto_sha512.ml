open SCaml
let main (x:unit) y =
  [],
  assert (
    Crypto.sha512 (Bytes "0123456789ABCDEF") =
    Crypto.sha512 (Bytes "0123456789ABCDEF")
  )
