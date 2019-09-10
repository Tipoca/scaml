open SCaml
let main (x:unit) y =
  [],
  assert (
    Bytes "0123456789abcdef" 
    = Bytes.concat (Bytes "01234567") (Bytes "89ABCDEF")
  )
