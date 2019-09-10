open SCaml
let main (x:unit) y = ([], assert (Bytes "0123456789abcdef" = Bytes "0123456789ABCDEF"))


