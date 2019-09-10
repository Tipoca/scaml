open SCaml
let main (x:unit) y =
  [],
  assert ( Global.get_amount () = Global.get_amount ())
