open SCaml
let main (x:unit) y =
  [],
  assert ( Global.get_source () = Global.get_source ())
