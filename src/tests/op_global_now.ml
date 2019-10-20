open SCaml
let main (x:unit) y =
  [],
  assert ( Global.get_now () = Global.get_now ())
