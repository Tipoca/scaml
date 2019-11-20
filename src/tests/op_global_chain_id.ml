open SCaml
let main (x:unit) y =
  [],
  assert ( Global.get_chain_id () = Global.get_chain_id ())
