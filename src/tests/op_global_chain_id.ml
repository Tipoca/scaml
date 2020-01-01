open SCaml

(* chain_id is not comparable *)
let main param storage =
  [], assert (
    Obj.pack (Global.get_chain_id ())
    = Obj.pack (Chain_id "NetXdQprcVkpaWU")
  )

