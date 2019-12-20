open SCaml

(* chain_id is not comparable *)
let main (param : unit) (storage : unit) = 
  [], assert (
    Obj.pack (Global.get_chain_id ())
    = Obj.pack (Chain_id "NetXdQprcVkpaWU")
  )

