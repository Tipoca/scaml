open SCaml

type init_action = 
  { title : string
  ; beginning_time : timestamp
  ; finish_time : timestamp
  }
        
type action = 
  | Vote of string
  | Init of init_action

type storage = 
  { title : string
  ; candidates : (string, int) map
  ; voters : address set
  ; beginning_time : timestamp
  ; finish_time : timestamp
  }

let init (init_params : init_action) (_ : storage) =
  let candidates = Map [ ("Yes", Int 0); ("No", Int 0) ] in
  ([] : operations),
  { title= init_params.title
  ; candidates
  ; voters= Set []
  ; beginning_time= init_params.beginning_time
  ; finish_time= init_params.finish_time
  }

let vote (parameter : string) (storage : storage) =
  let now = Global.get_now () in

  let _ = assert (now >= storage.beginning_time && storage.finish_time > now) in

  let addr = Global.get_source () in
  let _ = assert (not (Set.mem addr storage.voters)) in
  let x = match Map.get parameter storage.candidates with
    | Some i -> i
    | None -> Int 0
  in
  (
    ([] : operation list),
    { storage with
      candidates = Map.update parameter (Some (x + Int 1)) storage.candidates ;
      voters = Set.update addr true storage.voters ;
    }
  )


let main action storage =
  match action with
  | Vote p -> vote p storage
  | Init x -> init x storage
