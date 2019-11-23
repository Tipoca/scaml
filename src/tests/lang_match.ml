open SCaml
let main (main : unit) (storage : unit) =
  [],
  assert (
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x -> Int 1 = x
      | Right y -> false 
    )

    && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum) with
      | Left x -> Int 1 = x
      | Right (Left y) -> false 
      | Right (Right z) -> false 
    )
 
   && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum), Int 1 with
      | Left x, y -> x = y
      | Right (Left y), _ -> false 
      | Right (Right z), _ -> false 
    )

   && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum), (Int 1, Int 2) with
      | Left x, (y, _) -> x = y
      | Right (Left _), (x,y) -> x = y
      | Right (Right z), _ -> false 
    )

   && 

   ( match () with
     | () -> true
   )

   &&

   ( match (Left (Int 1) : (int, (int, unit) sum) sum) with
     | Left _ -> true
     | _ -> false
   )

   &&

   ( match true with
     | true -> true
     | _ -> false
   )

   && 

   ( match Int 1 with
     | Int 2 -> false
     | Int 1 -> true
     | _ -> false
   )

 )
