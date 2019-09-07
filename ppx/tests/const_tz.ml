open SCaml
(* XXX still need to check the mutez conversion is correct *)
let main (x:unit) y = ([], assert (Tz 1.0 +$ Tz 0.000001 = Tz 1.000001))
                      
    

