(* encode SCaml types and values to Michelson *)

let encode_type s =
  let (++) x f = f x in
  let code = "type =\n" ^ s in (* XXX we should put line number directive *)
  let lexbuf = Lexing.from_string code in
  Location.init lexbuf "command line";
  Parse.implementation lexbuf
  ++ Typemod.type_implementation sourcefile outputprefix modulename env

    
  
  
  
