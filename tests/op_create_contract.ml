open SCaml

let main () () =
  let op, ad = 
    (* Write Michelson contract code in a string.  
       I recommend to use OCaml's quoted string {| .. |}
       to avoid escaping newlines and double quotes.
    *)
    Contract.create_raw {|
      parameter unit ;
      storage unit ;
      code { { /* defs */ } ;
             { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
             { /* entry point */ { /* entry main_1004 */ UNIT ; NIL operation ; PAIR } } ;
             { /* final clean up */ DIP { DROP 2 } } } ;
      |}
      None (Tz 1.0) ()
  in
  [op], ()

