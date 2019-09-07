```
    <data> ::=
V     | <int constant>
V     | <natural number constant>
v     | <string constant>
      | <timestamp string constant>
      | <signature string constant>
      | <key string constant>
      | <key_hash string constant>
V     | <mutez string constant>     XXX mutez string?
      | <contract string constant>
V     | Unit
V     | True
V     | False
V     | Pair <data> <data>
V     | Left <data>
V     | Right <data>
V     | Some <data>
V     | None
      | { <data> ; ... }
      | { Elt <data> <data> ; ... }
      | instruction
    <instruction> ::=
      | { <instruction> ... }
V     | DROP
V     | DUP
V     | SWAP
V     | PUSH <type> <data>
V     | SOME
V     | NONE <type>
V     | UNIT
V     | IF_NONE { <instruction> ... } { <instruction> ... }
V     | PAIR
V     | CAR
V     | CDR
V     | LEFT <type>
V     | RIGHT <type>
V     | IF_LEFT { <instruction> ... } { <instruction> ... }
-     | IF_RIGHT { <instruction> ... } { <instruction> ... }
V     | NIL <type>
V     | CONS
V     | IF_CONS { <instruction> ... } { <instruction> ... }
      | SIZE
      | EMPTY_SET <comparable type>
      | EMPTY_MAP <comparable type> <type>
      | MAP { <instruction> ... }
      | ITER { <instruction> ... }
      | MEM
      | GET
      | UPDATE
V     | IF { <instruction> ... } { <instruction> ... }
      | LOOP { <instruction> ... }
      | LOOP_LEFT { <instruction> ... }
V     | LAMBDA <type> <type> { <instruction> ... }
V     | EXEC
V     | DIP { <instruction> ... }
      | FAILWITH <data>
      | CAST
      | RENAME
      | CONCAT
      | SLICE
      | PACK
      | UNPACK
V     | ADD
V     | SUB
V     | MUL
      | EDIV
V     | ABS
V     | NEG
      | LSL
      | LSR
V     | OR
V     | AND   ---> evaluation rule must be checked XXX
V     | XOR
V     | NOT
V     | COMPARE
V     | EQ
V     | NEQ
V     | LT
V     | GT
V     | LE
V     | GE
      | SELF
      | CONTRACT <type>
      | TRANSFER_TOKENS
      | SET_DELEGATE
      | CREATE_ACCOUNT
      | CREATE_CONTRACT { <instruction> ... }
      | IMPLICIT_ACCOUNT
      | NOW
      | AMOUNT
      | BALANCE
      | CHECK_SIGNATURE
      | BLAKE2B
      | SHA256
      | SHA512
      | HASH_KEY
      | STEPS_TO_QUOTA
      | SOURCE
      | SENDER
      | ADDRESS
    <type> ::=
      | <comparable type>
      | key
      | unit
      | signature
      | option <type>
      | list <type>
      | set <comparable type>
      | operation
      | contract <type>
      | pair <type> <type>
      | or <type> <type>
      | lambda <type> <type>
      | map <comparable type> <type>
      | big_map <comparable type> <type>
    <comparable type> ::=
      | int
      | nat
      | string
      | bytes
      | mutez
      | bool
      | key_hash
      | timestamp
      | address
```
