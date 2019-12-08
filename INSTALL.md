# SCaml installation

## Installation

Clone the repo:

```
$ git clone https://gitlab.com/dailambda/scaml
$ cd scaml
```

Install `opam`, then:

```
$ opam switch create . ocaml-base-compiler.4.07.1
$ opam install -y vendors/*/*.opam src/scaml.opam
```

If successful, there should be the compiler executable:

```
$ which scamlc
.../_opam/bin/scamlc
```

## How to use

### Compiling `.ml` files to `.tz`

The compiler `scamlc` has almost the same interface as `ocamlc`.
`scamlc xxx.ml` compiles `xxx.ml` to `xxx.tz`.

### Compile ML values and types to Michelson

There is a special compiler switch `--scaml-convert`.  With this option,
`scamlc` command takes a `.ml` and print out Michelson representations of
ML constants and types.  The conversion targets must be defined as toplevel
declarations.  For example:

```ocaml
(* hoo.ml *)
open SCaml
type t = 
  { name   : string
  ; age    : nat
  ; salary : tz
  }

and u = 
   | Foo of int * tz * string
   | Bar
   | Boo of t list
   | Far

let v = Boo [ { name= "jon"; age= Nat 18; salary= Tz 10000.0 }
            ; { name= "doh"; age= Nat 50; salary= Tz 1.0 }
            ]
```

```shell
$ scamlc --scaml-convert hoo.ml
type t: pair string (pair nat mutez)
type u: or int (or (pair int (pair mutez string)) (list (pair string (pair nat mutez))))
v: Right (Right { Pair "jon" (Pair 18 10000000000) ; Pair "doh" (Pair 50 1000000) })
```

Note that definitions can refer types and constructors defined in other files,
if they are compiled and their `.cmi` files exist.

## Test and examples

`src/tests` directorty contains *working* tests which you can use as examples.

```
$ cd src/tests
$ ./test.sh xxx.ml
```

If `tezos-client` is in `PATH` and it is configured to connect to a running node with a valid blockchain protocol, it should also dry-run the compiled tz:

```
$ ./test.sh closure2.ml 
comp=dune exec ../main.exe --
----- closure2.ml
dune exec ../main.exe -- /Users/jun/.share/4.07.1/scaml/src/tests//_build/closure2.ml
Entering directory '/Users/jun/.share/4.07.1/scaml'
Entering directory '/Users/jun/.share/4.07.1/scaml'
parameter unit ;
storage unit ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main */
           PUSH int 6 ;
           { /* = d */ { /* = a */ PUSH int 1 } ; { /* = c */ PUSH int 3 } ; ADD } ;
           { /* = e */ PUSH int 2 } ;
           ADD ;
           COMPARE ;
           EQ ;
           ASSERT ;
           PUSH unit Unit ;
           NIL operation ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;

Executing /var/folders/6_/d6bx9d112z7fzxvwdmwk5qgr0000gn/T/tezos-tmp-client.XXXXXXXX.5M0z3d0L/bin/tezos-client run script /Users/jun/.share/4.07.1/scaml/src/tests//_build/closure2.tz on storage Unit and input Unit
storage
  Unit
emitted operations
  
big_map diff

```
