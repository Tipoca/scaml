# Installation

Clone the repo:

```
$ git clone https://gitlab.com/dailambda/scaml
$ cd scaml
```

If you have no 4.07.1 OCaml switch.  (Of course you have `opam` already!):

```
$ opam switch create . ocaml-base-compiler.4.07.1
```

Check your OCaml version:

```
$ ocamlc -version
4.07.1
```

Install required packages:

```
$ opam install -y ocaml-migrate-parsetree ppxx typpx spotlib hex ptime zarith
```

Compile SCaml:

```
$ dune build
$ dune install
```

If successful, there should be the compiler executable:

```
$ ls _opam/bin/scamlc
_opam/bin/scamlc
```

# Test

```
$ cd tests
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

