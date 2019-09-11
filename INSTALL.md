# Installation

Clone the repo:

```
$ git clone https://gitlab.com/dailambda/scaml
$ cd scaml
```

If you have no 4.07.1 OCaml switch:

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
$ ls _opam/lib/ppx_scaml/ppx.exe 
_opam/lib/ppx_scaml/ppx.exe*
```

# Test

```
$ cd tests
$ ./test.sh xxx.ml
```

If `tezos-client` is in `PATH` and it is configured to connect to a running node with a valid blockchain protocol, it should also dry-run the compiled tz:

```
ppx=/.../_build/install/default/lib/ppx_scaml/ppx.exe
----- ml_closure4.ml
/.../_build/install/default/lib/ppx_scaml/ppx.exe /.../ppx/tests//_build/ml_closure4.ml
{ parameter (unit) ; storage (unit) ;
  code { { /* defs */
           DIP { { /* f */
                   LAMBDA
                     (int)
                     (pair (lambda (pair (int) (option (int))) (pair (lambda (pair (int) (pair (option (int)) (option (int)))) (int)) (pair (option (int)) (option (int))))) (option (int)))
                     { { /* clos */ { /* x */ DUP } ; SOME } ;
                       LAMBDA
                         (pair (int) (option (int)))
                         (pair (lambda (pair (int) (pair (option (int)) (option (int)))) (int)) (pair (option (int)) (option (int))))
                         { { /* get arg */ DUP ; DIP { CAR } ; CDR } ;
                           { /* get x */ IF_NONE { UNIT ; FAILWITH } {  } } ;
                           { /* clos */
                             { /* y */ DIP { DUP } ; SWAP } ; SOME ;
                             { /* x */ DIP { DUP } ; SWAP } ; SOME ; PAIR } ;
                           LAMBDA
                             (pair (int) (pair (option (int)) (option (int))))
                             (int)
                             { { /* get arg */ DUP ; DIP { CAR } ; CDR } ;
                               DUP ;
                               DIP { CAR ;
                                     { /* get x */
                                       IF_NONE { UNIT ; FAILWITH } {  } } } ;
                               CDR ;
                               { /* get y */ IF_NONE { UNIT ; FAILWITH } {  } } ;
                               { /* z */ DIP { DIP { DUP } ; SWAP } ; SWAP } ;
                               { /* y */ DIP { DUP } ; SWAP } ; MUL ;
                               { /* x */ DIP { DIP { DUP } ; SWAP } ; SWAP } ;
                               SUB ;
                               { /* lambda clean up */
                                 DIP { DROP ; DROP ; DROP } } } ;
                           PAIR ;
                           { /* lambda clean up */ DIP { DROP ; DROP } } } ;
                       PAIR ; { /* lambda clean up */ DIP { DROP } } } } } };
         { /* entry point init */ DUP ; CDR ; DIP { CAR } };
         { /* entry point */
           PUSH (int) 4 ;
           { /* f */ DIP { DIP { DIP { DUP } ; SWAP } ; SWAP } ; SWAP } ;
           PUSH (int) 10 ; EXEC ; PUSH (int) 3 ;
           DIP { DUP ; CDR ; DIP { CAR } } ; PAIR ; EXEC ; PUSH (int) 2 ;
           DIP { DUP ; CDR ; DIP { CAR } } ; PAIR ; EXEC ; COMPARE ; 
           EQ ; ASSERT ; PUSH (unit) Unit ; NIL (operation) ; PAIR };
         { /* final clean up */ DIP { DROP ; DROP ; DROP } } } }
open SCaml
let f x y z = x - (y * z)
let main () () = ([], (assert ((f (Int 10) (Int 3) (Int 2)) = (Int 4))))
Executing /.../tezos-client run script /.../ppx/tests//_build/ml_closure4.tz on storage Unit and input Unit
storage
  Unit
emitted operations
  

```


