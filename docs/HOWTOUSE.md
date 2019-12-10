# How to use SCaml

## Learn OCaml

SCaml is a modified OCaml compiler.
If you are not familiar with OCaml, please learn it first.

## Modes

### Compilation mode: `scamlc`

The compiler `scamlc` has almost the same interface as `ocamlc`.
`scamlc xxx.ml` compiles `xxx.ml` to `xxx.tz`.

### Converrsion mode: `scamlc --scaml-convert`

There is a SCaml specific compiler switch `--scaml-convert`.
With this option, `scamlc` command takes a `.ml` and print Michelson representations of
ML constants and types to stdout.  The conversion targets must be defined as toplevel
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
            ; { name= "dow"; age= Nat 50; salary= Tz 1.0 }
            ]
```

then,

```shell
$ scamlc --scaml-convert hoo.ml
type t: pair string (pair nat mutez)
type u: or int (or (pair int (pair mutez string)) (list (pair string (pair nat mutez))))
v: Right (Right { Pair "jon" (Pair 18 10000000000) ; Pair "dow" (Pair 50 1000000) })
```

Note that the values must be constants.  Constructors and types can refer to types
defined in other modules, as far as they are already compiled to `.cmi` files.
