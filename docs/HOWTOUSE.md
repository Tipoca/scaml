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

## Writing Smart Contracts in OCaml

### One file per contract

SCaml does not support separate compilation.  All the code for a contract
must be in one `.ml` file.

### `open SCaml`

API functions to access Michelson primitives are declared in module `SCaml`.
In SCaml, you should always `open SCaml` first.
(You can skip it but there is almost no point to do it.)

In the normal installation, module `SCaml` should be found at directory
`` `opam config var prefix`/lib/scaml ``  Check `SCaml.mli` in this directory
or the source code of SCaml to learn what functions are available and their comments.

### Entry point

Unless specified explicitly, the last value definition in an `.ml` file is considered
as the entry point of the smart contract defined by the source file.

### Multiple entry point support (experimental)

SCaml expermentally supports the multiple entry points introduced in Tezos Babylon.

To have more than one entry points their definitions must be attributed with `[@@entry]`. 





