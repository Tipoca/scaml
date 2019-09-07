# SCaml, Smart Contract Abstract Machine Language.

> Scam never calls itself a scam.

## Strict subset of OCaml

SCaml is a **strict** subset of OCaml programming language.
All the valid SCaml programs are also valid as OCaml programs.
This immediately leads to the following benefits:

* Many OCaml programming tools can be used for SCaml for free.
* SCaml programs are also compilable as OCaml code, which can simulate the behaviour.
* Users can use the existing OCaml programming language tutorials and references to learn SCaml.

## Restrictions

The following OCaml features are not supported:

* Recursion
* Sum types other than lists and `type ('a,'b) sum = Left of 'a | Right of 'b`.
* Product types other than a pair: `t1 * t2`.
* SML modules.
* Labeled functions.
* Nested patterns. Constants in patterns. "Or" pattern.  Pattern guards.  Exception patterns.
* Multi case in `function`
* Partial applicaiton of primitives defined in `SCaml`.
* Reference or mutable record fields.
* Exceptions.
* Arrays.
* Classes, and objects.
* Local `let .. in ..`

## Desgin

### Parsing by OCaml

Parsing is done by the original OCaml parser.

### Typing by OCaml

Typing is done by the original OCaml type checker.
Primitives defined in a module `SCaml` is referred.

### SCaml typing 

An additional small typing phase to enforce the type of the entry point
to be `'parameter -> 'storage -> (SCaml.operation list, 'storage)`

It also forces the typing of `SCaml.self`.

### Conversion to IML

Typed OCaml AST is converted to InterMediate Laguage `IML`.
Most of the unsupported features of OCaml are rejected here.

`IML` performs type inference for typed closure conversion.

### Compilation to Michelson

`IML` AST is compiled to Michelson.


