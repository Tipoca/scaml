# SCaml, Smart Contract Abstract Machine Language.

> SCaml.  It's not a Scam.

Small and Simple Strict Subset of OCaml for Smart contracts.

## Strict subset of OCaml

SCaml is a **strict** subset of OCaml programming language.
All the valid SCaml programs are also valid as OCaml programs.
This immediately leads to the following benefits:

* Many OCaml programming tools, such as Merlin, Tuareg, OCamlFormat, PPX'es, etc can be used for SCaml for free.
* SCaml programs are also compilable as OCaml programs, which can simulate their behaviour.  It will also open a door to write a dApp consists both of on-chain and off-chain programs in one language.
* OCaml programmers can start writing SCaml programs immediately.  New comers can learn SCaml through existing OCaml language tutorials and references.
* Researchers can use SCaml for bases of their research prototypes.

## Restrictions

The following OCaml features are **not** supported:

* Recursion
* Polymorphism
* Sum types other than lists, options, and `type ('a,'b) sum = Left of 'a | Right of 'b`.
* Product types other than a pair: `t1 * t2`.
* Modules.
* Labeled functions.
* Exception patterns.
* Multi case in `function`
* Partial applicaiton of primitives defined in `SCaml`.
* Reference or mutable record fields.
* Exceptions.
* Arrays.
* Classes, and objects.

### No user defined types (yet)

For simplicity SCaml support only 2 ways to create complex types:
pair `ty1 * ty2` and sum `(ty1, ty2) sum`, which correspond with
the composite types of Michelson.  The pair is a two arity tuple
and `sum` has the following definition:

```
type ('a, 'b) sum =
  | Left of 'a
  | Right of 'b
```

### Experimental: Pattern match

Full pattern matching for `match` is recently added as an experimental feature.  It is not yet fully tested.

## Design

### Parsing by OCaml

Parsing is done by the original OCaml parser.

### Typing by OCaml

Typing is done by the original OCaml type checker.
Primitives defined in a module `SCaml` is referred.

### SCaml typing 

An additional small typing phase to enforce the types of the entry points
to be `'parameter -> 'storage -> (SCaml.operation list, 'storage)`

It will also try forcing the typing of `SCaml.self`.

### Conversion to IML

Typed OCaml AST is converted to InterMediate Laguage IML.
IML is a simple typed purely functional language.

Most of the unsupported features of OCaml are rejected here.

### Compilation to Michelson

`IML` AST is compiled to Michelson.

## Features

### Arithmetic types

* Integers: `Int 42`, `Int (-100)`
* Natural numbers: `Nat 42`, `Nat 12345`
* Tezzies: `Tz 1.0`, `Tz 0.000001`

Each arithmetic type has its own set of arithmetic binary operators:

* Integers: `+`, `-`, `*`, etc
* Natural numbers: `+^`, `-^`, `*^`, etc
* Tezzys: `+$`, `-$`, `*$`, etc

In future,

* Integer suffixes to natural numbers and tezzys, like `42p`, `1.23t`
* SML style overloading of these operators.

### Container literals

* Lists: `[ Int 1; Int 2; Int 3 ]`
* Sets: `Set [ Nat 1; Nat 2; Nat 3 ]`
* Maps: `Map [ (Nat 1, "1"); (Nat 2, "2"); (Nat 3, "3") ]`

### String based constants

* Bytes: `Bytes "0123456789abcdef"`
* Address: `Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"`
* Keys: `Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"`
* Key hashes: `Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`
* Signatures: `Signature "edsigu4chLHh7rDAUxHyifHYTJyuS8zybSSFQ5eSXydXD7PWtHXrpeS19ds3hA587p5JNjyJcZbLx8QtemuJBEFkLyzjAhTjjta"`

### Entry points

Top level `let` bindings with `[@entry]` attribute are treated as entry points.  If none of `[@entry]` specified, the last value definition is treated as the entry point:

```
let [@entry] case1 (param : int) storage = ...
let [@entry] case2 (param : string) storage = ...
```

## Examples

Under `src/tests/`

Library functions are listed in `src/tests/SCaml.ml`.

## Unsupported

* `CREATE_CONTRACT`
* Check of validities of addresses, keys, key_hashes and signatures, which can be checked by Michelson typing.


