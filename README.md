# SCaml, Smart Contract Abstract Machine Language.

> SCaml.  It's not a Scam.

Small and Simple Strict Subset of OCaml for Smart contracts.

## Strict subset of OCaml

SCaml is a **strict** subset of OCaml programming language.
All the valid SCaml programs are also valid as OCaml programs.
This immediately leads to the following benefits:

* Many OCaml programming tools, such as Merlin, Tuareg, OCamlFormat, PPX'es, etc can be used for SCaml for free.
* SCaml programs should be compilable by OCaml to native executables with an appropriate library, which can simulate their behavior.  It will also open a door to write a dApp consists both of on-chain and off-chain programs in one language.
* OCaml programmers can start writing SCaml programs immediately.  New comers can learn SCaml through existing OCaml language tutorials and references.
* Researchers can use SCaml for bases of their research prototypes.

## Restrictions

The following OCaml features are **not** supported:

### Recursion

Michelson has no recursion opcode.  Therefore neither for SCaml: no `let rec` construct.  

Map/iter/folding over lists, sets, maps and big maps are supported.
`Loop.left` is also available to uses `LOOP` Michelson opcode.

### Polymorphism

Michelson is monomorphic language.  So is SCaml.

If you need polymorphism, copy the definition and instantiate for each type.

You may have to add type constraints to your SCaml programs when expressions have too general types.

### Separate compilation

A contract must be written in one file.

### Others

* Modules.
* Labeled functions.
* Partial application of primitives defined in `SCaml`.
* Side effects: reference, mutable record fields, arrays.
* Exceptions.
* Classes, and objects.

## Unsupported features of Michelson for now

### `CREATE_CONTRACT`

Giving a nice API for `CREATE_CONTRACT` opcode in ML is not trivial.
We are currently studying the optimal design.

### Check of pushable/comparable or not

SCaml itself does not type-check its Michelson output.  The output
must be checked by Tezos node's Michelson type checker to find out
illegal uses of `PUSH` and comparisons for now.

### Validity checks of string representations of addresses, keys, key hashes and signatures

Michelson type-checker must be used to detect invalid string based literals.

## Design

### Parsing by OCaml

Parsing is done by the original OCaml parser.

### Typing by OCaml

Typing is done by the original OCaml type checker.
It uses module `SCaml` which defines primitive values and types for Michelson.

### SCaml typing 

An additional small typing phase to enforce the types of the entry points
to be `'parameter -> 'storage -> (SCaml.operation list, 'storage)`

It will also try forcing the typing of `SCaml.self`.

### Conversion to IML

Typed OCaml AST is converted to InterMediate Laguage IML.
IML is a simple typed purely functional language.

Most of the unsupported features of OCaml are rejected here.

Pattern matches are decomposed to simple switches in this phase.

### Compilation to Michelson

`IML` AST is compiled down to Michelson.
It is a trivial compilation from FP to stack VM.

## Examples

Under `src/tests/`

Library functions are listed in `src/tests/SCaml.ml`.

## Road-map

> "Road-map" is another expression of "promises almost never fulfilled in time". 

### 1.0 Pyramid (2020-01-01)

"Pyramid" will be the first official release of SCaml:

* Attractive web site.
* Minimal support of `CREATE_CONTRACT`.

Pyramid is a big feature close.  After it, we concentrate on the maintenance
for a while.

## Further information

Look documents under `docs/`.
