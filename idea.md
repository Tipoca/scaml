# SCaml

OCaml based compiler for blockchain smart contracts.

## Goal

Provide a solid, industry ready Michelson compiler as soon as possible.

Currently we have several high level programming languages for Tezos,
which compile their code to Michelson.  The problem is there is none
ready for serious use:

* Liquidity: It looks good but with a serious bug of inter contract calls.  Huge political risk, too.
* LIGO: The current implementation is far from complete with too many bugs.  The team seems to build a new one from scratch, as a certified compiler, which should require very long time to release.
* Fi: many glitches. Not all Michelson features are accessible.
* SmartPy: There is no documentation except few blog posts.
* Archtype
* ... other compilers should be visited too

The lack of serious high level languages is now the bottleneck of development around Tezos, which forces developers write Michelson by hand.  Michelson is OK to read and write if the code is small, but it does not scale at all.

To overcome this difficulty, we are going to build a compiler which is industry ready in a short time.

## Priorities

Time: We want a working tools as soon as possible.

Features: SCaml must be a *strict* subset of a well-documented existing language, so that users can use SCaml reading the existing documentation of the base language.  This means that any valid code for SCaml can be compiled and be simulated in the base language.  Most of the existing tools of the base language's eco system are expected to be ready for SCaml for free.  We are not interested to port all the funcitonalities of the base language in SCaml.  It's features can be limited, but in a predictable way.

Not from scratch: To reduce the development time and possibility of introducing bugs, SCaml compiler code must reuse the source code of the base language as much as possible.

We do not care about the user population of the base language, since our objective is to provide a firm development tool for exisiting professional Tezos developers.  The mass can follow them afterwards.

## Solution

The name has already suggests, we use OCaml as the base of SCaml:

* OCaml is used in Tezos.  Popular to its developers.
* It is well tested in the industry for many years.
* The compiler code is modular (i.e. `compiler-libs`), and we have good knowledge of its compiler.

### Compiler phases

* Parsing: We use OCaml compliler's parser.
* Pre-processing: We can add our language special syntactic features at this phase, using PPX.
* Filtering: Code with any unsupported syntactic constract in SCaml is rejected:, objects, functors, polymorphic variants, modules, etc.
* Typing: We use OCaml compiler's typing system.
* Post-type check:  Some well-typed programs checked by the previous phase may be rejected by this layer for SCaml's restriction.
* Compilation to Michelson: The typed OCaml AST is compiled down to Michelson.  Here again, some of well-typed OCaml code may be rejected.

### Difficulties

* Pattern match compilation:  Mainly due to its complexity and lack of our knowledge about it.  We do not support it in the first version.
