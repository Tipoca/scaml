# 1.0.1

## Language

* Prevent non storables from being `APPLY`'ed to `LAMBDA`.
    * `fun` body can no longer have free variable occurrences 
	   with non storable types.
    * Stopped reducing  let x = e1 in e2  =>  e2[e1/x]  since it may change 
	  free variable occurrences inside fun, which may put unserializable 
	  values into closures.
* Added missing optimizations
* IML printing by Pprintast using ppxlib
* Removing garbages after FAILWITH in constants

## Library

* Added `List.fold_left'`, `Set.fold'`, `Map.fold'`, `Map.map'`, variants of
  `fold` and `map` which take uncurried functions.

## Tests

* Run typecheck before run
* Multisig examples
* Doc and fix for `test.sh` and `test_all.sh`

# 1.0.0

Initial release
