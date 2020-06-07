open Typerep_lib.Std

exception Overflow
exception Rounded

val to_michelson : 'a Typerep.t -> 'a -> Michelson.Constant.t

val of_micheline : 'a Typerep.t -> Michelson.Micheline.parsed -> Michelson.Constant.t option

val of_michelson : 'a Typerep.t -> Michelson.Constant.t -> 'a option

module TypeSafePack : SCaml.Obj.Internal.TypeSafePack
