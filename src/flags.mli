open Spotlib.Spot

type t = 
  { iml_optimization  : bool
  ; iml_pattern_match : bool
  ; scaml_debug       : bool
  }

val flags : t ref

val pp : Format.t -> t -> unit
val eval : t -> Longident.t * [`Bool of bool | `Constant of Parsetree.constant ] -> (t, string) Result.t
val update : (t -> t) -> unit
val if_debug : (unit -> unit) -> unit
