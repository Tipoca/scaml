open Spotlib.Spot

type t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  }

val pp : Format.t -> t -> unit
val eval : t -> Longident.t Location.loc * [`Bool of bool] -> t
