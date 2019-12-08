open Spotlib.Spot

type t = 
  { iml_optimization  : bool
  ; iml_pattern_match : bool
  ; scaml_debug       : bool
  ; scaml_convert     : bool (** type and value conversion mode *)
  ; scaml_noscamlib   : bool (** do not add -I `opam config var prefix`/scaml *)
  }

val flags : t ref

val pp : Format.t -> t -> unit
val eval : t -> Longident.t * [`Bool of bool | `Constant of Parsetree.constant ] -> (t, string) Result.t
val update : (t -> t) -> unit
val if_debug : (unit -> unit) -> unit
