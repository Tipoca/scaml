type t = int * int
[@@deriving conv{ocaml}]

val to_string : t -> string
val parse : string -> (t, string) result
val default : t
