type side = Left | Right

type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

val place : 'a list -> 'a tree

val path : int -> int -> side list

val fold : leaf:('a -> 'b) -> branch:('b -> 'b -> 'b) -> 'a tree -> 'b
