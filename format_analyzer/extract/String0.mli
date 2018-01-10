val string_rect :
  'a1 -> (char -> char list -> 'a1 -> 'a1) -> char list -> 'a1

val string_rec : 'a1 -> (char -> char list -> 'a1 -> 'a1) -> char list -> 'a1

val string_dec : char list -> char list -> bool

val append : char list -> char list -> char list

val length : char list -> int

val get : int -> char list -> char option

val substring : int -> int -> char list -> char list

val prefix : char list -> char list -> bool

val index : int -> char list -> char list -> int option

val findex : int -> char list -> char list -> int

val coq_HelloWorld : char list

