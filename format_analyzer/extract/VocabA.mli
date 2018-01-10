open BinInt
open List0
open TStr
open ZArith_dec

val c_div : int -> int -> int

val default : 'a1 -> 'a1 option -> 'a1

val list_fold : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2

val list_fold2_def :
  ('a1 -> 'a2 -> 'a3 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 -> 'a3 -> 'a3

val list_fold2 :
  ('a1 -> 'a2 -> 'a3 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 -> 'a3

val print : 'a1 -> unit

val print2 : 'a1 -> 'a2 -> unit

val print_when_false : ('a1 -> unit) -> 'a1 -> ('a2 -> bool) -> 'a2 -> bool

val small_step : ('a1 -> 'a2) -> 'a1 -> 'a2

val physical_eq : 'a1 -> 'a1 -> bool

val structural_eq : 'a1 -> 'a1 -> bool

val invalid_arg : string_t -> 'a1

