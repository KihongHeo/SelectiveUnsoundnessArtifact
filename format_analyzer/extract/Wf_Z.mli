open BinInt

type __ = Obj.t

val coq_Z_of_nat_complete_inf : int -> int

val coq_Z_of_nat_set : (int -> 'a1) -> int -> 'a1

val natlike_rec : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1

val natlike_rec2 : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1

val natlike_rec3 : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1

val coq_Zlt_0_rec : (int -> (int -> __ -> 'a1) -> __ -> 'a1) -> int -> 'a1

val coq_Z_lt_rec : (int -> (int -> __ -> 'a1) -> 'a1) -> int -> 'a1

val coq_Zlt_lower_bound_rec :
  int -> (int -> (int -> __ -> 'a1) -> __ -> 'a1) -> int -> 'a1

