open Datatypes

val zerop : int -> bool

val lt_eq_lt_dec : int -> int -> bool option

val gt_eq_gt_dec : int -> int -> bool option

val le_lt_dec : int -> int -> bool

val le_le_S_dec : int -> int -> bool

val le_ge_dec : int -> int -> bool

val le_gt_dec : int -> int -> bool

val le_lt_eq_dec : int -> int -> bool

val le_dec : int -> int -> bool

val lt_dec : int -> int -> bool

val gt_dec : int -> int -> bool

val ge_dec : int -> int -> bool

val nat_compare : int -> int -> comparison

val nat_compare_alt : int -> int -> comparison

val leb : int -> int -> bool

