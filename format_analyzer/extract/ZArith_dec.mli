open BinInt
open Datatypes
open Sumbool

type __ = Obj.t

val coq_Dcompare_inf : comparison -> bool option

val coq_Zcompare_rect :
  int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1

val coq_Zcompare_rec :
  int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1

val coq_Z_lt_dec : int -> int -> bool

val coq_Z_le_dec : int -> int -> bool

val coq_Z_gt_dec : int -> int -> bool

val coq_Z_ge_dec : int -> int -> bool

val coq_Z_lt_ge_dec : int -> int -> bool

val coq_Z_lt_le_dec : int -> int -> bool

val coq_Z_le_gt_dec : int -> int -> bool

val coq_Z_gt_le_dec : int -> int -> bool

val coq_Z_ge_lt_dec : int -> int -> bool

val coq_Z_le_lt_eq_dec : int -> int -> bool

val coq_Zlt_cotrans : int -> int -> int -> bool

val coq_Zlt_cotrans_pos : int -> int -> bool

val coq_Zlt_cotrans_neg : int -> int -> bool

val not_Zeq_inf : int -> int -> bool

val coq_Z_dec : int -> int -> bool option

val coq_Z_dec' : int -> int -> bool option

val coq_Z_zerop : int -> bool

val coq_Z_notzerop : int -> bool

val coq_Z_noteq_dec : int -> int -> bool

