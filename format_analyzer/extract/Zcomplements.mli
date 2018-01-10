open BinInt
open Wf_Z
open Zabs

type __ = Obj.t

val floor_pos : int -> int

val floor : int -> int

val coq_Z_lt_abs_rec : (int -> (int -> __ -> 'a1) -> 'a1) -> int -> 'a1

val coq_Zlength_aux : int -> 'a1 list -> int

val coq_Zlength : 'a1 list -> int

