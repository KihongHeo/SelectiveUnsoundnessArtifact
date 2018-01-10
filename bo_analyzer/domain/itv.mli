(** Interval domain *)
module Integer : sig
  type t = Int of int | MInf | PInf
  val minf : t
  val pinf : t
  val of_int : int -> t
  val le : t -> t -> bool
  val eq : t -> t -> bool
end

include AbsDom.LAT

val bot         : t
val top         : t

val is_bot      : t -> bool
val is_const    : t -> bool

val zero        : t
val one         : t
val nat         : t
val bot         : t
val top         : t
val pos         : t
val neg         : t

val of_int      : int -> t
val of_ints     : int -> int -> t 
val of_integer  : Integer.t -> Integer.t -> t

val lower : t -> int
val upper : t -> int
val lower_integer : t -> Integer.t
val upper_integer : t -> Integer.t

(** {2 Integer Arithmetic Operators } *)
val plus        : t -> t -> t
val minus       : t -> t -> t
val times       : t -> t -> t
val divide      : t -> t -> t 
val modulo      : t -> t -> t
val absolute    : t -> t

(** {2 Logical Operators } *)
val lt_itv : t -> t -> t
val gt_itv : t -> t -> t
val le_itv : t -> t -> t
val ge_itv : t -> t -> t
val eq_itv : t -> t -> t
val ne_itv : t -> t -> t

val l_not   : t -> t
val l_and   : t -> t -> t
val l_or    : t -> t -> t

(** {2 Bitwise Operators } *)
val l_shift : t -> t -> t
val r_shift : t -> t -> t 
val b_xor   : t -> t -> t
val b_or    : t -> t -> t
val b_and   : t -> t -> t
val b_not   : t -> t

(** {2 Casting } *)
val cast : Cil.typ -> Cil.typ -> t -> t

(** {2 Pruning } *)
val prune : Cil.binop -> t -> t -> t
