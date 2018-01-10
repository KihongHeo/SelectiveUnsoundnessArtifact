open Datatypes
open Peano

module Pos : 
 sig 
  type t = int
  
  val succ : int -> int
  
  val add : int -> int -> int
  
  val add_carry : int -> int -> int
  
  val pred_double : int -> int
  
  val pred : int -> int
  
  val pred_N : int -> int
  
  type mask =
  | IsNul
  | IsPos of int
  | IsNeg
  
  val mask_rect : 'a1 -> (int -> 'a1) -> 'a1 -> mask -> 'a1
  
  val mask_rec : 'a1 -> (int -> 'a1) -> 'a1 -> mask -> 'a1
  
  val succ_double_mask : mask -> mask
  
  val double_mask : mask -> mask
  
  val double_pred_mask : int -> mask
  
  val pred_mask : mask -> mask
  
  val sub_mask : int -> int -> mask
  
  val sub_mask_carry : int -> int -> mask
  
  val sub : int -> int -> int
  
  val mul : int -> int -> int
  
  val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1
  
  val pow : int -> int -> int
  
  val square : int -> int
  
  val div2 : int -> int
  
  val div2_up : int -> int
  
  val size_nat : int -> int
  
  val size : int -> int
  
  val compare_cont : int -> int -> comparison -> comparison
  
  val compare : int -> int -> comparison
  
  val min : int -> int -> int
  
  val max : int -> int -> int
  
  val eqb : int -> int -> bool
  
  val leb : int -> int -> bool
  
  val ltb : int -> int -> bool
  
  val sqrtrem_step :
    (int -> int) -> (int -> int) -> (int * mask) -> int * mask
  
  val sqrtrem : int -> int * mask
  
  val sqrt : int -> int
  
  val gcdn : int -> int -> int -> int
  
  val gcd : int -> int -> int
  
  val ggcdn : int -> int -> int -> int * (int * int)
  
  val ggcd : int -> int -> int * (int * int)
  
  val coq_Nsucc_double : int -> int
  
  val coq_Ndouble : int -> int
  
  val coq_lor : int -> int -> int
  
  val coq_land : int -> int -> int
  
  val ldiff : int -> int -> int
  
  val coq_lxor : int -> int -> int
  
  val shiftl_nat : int -> int -> int
  
  val shiftr_nat : int -> int -> int
  
  val shiftl : int -> int -> int
  
  val shiftr : int -> int -> int
  
  val testbit_nat : int -> int -> bool
  
  val testbit : int -> int -> bool
  
  val iter_op : ('a1 -> 'a1 -> 'a1) -> int -> 'a1 -> 'a1
  
  val to_nat : int -> int
  
  val of_nat : int -> int
  
  val of_succ_nat : int -> int
 end

