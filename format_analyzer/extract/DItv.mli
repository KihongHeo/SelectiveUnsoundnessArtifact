open BinInt
open DLat
open List0
open Sumbool
open TStr
open VocabA
open ZArith_dec

type __ = Obj.t

module Itv : 
 sig 
  val threshold : int list
  
  type t' =
  | Int of int
  | PInf
  | MInf
  
  val t'_rect : (int -> 'a1) -> 'a1 -> 'a1 -> t' -> 'a1
  
  val t'_rec : (int -> 'a1) -> 'a1 -> 'a1 -> t' -> 'a1
  
  val le'_dec : t' -> t' -> bool
  
  val eq'_dec : t' -> t' -> bool
  
  val min' : t' -> t' -> t'
  
  val max' : t' -> t' -> t'
  
  val lower_widen' : t' -> t' -> t'
  
  val upper_widen' : t' -> t' -> t'
  
  val lower_narrow_msg : string_t
  
  val lower_narrow' : t' -> t' -> t'
  
  val upper_narrow_msg : string_t
  
  val upper_narrow' : t' -> t' -> t'
  
  val plus' : t' -> t' -> t'
  
  val plus'_one : t' -> t'
  
  val minus' : t' -> t' -> t'
  
  val minus'_one : t' -> t'
  
  val times' : t' -> t' -> t'
  
  val divide' : t' -> t' -> t'
  
  val min4' : t' -> t' -> t' -> t' -> t'
  
  val max4' : t' -> t' -> t' -> t' -> t'
  
  type t'' =
  | V of t' * t'
  | Bot
  
  val t''_rect : (t' -> t' -> __ -> __ -> __ -> 'a1) -> 'a1 -> t'' -> 'a1
  
  val t''_rec : (t' -> t' -> __ -> __ -> __ -> 'a1) -> 'a1 -> t'' -> 'a1
  
  type t = t''
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val of_int : int -> t
  
  val of_ints : int -> int -> t
  
  val of_lb : int -> t
  
  val of_ub : int -> t
  
  val zero : t
  
  val false_itv : t
  
  val true_itv : t
  
  val unknown_bool : t
  
  val pos : t
  
  val neg : t
  
  val zero_pos : t
  
  val join : t -> t -> t
  
  val gen_itv : t' -> t' -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow_msg : string_t
  
  val narrow : t -> t -> t
  
  val is_const : t -> bool
  
  val diff : t -> int
  
  val plus : t -> t -> t
  
  val minus : t -> t -> t
  
  val times : t -> t -> t
  
  val divide : t -> t -> t
  
  val and_itv : t -> t -> t
  
  val or_itv : t -> t -> t
  
  val eq_itv : t -> t -> t
  
  val ne_itv : t -> t -> t
  
  val lt_itv : t -> t -> t
  
  val le_itv : t -> t -> t
  
  val gt_itv : t -> t -> t
  
  val ge_itv : t -> t -> t
  
  val not_itv : t -> t
  
  val unknown_binary : t -> t -> t
  
  val unknown_unary : t -> t
  
  val l_shift_itv : t -> t -> t
  
  val r_shift_itv : t -> t -> t
  
  val b_xor_itv : t -> t -> t
  
  val b_or_itv : t -> t -> t
  
  val b_and_itv : t -> t -> t
  
  val mod_itv : t -> t -> t
  
  val b_not_itv : t -> t
  
  val coq_ILat : t coq_TCLat
 end

