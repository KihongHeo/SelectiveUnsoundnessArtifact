open BinNat
open BinPos
open Bool
open Datatypes

type __ = Obj.t

module Z : 
 sig 
  type t = int
  
  val zero : int
  
  val one : int
  
  val two : int
  
  val double : int -> int
  
  val succ_double : int -> int
  
  val pred_double : int -> int
  
  val pos_sub : int -> int -> int
  
  val add : int -> int -> int
  
  val opp : int -> int
  
  val succ : int -> int
  
  val pred : int -> int
  
  val sub : int -> int -> int
  
  val mul : int -> int -> int
  
  val pow_pos : int -> int -> int
  
  val pow : int -> int -> int
  
  val square : int -> int
  
  val compare : int -> int -> comparison
  
  val sgn : int -> int
  
  val leb : int -> int -> bool
  
  val ltb : int -> int -> bool
  
  val geb : int -> int -> bool
  
  val gtb : int -> int -> bool
  
  val eqb : int -> int -> bool
  
  val max : int -> int -> int
  
  val min : int -> int -> int
  
  val abs : int -> int
  
  val abs_nat : int -> int
  
  val abs_N : int -> int
  
  val to_nat : int -> int
  
  val to_N : int -> int
  
  val of_nat : int -> int
  
  val of_N : int -> int
  
  val to_pos : int -> int
  
  val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1
  
  val pos_div_eucl : int -> int -> int * int
  
  val div_eucl : int -> int -> int * int
  
  val div : int -> int -> int
  
  val modulo : int -> int -> int
  
  val quotrem : int -> int -> int * int
  
  val quot : int -> int -> int
  
  val rem : int -> int -> int
  
  val even : int -> bool
  
  val odd : int -> bool
  
  val div2 : int -> int
  
  val quot2 : int -> int
  
  val log2 : int -> int
  
  val sqrtrem : int -> int * int
  
  val sqrt : int -> int
  
  val gcd : int -> int -> int
  
  val ggcd : int -> int -> int * (int * int)
  
  val testbit : int -> int -> bool
  
  val shiftl : int -> int -> int
  
  val shiftr : int -> int -> int
  
  val coq_lor : int -> int -> int
  
  val coq_land : int -> int -> int
  
  val ldiff : int -> int -> int
  
  val coq_lxor : int -> int -> int
  
  val eq_dec : int -> int -> bool
  
  module Private_BootStrap : 
   sig 
    
   end
  
  val leb_spec0 : int -> int -> reflect
  
  val ltb_spec0 : int -> int -> reflect
  
  module Private_OrderTac : 
   sig 
    module IsTotal : 
     sig 
      
     end
    
    module Tac : 
     sig 
      
     end
   end
  
  val sqrt_up : int -> int
  
  val log2_up : int -> int
  
  module Private_NZDiv : 
   sig 
    
   end
  
  module Private_Div : 
   sig 
    module Quot2Div : 
     sig 
      val div : int -> int -> int
      
      val modulo : int -> int -> int
     end
    
    module NZQuot : 
     sig 
      
     end
   end
  
  val lcm : int -> int -> int
  
  val eqb_spec : int -> int -> reflect
  
  val b2z : bool -> int
  
  val setbit : int -> int -> int
  
  val clearbit : int -> int -> int
  
  val lnot : int -> int
  
  val ones : int -> int
  
  module Private_Tac : 
   sig 
    
   end
  
  module Private_Dec : 
   sig 
    val max_case_strong :
      int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val max_case :
      int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val max_dec : int -> int -> bool
    
    val min_case_strong :
      int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ ->
      'a1) -> 'a1
    
    val min_case :
      int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1
    
    val min_dec : int -> int -> bool
   end
  
  val max_case_strong : int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val max_case : int -> int -> 'a1 -> 'a1 -> 'a1
  
  val max_dec : int -> int -> bool
  
  val min_case_strong : int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1
  
  val min_case : int -> int -> 'a1 -> 'a1 -> 'a1
  
  val min_dec : int -> int -> bool
 end

module Pos2Z : 
 sig 
  
 end

module Z2Pos : 
 sig 
  
 end

