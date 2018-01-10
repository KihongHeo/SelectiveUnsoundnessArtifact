open OrderedTypeEx

module Nat : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

