open TStr

module DStr : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

