open Pos
open TStr

module DPos : 
 sig 
  type t = pos_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val unknown_pos : pos_t
  
  val string_of_pos : pos_t -> string_t
 end

