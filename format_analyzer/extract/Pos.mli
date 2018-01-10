open TStr

type pos_t = { pos_file : string
; pos_line : int
; pos_id : int
}

module Pos_as_OT : 
 sig 
  type t = pos_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val unknown_pos : pos_t
  
  val string_of_pos : pos_t -> string_t
 end

