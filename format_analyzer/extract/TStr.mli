type string_t = string

module String_as_OT : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

