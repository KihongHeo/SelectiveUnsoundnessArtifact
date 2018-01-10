module Unit = 
 struct 
  type t = unit
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    OrderedType.EQ
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    true
 end

