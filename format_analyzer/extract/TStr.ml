type string_t = string

module String_as_OT = 
 struct 
  type t = string_t
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare = fun x y ->
  let c = compare x y in
  if c = 0 then OrderedType.EQ else
  if c < 0 then OrderedType.LT else
  OrderedType.GT
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = (=)
 end

