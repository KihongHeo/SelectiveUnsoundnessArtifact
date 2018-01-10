open TStr

type pos_t = { pos_file : string
; pos_line : int
; pos_id : int
}

module Pos_as_OT = 
 struct 
  type t = pos_t
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare = fun x y ->
  let c = compare x.pos_id y.pos_id in
  if c = 0 then OrderedType.EQ else
  if c < 0 then OrderedType.LT else
  OrderedType.GT
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec = fun x y -> x.pos_id = y.pos_id
  
  (** val unknown_pos : pos_t **)
  
  let unknown_pos = { pos_file = "__unknown_file"
; pos_line = -1
; pos_id = -1
}
  
  (** val string_of_pos : pos_t -> string_t **)
  
  let string_of_pos = fun p -> p.pos_file ^ ":" ^ string_of_int p.pos_line
 end

