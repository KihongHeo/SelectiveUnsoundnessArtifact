type __ = Obj.t

module type KEY = 
 OrderedType.OrderedType

module type LAT = 
 sig 
  type t 
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
 end

type 't coq_TCLat = { le_dec : ('t -> 't -> bool);
                      eq_dec : ('t -> 't -> bool); bot : 't;
                      join : ('t -> 't -> 't); meet : ('t -> 't -> 't);
                      widen : ('t -> 't -> 't); narrow : ('t -> 't -> 't) }

val coq_TCLat_rect :
  (__ -> ('a1 -> 'a1 -> bool) -> __ -> ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 ->
  'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  'a1) -> 'a2) -> 'a1 coq_TCLat -> 'a2

val coq_TCLat_rec :
  (__ -> ('a1 -> 'a1 -> bool) -> __ -> ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 ->
  'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  'a1) -> 'a2) -> 'a1 coq_TCLat -> 'a2

val le_dec : 'a1 coq_TCLat -> 'a1 -> 'a1 -> bool

val eq_dec : 'a1 coq_TCLat -> 'a1 -> 'a1 -> bool

val bot : 'a1 coq_TCLat -> 'a1

val join : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1

val meet : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1

val widen : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1

val narrow : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1

