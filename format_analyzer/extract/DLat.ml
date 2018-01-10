type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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

(** val coq_TCLat_rect :
    (__ -> ('a1 -> 'a1 -> bool) -> __ -> ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1
    -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 ->
    'a1 -> 'a1) -> 'a2) -> 'a1 coq_TCLat -> 'a2 **)

let coq_TCLat_rect f t0 =
  let { le_dec = x; eq_dec = x0; bot = x1; join = x2; meet = x3; widen = x4;
    narrow = x5 } = t0
  in
  f __ x __ x0 x1 x2 x3 x4 x5

(** val coq_TCLat_rec :
    (__ -> ('a1 -> 'a1 -> bool) -> __ -> ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1
    -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 ->
    'a1 -> 'a1) -> 'a2) -> 'a1 coq_TCLat -> 'a2 **)

let coq_TCLat_rec f t0 =
  let { le_dec = x; eq_dec = x0; bot = x1; join = x2; meet = x3; widen = x4;
    narrow = x5 } = t0
  in
  f __ x __ x0 x1 x2 x3 x4 x5

(** val le_dec : 'a1 coq_TCLat -> 'a1 -> 'a1 -> bool **)

let le_dec x = x.le_dec

(** val eq_dec : 'a1 coq_TCLat -> 'a1 -> 'a1 -> bool **)

let eq_dec x = x.eq_dec

(** val bot : 'a1 coq_TCLat -> 'a1 **)

let bot x = x.bot

(** val join : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1 **)

let join x = x.join

(** val meet : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1 **)

let meet x = x.meet

(** val widen : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1 **)

let widen x = x.widen

(** val narrow : 'a1 coq_TCLat -> 'a1 -> 'a1 -> 'a1 **)

let narrow x = x.narrow

