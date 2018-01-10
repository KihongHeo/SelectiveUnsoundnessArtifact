open DLat
open DMap
open DPow

type __ = Obj.t

module type DMAP = 
 sig 
  type t 
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  module A : 
   KEY
  
  module B : 
   LAT
  
  module PowA : 
   POW with type elt = A.t
  
  val empty : t
  
  val is_empty : t -> bool
  
  val find : A.t -> t -> B.t
  
  val add : PowA.t -> A.t -> B.t -> t -> t
  
  val pre_add : PowA.t -> A.t -> B.t -> t -> t
  
  val main_add : PowA.t -> A.t -> B.t -> t -> t
  
  val weak_add : PowA.t -> A.t -> B.t -> t -> t
  
  val pre_weak_add : PowA.t -> A.t -> B.t -> t -> t
  
  val main_weak_add : PowA.t -> A.t -> B.t -> t -> t
  
  val remove : A.t -> t -> t
  
  val map : (B.t -> B.t) -> t -> t
  
  val mapi : (A.t -> B.t -> B.t) -> t -> t
  
  val filteri : (A.t -> B.t -> bool) -> t -> t
  
  val fold : (B.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val foldi : (A.t -> B.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val cardinal : t -> int
  
  val unstables :
    t -> t -> (B.t -> B.t -> bool) -> A.t list -> ((A.t * B.t) * B.t) list
  
  val meet_big_small : t -> t -> t
 end

module MemSum = 
 functor (A__1:KEY) ->
 functor (B__2:LAT) ->
 functor (PowA__3:POW with type elt = A__1.t) ->
 struct 
  module A = A__1
  
  module B = B__2
  
  module Mem = Map(A__1)(B__2)
  
  module PowA = PowA__3
  
  type t' = { base : Mem.t; spec : Mem.t }
  
  (** val t'_rect : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1 **)
  
  let t'_rect f t0 =
    let { base = x; spec = x0 } = t0 in f x x0
  
  (** val t'_rec : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1 **)
  
  let t'_rec f t0 =
    let { base = x; spec = x0 } = t0 in f x x0
  
  (** val base : t' -> Mem.t **)
  
  let base t0 =
    t0.base
  
  (** val spec : t' -> Mem.t **)
  
  let spec t0 =
    t0.spec
  
  type t = t'
  
  (** val all : t -> Mem.t **)
  
  let all m =
    Mem.coq_ILat.join (base m) (spec m)
  
  (** val empty : t **)
  
  let empty =
    { base = Mem.coq_IMap.map_empty; spec = Mem.coq_IMap.map_empty }
  
  (** val is_empty : t -> bool **)
  
  let is_empty x =
    (&&) (Mem.coq_IMap.map_is_empty (base x))
      (Mem.coq_IMap.map_is_empty (spec x))
  
  (** val find : A__1.t -> t -> B__2.t **)
  
  let find k m =
    B__2.join (Mem.coq_IMap.map_find k (base m))
      (Mem.coq_IMap.map_find k (spec m))
  
  (** val add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec = (Mem.coq_IMap.map_add k v (spec m)) }
    else { base = (Mem.coq_IMap.map_weak_add k v (base m)); spec = (spec m) }
  
  (** val pre_add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let pre_add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec = (Mem.coq_IMap.map_add k v (spec m)) }
    else { base = (Mem.coq_IMap.map_fast_weak_add k v (base m)); spec =
           (spec m) }
  
  (** val main_add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let main_add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec = (Mem.coq_IMap.map_add k v (spec m)) }
    else m
  
  (** val weak_add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let weak_add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec = (Mem.coq_IMap.map_weak_add k v (spec m)) }
    else { base = (Mem.coq_IMap.map_weak_add k v (base m)); spec = (spec m) }
  
  (** val pre_weak_add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let pre_weak_add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec =
           (Mem.coq_IMap.map_fast_weak_add k v (spec m)) }
    else { base = (Mem.coq_IMap.map_fast_weak_add k v (base m)); spec =
           (spec m) }
  
  (** val main_weak_add : PowA__3.t -> A__1.t -> B__2.t -> t -> t **)
  
  let main_weak_add locs k v m =
    if PowA__3.mem k locs
    then { base = (base m); spec = (Mem.coq_IMap.map_weak_add k v (spec m)) }
    else m
  
  (** val remove : A__1.t -> t -> t **)
  
  let remove k m =
    { base = (Mem.coq_IMap.map_remove k (base m)); spec =
      (Mem.coq_IMap.map_remove k (spec m)) }
  
  (** val map : (B__2.t -> B__2.t) -> t -> t **)
  
  let map f m =
    { base = (Mem.coq_IMap.map_map f (base m)); spec =
      (Mem.coq_IMap.map_map f (spec m)) }
  
  (** val mapi : (A__1.t -> B__2.t -> B__2.t) -> t -> t **)
  
  let mapi f m =
    { base = (Mem.coq_IMap.map_mapi f (base m)); spec =
      (Mem.coq_IMap.map_mapi f (spec m)) }
  
  (** val filteri : (A__1.t -> B__2.t -> bool) -> t -> t **)
  
  let filteri f m =
    { base = (base m); spec = (Mem.coq_IMap.map_filteri f (spec m)) }
  
  (** val fold : (B__2.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let fold f m acc =
    map_fold Mem.coq_IMap f (all m) acc
  
  (** val foldi : (A__1.t -> B__2.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let foldi f m acc =
    map_foldi Mem.coq_IMap f (all m) acc
  
  (** val cardinal : t -> int **)
  
  let cardinal m =
    Mem.cardinal (all m)
  
  (** val unstables :
      t -> t -> (B__2.t -> B__2.t -> bool) -> A__1.t list ->
      ((A__1.t * B__2.t) * B__2.t) list **)
  
  let unstables old_m new_m is_unstb candidate =
    Mem.unstables (spec old_m) (spec new_m) is_unstb candidate
  
  (** val meet_big_small : t -> t -> t **)
  
  let meet_big_small memFI y =
    { base = (base y); spec = (Mem.meet_big_small (spec memFI) (spec y)) }
  
  (** val le_than : t -> A__1.t -> B__2.t -> bool **)
  
  let le_than y k v =
    if B__2.le_dec v (find k y) then true else false
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    Mem.coq_ILat.le_dec (all x) (all y)
  
  (** val strong_le : t -> t -> bool **)
  
  let strong_le x y =
    if Mem.strong_le (base x) (base y)
    then if Mem.strong_le (spec x) (spec y)
         then true
         else if Mem.le_dec (spec x) (spec y) then true else false
    else false
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if le_dec x y then le_dec y x else false
  
  (** val bot : t **)
  
  let bot =
    empty
  
  (** val join : t -> t -> t **)
  
  let join x y =
    { base = (Mem.coq_ILat.join (base x) (base y)); spec =
      (Mem.coq_ILat.join (spec x) (spec y)) }
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    let z = Mem.meet (all x) (all y) in { base = z; spec = z }
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    { base = (Mem.coq_ILat.widen (base x) (base y)); spec =
      (Mem.coq_ILat.widen (spec x) (spec y)) }
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    { base = (Mem.coq_ILat.narrow (base x) (base y)); spec =
      (Mem.coq_ILat.narrow (spec x) (spec y)) }
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

