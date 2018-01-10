open DLat
open DPow
open Datatypes
open Sumbool
open VocabA

module Access = 
 functor (PowLoc:POW) ->
 struct 
  type t = PowLoc.t * PowLoc.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in
         sumbool_and (PowLoc.le_dec x1 y1) (PowLoc.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in
         sumbool_and (PowLoc.eq_dec x1 y1) (PowLoc.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (PowLoc.bot, PowLoc.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((PowLoc.join (fst x) (fst y)), (PowLoc.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((PowLoc.meet (fst x) (fst y)), (PowLoc.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((PowLoc.widen (fst x) (fst y)), (PowLoc.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((PowLoc.narrow (fst x) (fst y)), (PowLoc.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val fst : ('a1 * 'a2) -> 'a1 **)
  
  let fst =
    fst
  
  (** val snd : ('a1 * 'a2) -> 'a2 **)
  
  let snd =
    snd
  
  type access_mode =
  | DEF
  | USE
  | ALL
  
  (** val access_mode_rect : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1 **)
  
  let access_mode_rect f f0 f1 = function
  | DEF -> f
  | USE -> f0
  | ALL -> f1
  
  (** val access_mode_rec : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1 **)
  
  let access_mode_rec f f0 f1 = function
  | DEF -> f
  | USE -> f0
  | ALL -> f1
  
  (** val defof : t -> PowLoc.t **)
  
  let defof =
    fst
  
  (** val useof : t -> PowLoc.t **)
  
  let useof =
    snd
  
  (** val accessof : t -> PowLoc.t **)
  
  let accessof a =
    PowLoc.join (defof a) (useof a)
  
  (** val empty : t **)
  
  let empty =
    bot
  
  (** val add : access_mode -> PowLoc.elt -> t -> t **)
  
  let add m l a =
    match m with
    | DEF -> ((PowLoc.add l (defof a)), (useof a))
    | USE -> ((defof a), (PowLoc.add l (useof a)))
    | ALL -> ((PowLoc.add l (defof a)), (PowLoc.add l (useof a)))
  
  (** val add_set : access_mode -> PowLoc.t -> t -> t **)
  
  let add_set m ls a =
    match m with
    | DEF -> ((PowLoc.join (defof a) ls), (useof a))
    | USE -> ((defof a), (PowLoc.join (useof a) ls))
    | ALL -> ((PowLoc.join (defof a) ls), (PowLoc.join (useof a) ls))
  
  (** val singleton : access_mode -> PowLoc.elt -> t **)
  
  let singleton m l =
    match m with
    | DEF -> ((PowLoc.singleton l), PowLoc.empty)
    | USE -> (PowLoc.empty, (PowLoc.singleton l))
    | ALL -> ((PowLoc.singleton l), (PowLoc.singleton l))
  
  (** val from_set : access_mode -> PowLoc.t -> t **)
  
  let from_set m ls =
    match m with
    | DEF -> (ls, PowLoc.empty)
    | USE -> (PowLoc.empty, ls)
    | ALL -> (ls, ls)
  
  (** val mem : PowLoc.elt -> t -> bool **)
  
  let mem l a =
    (||) (PowLoc.mem l (defof a)) (PowLoc.mem l (useof a))
  
  (** val remove : PowLoc.elt -> t -> t **)
  
  let remove l a =
    ((PowLoc.remove l (defof a)), (PowLoc.remove l (useof a)))
  
  (** val remove_set : PowLoc.t -> t -> t **)
  
  let remove_set ls a =
    ((PowLoc.diff (defof a) ls), (PowLoc.diff (useof a) ls))
  
  (** val add_list : access_mode -> PowLoc.elt list -> t -> t **)
  
  let add_list m ls a =
    list_fold (add m) ls a
  
  (** val union : t -> t -> t **)
  
  let union a b =
    ((PowLoc.join (defof a) (defof b)), (PowLoc.join (useof a) (useof b)))
  
  (** val restrict : PowLoc.t -> t -> t **)
  
  let restrict ls a =
    ((PowLoc.meet (defof a) ls), (PowLoc.meet (useof a) ls))
  
  (** val filter_out : PowLoc.t -> t -> t **)
  
  let filter_out ls a =
    ((PowLoc.diff (defof a) ls), (PowLoc.diff (useof a) ls))
 end

