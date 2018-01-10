open DFMapAVL
open DLat
open TStr
open VocabA
open Zcomplements

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val map_narrow_msg : string_t **)

let map_narrow_msg = "narrow small big"

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
  
  val empty : t
  
  val is_empty : t -> bool
  
  val find : A.t -> t -> B.t
  
  val add : A.t -> B.t -> t -> t
  
  val weak_add : A.t -> B.t -> t -> t
  
  val fast_weak_add : A.t -> B.t -> t -> t
  
  val remove : A.t -> t -> t
  
  val map : (B.t -> B.t) -> t -> t
  
  val mapi : (A.t -> B.t -> B.t) -> t -> t
  
  val filteri : (A.t -> B.t -> bool) -> t -> t
  
  val fold : (B.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val foldi : (A.t -> B.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val elements : t -> (A.t * B.t) list
  
  val cardinal : t -> int
  
  val for_all : (A.t -> B.t -> unit) -> (A.t -> B.t -> bool) -> t -> bool
  
  val unstables :
    t -> t -> (B.t -> B.t -> bool) -> A.t list -> ((A.t * B.t) * B.t) list
  
  val meet_big_small : t -> t -> t
 end

type ('k, 'v, 'm) coq_TCMap = { map_empty : 'm; map_is_empty : ('m -> bool);
                                map_find : ('k -> 'm -> 'v);
                                map_add : ('k -> 'v -> 'm -> 'm);
                                map_weak_add : ('k -> 'v -> 'm -> 'm);
                                map_fast_weak_add : ('k -> 'v -> 'm -> 'm);
                                map_remove : ('k -> 'm -> 'm);
                                map_map : (('v -> 'v) -> 'm -> 'm);
                                map_mapi : (('k -> 'v -> 'v) -> 'm -> 'm);
                                map_filteri : (('k -> 'v -> bool) -> 'm ->
                                              'm);
                                map_fold : (__ -> ('v -> __ -> __) -> 'm ->
                                           __ -> __);
                                map_foldi : (__ -> ('k -> 'v -> __ -> __) ->
                                            'm -> __ -> __);
                                map_elements : ('m -> ('k * 'v) list);
                                map_cardinal : ('m -> int);
                                map_for_all : (('k -> 'v -> unit) -> ('k ->
                                              'v -> bool) -> 'm -> bool) }

(** val coq_TCMap_rect :
    ('a3 -> ('a3 -> bool) -> ('a1 -> 'a3 -> 'a2) -> ('a1 -> 'a2 -> 'a3 ->
    'a3) -> ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1
    -> 'a3 -> 'a3) -> (('a2 -> 'a2) -> 'a3 -> 'a3) -> (('a1 -> 'a2 -> 'a2) ->
    'a3 -> 'a3) -> (('a1 -> 'a2 -> bool) -> 'a3 -> 'a3) -> (__ -> ('a2 -> __
    -> __) -> 'a3 -> __ -> __) -> (__ -> ('a1 -> 'a2 -> __ -> __) -> 'a3 ->
    __ -> __) -> ('a3 -> ('a1 * 'a2) list) -> ('a3 -> int) -> (('a1 -> 'a2 ->
    unit) -> ('a1 -> 'a2 -> bool) -> 'a3 -> bool) -> 'a4) -> ('a1, 'a2, 'a3)
    coq_TCMap -> 'a4 **)

let coq_TCMap_rect f t0 =
  let { map_empty = x; map_is_empty = x0; map_find = x1; map_add = x2;
    map_weak_add = x3; map_fast_weak_add = x4; map_remove = x5; map_map = x6;
    map_mapi = x7; map_filteri = x8; map_fold = x9; map_foldi = x10;
    map_elements = x11; map_cardinal = x12; map_for_all = x13 } = t0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

(** val coq_TCMap_rec :
    ('a3 -> ('a3 -> bool) -> ('a1 -> 'a3 -> 'a2) -> ('a1 -> 'a2 -> 'a3 ->
    'a3) -> ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1
    -> 'a3 -> 'a3) -> (('a2 -> 'a2) -> 'a3 -> 'a3) -> (('a1 -> 'a2 -> 'a2) ->
    'a3 -> 'a3) -> (('a1 -> 'a2 -> bool) -> 'a3 -> 'a3) -> (__ -> ('a2 -> __
    -> __) -> 'a3 -> __ -> __) -> (__ -> ('a1 -> 'a2 -> __ -> __) -> 'a3 ->
    __ -> __) -> ('a3 -> ('a1 * 'a2) list) -> ('a3 -> int) -> (('a1 -> 'a2 ->
    unit) -> ('a1 -> 'a2 -> bool) -> 'a3 -> bool) -> 'a4) -> ('a1, 'a2, 'a3)
    coq_TCMap -> 'a4 **)

let coq_TCMap_rec f t0 =
  let { map_empty = x; map_is_empty = x0; map_find = x1; map_add = x2;
    map_weak_add = x3; map_fast_weak_add = x4; map_remove = x5; map_map = x6;
    map_mapi = x7; map_filteri = x8; map_fold = x9; map_foldi = x10;
    map_elements = x11; map_cardinal = x12; map_for_all = x13 } = t0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13

(** val map_empty : ('a1, 'a2, 'a3) coq_TCMap -> 'a3 **)

let map_empty x = x.map_empty

(** val map_is_empty : ('a1, 'a2, 'a3) coq_TCMap -> 'a3 -> bool **)

let map_is_empty x = x.map_is_empty

(** val map_find : ('a1, 'a2, 'a3) coq_TCMap -> 'a1 -> 'a3 -> 'a2 **)

let map_find x = x.map_find

(** val map_add : ('a1, 'a2, 'a3) coq_TCMap -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let map_add x = x.map_add

(** val map_weak_add :
    ('a1, 'a2, 'a3) coq_TCMap -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let map_weak_add x = x.map_weak_add

(** val map_fast_weak_add :
    ('a1, 'a2, 'a3) coq_TCMap -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let map_fast_weak_add x = x.map_fast_weak_add

(** val map_remove : ('a1, 'a2, 'a3) coq_TCMap -> 'a1 -> 'a3 -> 'a3 **)

let map_remove x = x.map_remove

(** val map_map : ('a1, 'a2, 'a3) coq_TCMap -> ('a2 -> 'a2) -> 'a3 -> 'a3 **)

let map_map x = x.map_map

(** val map_mapi :
    ('a1, 'a2, 'a3) coq_TCMap -> ('a1 -> 'a2 -> 'a2) -> 'a3 -> 'a3 **)

let map_mapi x = x.map_mapi

(** val map_filteri :
    ('a1, 'a2, 'a3) coq_TCMap -> ('a1 -> 'a2 -> bool) -> 'a3 -> 'a3 **)

let map_filteri x = x.map_filteri

(** val map_fold :
    ('a1, 'a2, 'a3) coq_TCMap -> ('a2 -> 'a4 -> 'a4) -> 'a3 -> 'a4 -> 'a4 **)

let map_fold tCMap x x0 x1 =
  let { map_empty = map_empty0; map_is_empty = map_is_empty0; map_find =
    map_find0; map_add = map_add0; map_weak_add = map_weak_add0;
    map_fast_weak_add = map_fast_weak_add0; map_remove = map_remove0;
    map_map = map_map0; map_mapi = map_mapi0; map_filteri = map_filteri0;
    map_fold = map_fold0; map_foldi = map_foldi0; map_elements =
    map_elements0; map_cardinal = map_cardinal0; map_for_all =
    map_for_all0 } = tCMap
  in
  Obj.magic map_fold0 __ x x0 x1

(** val map_foldi :
    ('a1, 'a2, 'a3) coq_TCMap -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a3 -> 'a4 ->
    'a4 **)

let map_foldi tCMap x x0 x1 =
  let { map_empty = map_empty0; map_is_empty = map_is_empty0; map_find =
    map_find0; map_add = map_add0; map_weak_add = map_weak_add0;
    map_fast_weak_add = map_fast_weak_add0; map_remove = map_remove0;
    map_map = map_map0; map_mapi = map_mapi0; map_filteri = map_filteri0;
    map_fold = map_fold0; map_foldi = map_foldi0; map_elements =
    map_elements0; map_cardinal = map_cardinal0; map_for_all =
    map_for_all0 } = tCMap
  in
  Obj.magic map_foldi0 __ x x0 x1

(** val map_elements :
    ('a1, 'a2, 'a3) coq_TCMap -> 'a3 -> ('a1 * 'a2) list **)

let map_elements x = x.map_elements

(** val map_cardinal : ('a1, 'a2, 'a3) coq_TCMap -> 'a3 -> int **)

let map_cardinal x = x.map_cardinal

(** val map_for_all :
    ('a1, 'a2, 'a3) coq_TCMap -> ('a1 -> 'a2 -> unit) -> ('a1 -> 'a2 -> bool)
    -> 'a3 -> bool **)

let map_for_all x = x.map_for_all

module Map = 
 functor (A__1:KEY) ->
 functor (B__2:LAT) ->
 struct 
  module A = A__1
  
  module B = B__2
  
  module M = FMapAVL'.Make(A__1)
  
  type t = B__2.t M.t
  
  (** val empty : t **)
  
  let empty =
    M.empty
  
  (** val is_empty : t -> bool **)
  
  let is_empty x =
    M.is_empty x
  
  (** val find : A__1.t -> t -> B__2.t **)
  
  let find k m =
    match M.find k m with
    | Some v -> v
    | None -> B__2.bot
  
  (** val add : A__1.t -> B__2.t -> t -> t **)
  
  let add k v m =
    M.add k v m
  
  (** val weak_add : A__1.t -> B__2.t -> t -> t **)
  
  let weak_add k v m =
    let orig_v = find k m in
    if B__2.le_dec v orig_v then m else M.add k (B__2.join orig_v v) m
  
  (** val fast_weak_add : A__1.t -> B__2.t -> t -> t **)
  
  let fast_weak_add k v m =
    let orig_v = find k m in M.add k (B__2.join orig_v v) m
  
  (** val remove : A__1.t -> t -> t **)
  
  let remove k m =
    M.remove k m
  
  (** val map : (B__2.t -> B__2.t) -> t -> t **)
  
  let map f m =
    M.map (fun k -> f k) m
  
  (** val mapi : (A__1.t -> B__2.t -> B__2.t) -> t -> t **)
  
  let mapi f m =
    M.mapi (fun k v -> f k v) m
  
  (** val fold : (B__2.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let fold f m acc =
    let f' = fun x v acc0 -> f v acc0 in M.fold f' m acc
  
  (** val foldi : (A__1.t -> B__2.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let foldi f m acc =
    M.fold f m acc
  
  (** val filteri : (A__1.t -> B__2.t -> bool) -> t -> t **)
  
  let filteri f m =
    let iter_elem = fun k v acc -> if f k v then add k v acc else acc in
    foldi iter_elem m empty
  
  (** val elements : t -> (A__1.t * B__2.t) list **)
  
  let elements m =
    M.elements m
  
  (** val cardinal : t -> int **)
  
  let cardinal m =
    coq_Zlength (M.elements m)
  
  (** val le_than : t -> A__1.t -> B__2.t -> bool **)
  
  let le_than y k v =
    if B__2.le_dec v (find k y) then true else false
  
  (** val for_all :
      (A__1.t -> B__2.t -> unit) -> (A__1.t -> B__2.t -> bool) -> t -> bool **)
  
  let for_all print cond m =
    M.for_all print cond m
  
  (** val unstables :
      t -> t -> (B__2.t -> B__2.t -> bool) -> A__1.t list ->
      ((A__1.t * B__2.t) * B__2.t) list **)
  
  let rec unstables old_m new_m is_unstb = function
  | [] -> []
  | k :: tl ->
    let old_v = find k old_m in
    let new_v = find k new_m in
    if is_unstb old_v new_v
    then ((k, old_v), new_v) :: (unstables old_m new_m is_unstb tl)
    else unstables old_m new_m is_unstb tl
  
  (** val meet_big_small : t -> t -> t **)
  
  let meet_big_small x y =
    let iter1 = fun k v -> add k (B__2.meet (find k x) v) in foldi iter1 y y
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else if for_all print2 (le_than y) x then true else false
  
  (** val strong_le : t -> t -> bool **)
  
  let strong_le x y =
    M.strong_le (fun x0 y0 -> if B__2.le_dec x0 y0 then true else false) x y
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if le_dec x y then le_dec y x else false
  
  (** val bot : t **)
  
  let bot =
    empty
  
  (** val join' : B__2.t option -> B__2.t option -> B__2.t option **)
  
  let join' opt_v1 opt_v2 =
    match opt_v1 with
    | Some v1 ->
      (match opt_v2 with
       | Some v2 ->
         let joined_v = B__2.join v1 v2 in
         if B__2.eq_dec joined_v B__2.bot then None else Some joined_v
       | None -> if B__2.eq_dec v1 B__2.bot then None else Some v1)
    | None ->
      (match opt_v2 with
       | Some v -> if B__2.eq_dec v B__2.bot then None else Some v
       | None -> None)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    M.map2 join' x y
  
  (** val meet' : B__2.t option -> B__2.t option -> B__2.t option **)
  
  let meet' opt_v1 opt_v2 =
    match opt_v1 with
    | Some v1 ->
      (match opt_v2 with
       | Some v2 ->
         let meeted_v = B__2.meet v1 v2 in
         if B__2.eq_dec meeted_v B__2.bot then None else Some meeted_v
       | None -> None)
    | None -> None
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    M.map2 meet' x y
  
  (** val widen' : B__2.t option -> B__2.t option -> B__2.t option **)
  
  let widen' opt_v1 opt_v2 =
    match opt_v1 with
    | Some v1 ->
      (match opt_v2 with
       | Some v2 ->
         let widened_v = B__2.widen v1 v2 in
         if B__2.eq_dec widened_v B__2.bot then None else Some widened_v
       | None -> if B__2.eq_dec v1 B__2.bot then None else Some v1)
    | None ->
      (match opt_v2 with
       | Some v -> if B__2.eq_dec v B__2.bot then None else Some v
       | None -> None)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    M.map2 widen' x y
  
  (** val narrow' : B__2.t option -> B__2.t option -> B__2.t option **)
  
  let narrow' opt_v1 opt_v2 =
    match opt_v1 with
    | Some v1 ->
      (match opt_v2 with
       | Some v2 ->
         let narrowed_v = B__2.narrow v1 v2 in
         if B__2.eq_dec narrowed_v B__2.bot then None else Some narrowed_v
       | None -> None)
    | None ->
      (match opt_v2 with
       | Some v ->
         if B__2.eq_dec v B__2.bot then None else invalid_arg map_narrow_msg
       | None -> None)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    M.map2 narrow' x y
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val coq_IMap : (A__1.t, B__2.t, t) coq_TCMap **)
  
  let coq_IMap =
    { map_empty = empty; map_is_empty = is_empty; map_find = find; map_add =
      add; map_weak_add = weak_add; map_fast_weak_add = fast_weak_add;
      map_remove = remove; map_map = map; map_mapi = mapi; map_filteri =
      filteri; map_fold = (fun _ -> fold); map_foldi = (fun _ -> foldi);
      map_elements = elements; map_cardinal = cardinal; map_for_all =
      for_all }
 end

