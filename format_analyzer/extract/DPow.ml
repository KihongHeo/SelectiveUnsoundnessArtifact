open DFSetAVL
open DLat
open Datatypes
open VocabA

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type ('e, 's) coq_TCSet = { set_empty : 's; set_is_empty : ('s -> bool);
                            set_add : ('e -> 's -> 's);
                            set_singleton : ('e -> 's);
                            set_mem : ('e -> 's -> bool);
                            set_remove : ('e -> 's -> 's);
                            set_union : ('s -> 's -> 's);
                            set_union_small_big : ('s -> 's -> 's);
                            set_intersect : ('s -> 's -> 's);
                            set_diff : ('s -> 's -> 's);
                            set_subset : ('s -> 's -> bool);
                            set_filter : (('e -> bool) -> 's -> 's);
                            set_fold : (__ -> ('e -> __ -> __) -> 's -> __ ->
                                       __);
                            set_iter : (('e -> unit) -> 's -> unit);
                            set_elements : ('s -> 'e list);
                            set_cardinal : ('s -> int);
                            set_choose : ('s -> 'e option);
                            set_choose_only : ('s -> 'e option);
                            set_for_all : (('e -> unit) -> ('e -> bool) -> 's
                                          -> bool) }

(** val coq_TCSet_rect :
    ('a2 -> ('a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a1 -> 'a2) -> ('a1 ->
    'a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2
    -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 ->
    bool) -> (('a1 -> bool) -> 'a2 -> 'a2) -> (__ -> ('a1 -> __ -> __) -> 'a2
    -> __ -> __) -> (('a1 -> unit) -> 'a2 -> unit) -> ('a2 -> 'a1 list) ->
    ('a2 -> int) -> ('a2 -> 'a1 option) -> ('a2 -> 'a1 option) -> (('a1 ->
    unit) -> ('a1 -> bool) -> 'a2 -> bool) -> 'a3) -> ('a1, 'a2) coq_TCSet ->
    'a3 **)

let coq_TCSet_rect f t0 =
  let { set_empty = x; set_is_empty = x0; set_add = x1; set_singleton = x2;
    set_mem = x3; set_remove = x4; set_union = x5; set_union_small_big = x6;
    set_intersect = x7; set_diff = x8; set_subset = x9; set_filter = x10;
    set_fold = x11; set_iter = x12; set_elements = x13; set_cardinal = x14;
    set_choose = x15; set_choose_only = x16; set_for_all = x17 } = t0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17

(** val coq_TCSet_rec :
    ('a2 -> ('a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a1 -> 'a2) -> ('a1 ->
    'a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2
    -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 ->
    bool) -> (('a1 -> bool) -> 'a2 -> 'a2) -> (__ -> ('a1 -> __ -> __) -> 'a2
    -> __ -> __) -> (('a1 -> unit) -> 'a2 -> unit) -> ('a2 -> 'a1 list) ->
    ('a2 -> int) -> ('a2 -> 'a1 option) -> ('a2 -> 'a1 option) -> (('a1 ->
    unit) -> ('a1 -> bool) -> 'a2 -> bool) -> 'a3) -> ('a1, 'a2) coq_TCSet ->
    'a3 **)

let coq_TCSet_rec f t0 =
  let { set_empty = x; set_is_empty = x0; set_add = x1; set_singleton = x2;
    set_mem = x3; set_remove = x4; set_union = x5; set_union_small_big = x6;
    set_intersect = x7; set_diff = x8; set_subset = x9; set_filter = x10;
    set_fold = x11; set_iter = x12; set_elements = x13; set_cardinal = x14;
    set_choose = x15; set_choose_only = x16; set_for_all = x17 } = t0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17

(** val set_empty : ('a1, 'a2) coq_TCSet -> 'a2 **)

let set_empty x = x.set_empty

(** val set_is_empty : ('a1, 'a2) coq_TCSet -> 'a2 -> bool **)

let set_is_empty x = x.set_is_empty

(** val set_add : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> 'a2 **)

let set_add x = x.set_add

(** val set_singleton : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 **)

let set_singleton x = x.set_singleton

(** val set_mem : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> bool **)

let set_mem x = x.set_mem

(** val set_remove : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> 'a2 **)

let set_remove x = x.set_remove

(** val set_union : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2 **)

let set_union x = x.set_union

(** val set_union_small_big : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2 **)

let set_union_small_big x = x.set_union_small_big

(** val set_intersect : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2 **)

let set_intersect x = x.set_intersect

(** val set_diff : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2 **)

let set_diff x = x.set_diff

(** val set_subset : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> bool **)

let set_subset x = x.set_subset

(** val set_filter : ('a1, 'a2) coq_TCSet -> ('a1 -> bool) -> 'a2 -> 'a2 **)

let set_filter x = x.set_filter

(** val set_fold :
    ('a1, 'a2) coq_TCSet -> ('a1 -> 'a3 -> 'a3) -> 'a2 -> 'a3 -> 'a3 **)

let set_fold tCSet x x0 x1 =
  let { set_empty = set_empty0; set_is_empty = set_is_empty0; set_add =
    set_add0; set_singleton = set_singleton0; set_mem = set_mem0;
    set_remove = set_remove0; set_union = set_union0; set_union_small_big =
    set_union_small_big0; set_intersect = set_intersect0; set_diff =
    set_diff0; set_subset = set_subset0; set_filter = set_filter0; set_fold =
    set_fold0; set_iter = set_iter0; set_elements = set_elements0;
    set_cardinal = set_cardinal0; set_choose = set_choose0; set_choose_only =
    set_choose_only0; set_for_all = set_for_all0 } = tCSet
  in
  Obj.magic set_fold0 __ x x0 x1

(** val set_iter : ('a1, 'a2) coq_TCSet -> ('a1 -> unit) -> 'a2 -> unit **)

let set_iter x = x.set_iter

(** val set_elements : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 list **)

let set_elements x = x.set_elements

(** val set_cardinal : ('a1, 'a2) coq_TCSet -> 'a2 -> int **)

let set_cardinal x = x.set_cardinal

(** val set_choose : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 option **)

let set_choose x = x.set_choose

(** val set_choose_only : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 option **)

let set_choose_only x = x.set_choose_only

(** val set_for_all :
    ('a1, 'a2) coq_TCSet -> ('a1 -> unit) -> ('a1 -> bool) -> 'a2 -> bool **)

let set_for_all x = x.set_for_all

module type POW = 
 sig 
  type t 
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  type elt 
  
  val empty : t
  
  val is_empty : t -> bool
  
  val add : elt -> t -> t
  
  val singleton : elt -> t
  
  val mem : elt -> t -> bool
  
  val remove : elt -> t -> t
  
  val union : t -> t -> t
  
  val union_small_big : t -> t -> t
  
  val intersect : t -> t -> t
  
  val diff : t -> t -> t
  
  val subset : t -> t -> bool
  
  val filter : (elt -> bool) -> t -> t
  
  val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val iter : (elt -> unit) -> t -> unit
  
  val elements : t -> elt list
  
  val cardinal : t -> int
  
  val choose : t -> elt option
  
  val choose_only : t -> elt option
  
  val for_all : (elt -> unit) -> (elt -> bool) -> t -> bool
 end

module Pow = 
 functor (A__1:KEY) ->
 struct 
  module A = A__1
  
  module SS = FSetAVL'.Make(A__1)
  
  type elt = A__1.t
  
  type t = SS.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y then true else if SS.subset x y then true else false
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if le_dec x y then le_dec y x else false
  
  (** val bot : t **)
  
  let bot =
    SS.empty
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y then y else if le_dec y x then x else SS.union x y
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y then x else if le_dec y x then y else SS.inter x y
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y then x else SS.union x y
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y then x else SS.inter x y
  
  (** val empty : t **)
  
  let empty =
    bot
  
  (** val is_empty : SS.t -> bool **)
  
  let is_empty =
    SS.is_empty
  
  (** val add : A__1.t -> t -> t **)
  
  let add e x =
    SS.add e x
  
  (** val singleton : A__1.t -> t **)
  
  let singleton e =
    SS.singleton e
  
  (** val mem : SS.elt -> SS.t -> bool **)
  
  let mem =
    SS.mem
  
  (** val remove : A__1.t -> t -> t **)
  
  let remove e x =
    SS.remove e x
  
  (** val union : SS.t -> SS.t -> SS.t **)
  
  let union =
    SS.union
  
  (** val union_small_big : SS.t -> SS.t -> SS.t **)
  
  let union_small_big small big =
    SS.fold SS.add small big
  
  (** val intersect : SS.t -> SS.t -> SS.t **)
  
  let intersect =
    SS.inter
  
  (** val diff : SS.t -> SS.t -> SS.t **)
  
  let diff =
    SS.diff
  
  (** val subset : SS.t -> SS.t -> bool **)
  
  let subset =
    SS.subset
  
  (** val filter : (A__1.t -> bool) -> t -> t **)
  
  let filter f x =
    SS.filter f x
  
  (** val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1 **)
  
  let fold x x0 x1 =
    SS.fold x x0 x1
  
  (** val iter : (A__1.t -> unit) -> t -> unit **)
  
  let iter f s =
    SS.fold (fun x acc -> acc) s ()
  
  (** val elements : SS.t -> SS.elt list **)
  
  let elements =
    SS.elements
  
  (** val cardinal : SS.t -> int **)
  
  let cardinal =
    SS.cardinal
  
  (** val choose : SS.t -> SS.elt option **)
  
  let choose =
    SS.choose
  
  (** val choose_only : SS.t -> SS.elt option **)
  
  let choose_only =
    SS.choose_only
  
  (** val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool **)
  
  let for_all =
    SS.for_all'
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val coq_ISet : (A__1.t, t) coq_TCSet **)
  
  let coq_ISet =
    { set_empty = empty; set_is_empty = is_empty; set_add = add;
      set_singleton = singleton; set_mem = mem; set_remove = remove;
      set_union = union; set_union_small_big = union_small_big;
      set_intersect = intersect; set_diff = diff; set_subset = subset;
      set_filter = filter; set_fold = (fun _ -> fold); set_iter = iter;
      set_elements = elements; set_cardinal = cardinal; set_choose = choose;
      set_choose_only = choose_only; set_for_all = for_all }
 end

