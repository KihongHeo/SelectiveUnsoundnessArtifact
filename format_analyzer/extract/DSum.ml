open DLat
open Datatypes
open TStr
open VocabA

(** val sum_narrow_msg : string_t **)

let sum_narrow_msg = "narrow small big"

module SumKey = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 struct 
  type t = (A.t, B.t) sum
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' ->
         (match A.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | Coq_inr y' -> OrderedType.LT)
    | Coq_inr x' ->
      (match y with
       | Coq_inl y' -> OrderedType.GT
       | Coq_inr y' ->
         (match B.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' -> A.eq_dec x' y'
       | Coq_inr t0 -> false)
    | Coq_inr x' ->
      (match y with
       | Coq_inl t0 -> false
       | Coq_inr y' -> B.eq_dec x' y')
 end

module Sum = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 struct 
  type t' =
  | Top
  | Inl of A.t
  | Inr of B.t
  | Bot
  
  (** val t'_rect :
      'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  (** val t'_rec :
      'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  type t = t'
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Top -> true
             | Inl y' -> A.le_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Top -> true
             | Inr y' -> B.le_dec x' y'
             | _ -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Inl y' -> A.eq_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Inr y' -> B.eq_dec x' y'
             | _ -> false)
          | Bot ->
            (match y with
             | Bot -> true
             | _ -> false))
  
  (** val top : t **)
  
  let top =
    Top
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val join : t -> t -> t **)
  
  let join x y =
    match x with
    | Top -> Top
    | Inl x' ->
      (match y with
       | Inl y' -> Inl (A.join x' y')
       | Bot -> x
       | _ -> Top)
    | Inr x' ->
      (match y with
       | Inr y' -> Inr (B.join x' y')
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    match x with
    | Top -> y
    | Inl x' ->
      (match y with
       | Top -> x
       | Inl y' -> Inl (A.meet x' y')
       | _ -> Bot)
    | Inr x' ->
      (match y with
       | Top -> x
       | Inr y' -> Inr (B.meet x' y')
       | _ -> Bot)
    | Bot ->
      (match y with
       | Top -> x
       | _ -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    match x with
    | Top -> Top
    | Inl a ->
      (match y with
       | Inl b -> Inl (A.widen a b)
       | Bot -> x
       | _ -> Top)
    | Inr a ->
      (match y with
       | Inr b -> Inr (B.widen a b)
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | Top -> y
          | Inl a ->
            (match y with
             | Inl b -> Inl (A.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Inr a ->
            (match y with
             | Inr b -> Inr (B.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Bot ->
            (match y with
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

module SumKey2 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 struct 
  type t = (A.t, B.t) sum
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' ->
         (match A.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | Coq_inr y' -> OrderedType.LT)
    | Coq_inr x' ->
      (match y with
       | Coq_inl y' -> OrderedType.GT
       | Coq_inr y' ->
         (match B.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' -> A.eq_dec x' y'
       | Coq_inr t0 -> false)
    | Coq_inr x' ->
      (match y with
       | Coq_inl t0 -> false
       | Coq_inr y' -> B.eq_dec x' y')
 end

module SumKey3 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 struct 
  module E2 = SumKey(A)(B)
  
  type t = ((A.t, B.t) sum, C.t) sum
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' ->
         (match E2.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | Coq_inr y' -> OrderedType.LT)
    | Coq_inr x' ->
      (match y with
       | Coq_inl y' -> OrderedType.GT
       | Coq_inr y' ->
         (match C.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' -> E2.eq_dec x' y'
       | Coq_inr t0 -> false)
    | Coq_inr x' ->
      (match y with
       | Coq_inl t0 -> false
       | Coq_inr y' -> C.eq_dec x' y')
 end

module SumKey4 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 struct 
  module E2 = SumKey(A)(B)
  
  module E3 = SumKey(E2)(C)
  
  type t = (((A.t, B.t) sum, C.t) sum, D.t) sum
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' ->
         (match E3.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | Coq_inr y' -> OrderedType.LT)
    | Coq_inr x' ->
      (match y with
       | Coq_inl y' -> OrderedType.GT
       | Coq_inr y' ->
         (match D.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' -> E3.eq_dec x' y'
       | Coq_inr t0 -> false)
    | Coq_inr x' ->
      (match y with
       | Coq_inl t0 -> false
       | Coq_inr y' -> D.eq_dec x' y')
 end

module SumKey5 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 functor (E:KEY) ->
 struct 
  module E2 = SumKey(A)(B)
  
  module E3 = SumKey(E2)(C)
  
  module E4 = SumKey(E3)(D)
  
  type t = ((((A.t, B.t) sum, C.t) sum, D.t) sum, E.t) sum
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' ->
         (match E4.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | Coq_inr y' -> OrderedType.LT)
    | Coq_inr x' ->
      (match y with
       | Coq_inl y' -> OrderedType.GT
       | Coq_inr y' ->
         (match E.compare x' y' with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match x with
    | Coq_inl x' ->
      (match y with
       | Coq_inl y' -> E4.eq_dec x' y'
       | Coq_inr t0 -> false)
    | Coq_inr x' ->
      (match y with
       | Coq_inl t0 -> false
       | Coq_inr y' -> E.eq_dec x' y')
 end

module Sum2 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 struct 
  type t' =
  | Top
  | Inl of A.t
  | Inr of B.t
  | Bot
  
  (** val t'_rect :
      'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  (** val t'_rec :
      'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  type t = t'
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Top -> true
             | Inl y' -> A.le_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Top -> true
             | Inr y' -> B.le_dec x' y'
             | _ -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Inl y' -> A.eq_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Inr y' -> B.eq_dec x' y'
             | _ -> false)
          | Bot ->
            (match y with
             | Bot -> true
             | _ -> false))
  
  (** val top : t **)
  
  let top =
    Top
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val join : t -> t -> t **)
  
  let join x y =
    match x with
    | Top -> Top
    | Inl x' ->
      (match y with
       | Inl y' -> Inl (A.join x' y')
       | Bot -> x
       | _ -> Top)
    | Inr x' ->
      (match y with
       | Inr y' -> Inr (B.join x' y')
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    match x with
    | Top -> y
    | Inl x' ->
      (match y with
       | Top -> x
       | Inl y' -> Inl (A.meet x' y')
       | _ -> Bot)
    | Inr x' ->
      (match y with
       | Top -> x
       | Inr y' -> Inr (B.meet x' y')
       | _ -> Bot)
    | Bot ->
      (match y with
       | Top -> x
       | _ -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    match x with
    | Top -> Top
    | Inl a ->
      (match y with
       | Inl b -> Inl (A.widen a b)
       | Bot -> x
       | _ -> Top)
    | Inr a ->
      (match y with
       | Inr b -> Inr (B.widen a b)
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | Top -> y
          | Inl a ->
            (match y with
             | Inl b -> Inl (A.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Inr a ->
            (match y with
             | Inr b -> Inr (B.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Bot ->
            (match y with
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

module Sum3 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 struct 
  module E2 = Sum(A)(B)
  
  type t' =
  | Top
  | Inl of E2.t
  | Inr of C.t
  | Bot
  
  (** val t'_rect :
      'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  (** val t'_rec :
      'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  type t = t'
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Top -> true
             | Inl y' -> E2.le_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Top -> true
             | Inr y' -> C.le_dec x' y'
             | _ -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Inl y' -> E2.eq_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Inr y' -> C.eq_dec x' y'
             | _ -> false)
          | Bot ->
            (match y with
             | Bot -> true
             | _ -> false))
  
  (** val top : t **)
  
  let top =
    Top
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val join : t -> t -> t **)
  
  let join x y =
    match x with
    | Top -> Top
    | Inl x' ->
      (match y with
       | Inl y' -> Inl (E2.join x' y')
       | Bot -> x
       | _ -> Top)
    | Inr x' ->
      (match y with
       | Inr y' -> Inr (C.join x' y')
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    match x with
    | Top -> y
    | Inl x' ->
      (match y with
       | Top -> x
       | Inl y' -> Inl (E2.meet x' y')
       | _ -> Bot)
    | Inr x' ->
      (match y with
       | Top -> x
       | Inr y' -> Inr (C.meet x' y')
       | _ -> Bot)
    | Bot ->
      (match y with
       | Top -> x
       | _ -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    match x with
    | Top -> Top
    | Inl a ->
      (match y with
       | Inl b -> Inl (E2.widen a b)
       | Bot -> x
       | _ -> Top)
    | Inr a ->
      (match y with
       | Inr b -> Inr (C.widen a b)
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | Top -> y
          | Inl a ->
            (match y with
             | Inl b -> Inl (E2.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Inr a ->
            (match y with
             | Inr b -> Inr (C.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Bot ->
            (match y with
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

module Sum4 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 struct 
  module E2 = Sum(A)(B)
  
  module E3 = Sum(E2)(C)
  
  type t' =
  | Top
  | Inl of E3.t
  | Inr of D.t
  | Bot
  
  (** val t'_rect :
      'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  (** val t'_rec :
      'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  type t = t'
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Top -> true
             | Inl y' -> E3.le_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Top -> true
             | Inr y' -> D.le_dec x' y'
             | _ -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Inl y' -> E3.eq_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Inr y' -> D.eq_dec x' y'
             | _ -> false)
          | Bot ->
            (match y with
             | Bot -> true
             | _ -> false))
  
  (** val top : t **)
  
  let top =
    Top
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val join : t -> t -> t **)
  
  let join x y =
    match x with
    | Top -> Top
    | Inl x' ->
      (match y with
       | Inl y' -> Inl (E3.join x' y')
       | Bot -> x
       | _ -> Top)
    | Inr x' ->
      (match y with
       | Inr y' -> Inr (D.join x' y')
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    match x with
    | Top -> y
    | Inl x' ->
      (match y with
       | Top -> x
       | Inl y' -> Inl (E3.meet x' y')
       | _ -> Bot)
    | Inr x' ->
      (match y with
       | Top -> x
       | Inr y' -> Inr (D.meet x' y')
       | _ -> Bot)
    | Bot ->
      (match y with
       | Top -> x
       | _ -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    match x with
    | Top -> Top
    | Inl a ->
      (match y with
       | Inl b -> Inl (E3.widen a b)
       | Bot -> x
       | _ -> Top)
    | Inr a ->
      (match y with
       | Inr b -> Inr (D.widen a b)
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | Top -> y
          | Inl a ->
            (match y with
             | Inl b -> Inl (E3.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Inr a ->
            (match y with
             | Inr b -> Inr (D.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Bot ->
            (match y with
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

module Sum5 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 functor (E:LAT) ->
 struct 
  module E2 = Sum(A)(B)
  
  module E3 = Sum(E2)(C)
  
  module E4 = Sum(E3)(D)
  
  type t' =
  | Top
  | Inl of E4.t
  | Inr of E.t
  | Bot
  
  (** val t'_rect :
      'a1 -> (E4.t -> 'a1) -> (E.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  (** val t'_rec :
      'a1 -> (E4.t -> 'a1) -> (E.t -> 'a1) -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 f2 = function
  | Top -> f
  | Inl x -> f0 x
  | Inr x -> f1 x
  | Bot -> f2
  
  type t = t'
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Top -> true
             | Inl y' -> E4.le_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Top -> true
             | Inr y' -> E.le_dec x' y'
             | _ -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | Top ->
            (match y with
             | Top -> true
             | _ -> false)
          | Inl x' ->
            (match y with
             | Inl y' -> E4.eq_dec x' y'
             | _ -> false)
          | Inr x' ->
            (match y with
             | Inr y' -> E.eq_dec x' y'
             | _ -> false)
          | Bot ->
            (match y with
             | Bot -> true
             | _ -> false))
  
  (** val top : t **)
  
  let top =
    Top
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val join : t -> t -> t **)
  
  let join x y =
    match x with
    | Top -> Top
    | Inl x' ->
      (match y with
       | Inl y' -> Inl (E4.join x' y')
       | Bot -> x
       | _ -> Top)
    | Inr x' ->
      (match y with
       | Inr y' -> Inr (E.join x' y')
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    match x with
    | Top -> y
    | Inl x' ->
      (match y with
       | Top -> x
       | Inl y' -> Inl (E4.meet x' y')
       | _ -> Bot)
    | Inr x' ->
      (match y with
       | Top -> x
       | Inr y' -> Inr (E.meet x' y')
       | _ -> Bot)
    | Bot ->
      (match y with
       | Top -> x
       | _ -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    match x with
    | Top -> Top
    | Inl a ->
      (match y with
       | Inl b -> Inl (E4.widen a b)
       | Bot -> x
       | _ -> Top)
    | Inr a ->
      (match y with
       | Inr b -> Inr (E.widen a b)
       | Bot -> x
       | _ -> Top)
    | Bot ->
      (match y with
       | Top -> Top
       | _ -> y)
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | Top -> y
          | Inl a ->
            (match y with
             | Inl b -> Inl (E4.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Inr a ->
            (match y with
             | Inr b -> Inr (E.narrow a b)
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg)
          | Bot ->
            (match y with
             | Bot -> Bot
             | _ -> invalid_arg sum_narrow_msg))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

