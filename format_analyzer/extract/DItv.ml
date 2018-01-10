open BinInt
open DLat
open List0
open Sumbool
open TStr
open VocabA
open ZArith_dec

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Itv = 
 struct 
  (** val threshold : int list **)
  
  let threshold =
    0 :: (((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))) :: [])
  
  type t' =
  | Int of int
  | PInf
  | MInf
  
  (** val t'_rect : (int -> 'a1) -> 'a1 -> 'a1 -> t' -> 'a1 **)
  
  let t'_rect f f0 f1 = function
  | Int x -> f x
  | PInf -> f0
  | MInf -> f1
  
  (** val t'_rec : (int -> 'a1) -> 'a1 -> 'a1 -> t' -> 'a1 **)
  
  let t'_rec f f0 f1 = function
  | Int x -> f x
  | PInf -> f0
  | MInf -> f1
  
  (** val le'_dec : t' -> t' -> bool **)
  
  let le'_dec x y =
    match x with
    | Int i ->
      (match y with
       | Int j -> coq_Z_le_dec i j
       | PInf -> true
       | MInf -> false)
    | PInf ->
      (match y with
       | PInf -> true
       | _ -> false)
    | MInf -> true
  
  (** val eq'_dec : t' -> t' -> bool **)
  
  let eq'_dec x y =
    match x with
    | Int i ->
      (match y with
       | Int j -> Z.eq_dec i j
       | _ -> false)
    | PInf ->
      (match y with
       | PInf -> true
       | _ -> false)
    | MInf ->
      (match y with
       | MInf -> true
       | _ -> false)
  
  (** val min' : t' -> t' -> t' **)
  
  let min' x y =
    if le'_dec x y then x else y
  
  (** val max' : t' -> t' -> t' **)
  
  let max' x y =
    if le'_dec x y then y else x
  
  (** val lower_widen' : t' -> t' -> t' **)
  
  let lower_widen' x y =
    if le'_dec y x
    then if eq'_dec y x
         then y
         else let filtered =
                filter (fun k -> if le'_dec (Int k) y then true else false)
                  threshold
              in
              list_fold (fun k m -> max' (Int k) m) filtered MInf
    else x
  
  (** val upper_widen' : t' -> t' -> t' **)
  
  let upper_widen' x y =
    if le'_dec x y
    then if eq'_dec x y
         then y
         else let filtered =
                filter (fun k -> if le'_dec y (Int k) then true else false)
                  threshold
              in
              list_fold (fun k m -> min' (Int k) m) filtered PInf
    else x
  
  (** val lower_narrow_msg : string_t **)
  
  let lower_narrow_msg = "lower_narrow x y when x > y"
  
  (** val lower_narrow' : t' -> t' -> t' **)
  
  let lower_narrow' x y =
    if le'_dec x y then y else invalid_arg lower_narrow_msg
  
  (** val upper_narrow_msg : string_t **)
  
  let upper_narrow_msg = "upper_narrow x y when y > x"
  
  (** val upper_narrow' : t' -> t' -> t' **)
  
  let upper_narrow' x y =
    if le'_dec y x then y else invalid_arg upper_narrow_msg
  
  (** val plus' : t' -> t' -> t' **)
  
  let plus' x y =
    match x with
    | Int n1 ->
      (match y with
       | Int n2 -> Int (Z.add n1 n2)
       | x0 -> x0)
    | PInf ->
      (match y with
       | MInf -> assert false (* absurd case *)
       | _ -> PInf)
    | MInf ->
      (match y with
       | PInf -> assert false (* absurd case *)
       | _ -> MInf)
  
  (** val plus'_one : t' -> t' **)
  
  let plus'_one x =
    plus' x (Int 1)
  
  (** val minus' : t' -> t' -> t' **)
  
  let minus' x y =
    match x with
    | Int n1 ->
      (match y with
       | Int n2 -> Int (Z.sub n1 n2)
       | PInf -> MInf
       | MInf -> PInf)
    | PInf ->
      (match y with
       | PInf -> assert false (* absurd case *)
       | _ -> PInf)
    | MInf ->
      (match y with
       | MInf -> assert false (* absurd case *)
       | _ -> MInf)
  
  (** val minus'_one : t' -> t' **)
  
  let minus'_one x =
    minus' x (Int 1)
  
  (** val times' : t' -> t' -> t' **)
  
  let times' x y =
    match x with
    | Int n ->
      (match y with
       | Int n2 -> Int (Z.mul n n2)
       | PInf ->
         if coq_Z_lt_dec n 0
         then MInf
         else if coq_Z_gt_dec n 0 then PInf else Int 0
       | MInf ->
         if coq_Z_lt_dec n 0
         then PInf
         else if coq_Z_gt_dec n 0 then MInf else Int 0)
    | PInf ->
      (match y with
       | Int n ->
         if coq_Z_lt_dec n 0
         then MInf
         else if coq_Z_gt_dec n 0 then PInf else Int 0
       | x0 -> x0)
    | MInf ->
      (match y with
       | Int n ->
         if coq_Z_lt_dec n 0
         then PInf
         else if coq_Z_gt_dec n 0 then MInf else Int 0
       | PInf -> MInf
       | MInf -> PInf)
  
  (** val divide' : t' -> t' -> t' **)
  
  let divide' x y =
    match x with
    | Int n1 ->
      (match y with
       | Int n2 -> Int (c_div n1 n2)
       | _ -> Int 0)
    | PInf ->
      (match y with
       | Int n -> if coq_Z_lt_dec 0 n then PInf else MInf
       | _ -> Int 0)
    | MInf ->
      (match y with
       | Int n -> if coq_Z_lt_dec 0 n then MInf else PInf
       | _ -> Int 0)
  
  (** val min4' : t' -> t' -> t' -> t' -> t' **)
  
  let min4' x y z w =
    min' (min' x y) (min' z w)
  
  (** val max4' : t' -> t' -> t' -> t' -> t' **)
  
  let max4' x y z w =
    max' (max' x y) (max' z w)
  
  type t'' =
  | V of t' * t'
  | Bot
  
  (** val t''_rect :
      (t' -> t' -> __ -> __ -> __ -> 'a1) -> 'a1 -> t'' -> 'a1 **)
  
  let t''_rect f f0 = function
  | V (x, x0) -> f x x0 __ __ __
  | Bot -> f0
  
  (** val t''_rec :
      (t' -> t' -> __ -> __ -> __ -> 'a1) -> 'a1 -> t'' -> 'a1 **)
  
  let t''_rec f f0 = function
  | V (x, x0) -> f x x0 __ __ __
  | Bot -> f0
  
  type t = t''
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | V (l1, u1) ->
            (match y with
             | V (l2, u2) -> if le'_dec l2 l1 then le'_dec u1 u2 else false
             | Bot -> false)
          | Bot -> true)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | V (l1, u1) ->
            (match y with
             | V (l2, u2) -> if eq'_dec l2 l1 then eq'_dec u1 u2 else false
             | Bot -> false)
          | Bot ->
            (match y with
             | V (lb, ub) -> false
             | Bot -> true))
  
  (** val top : t **)
  
  let top =
    V (MInf, PInf)
  
  (** val bot : t **)
  
  let bot =
    Bot
  
  (** val of_int : int -> t **)
  
  let of_int i =
    V ((Int i), (Int i))
  
  (** val of_ints : int -> int -> t **)
  
  let of_ints i j =
    if coq_Z_le_dec i j then V ((Int i), (Int j)) else Bot
  
  (** val of_lb : int -> t **)
  
  let of_lb i =
    V ((Int i), PInf)
  
  (** val of_ub : int -> t **)
  
  let of_ub i =
    V (MInf, (Int i))
  
  (** val zero : t **)
  
  let zero =
    of_int 0
  
  (** val false_itv : t **)
  
  let false_itv =
    zero
  
  (** val true_itv : t **)
  
  let true_itv =
    of_int 1
  
  (** val unknown_bool : t **)
  
  let unknown_bool =
    of_ints 0 1
  
  (** val pos : t **)
  
  let pos =
    of_lb 1
  
  (** val neg : t **)
  
  let neg =
    of_ub ((~-) 1)
  
  (** val zero_pos : t **)
  
  let zero_pos =
    of_lb 0
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else (match x with
               | V (l1, u1) ->
                 (match y with
                  | V (l2, u2) -> V ((min' l1 l2), (max' u1 u2))
                  | Bot -> x)
               | Bot -> y)
  
  (** val gen_itv : t' -> t' -> t **)
  
  let gen_itv l u =
    if le'_dec l u
    then (match l with
          | PInf -> Bot
          | _ ->
            (match u with
             | MInf -> Bot
             | _ -> V (l, u)))
    else Bot
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else (match x with
               | V (l1, u1) ->
                 (match y with
                  | V (l2, u2) -> gen_itv (max' l1 l2) (min' u1 u2)
                  | Bot -> Bot)
               | Bot -> Bot)
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else (match x with
          | V (l1, u1) ->
            (match y with
             | V (l2, u2) ->
               gen_itv (lower_widen' l1 l2) (upper_widen' u1 u2)
             | Bot -> x)
          | Bot -> y)
  
  (** val narrow_msg : string_t **)
  
  let narrow_msg = "narrow bot _"
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else (match x with
          | V (l1, u1) ->
            (match y with
             | V (l2, u2) ->
               gen_itv (lower_narrow' l1 l2) (upper_narrow' u1 u2)
             | Bot -> Bot)
          | Bot ->
            (match y with
             | V (lb, ub) -> invalid_arg narrow_msg
             | Bot -> Bot))
  
  (** val is_const : t -> bool **)
  
  let is_const = function
  | V (lb, ub) ->
    (match lb with
     | Int i1 ->
       (match ub with
        | Int i2 -> if eq'_dec (Int i1) (Int i2) then true else false
        | _ -> false)
     | _ -> false)
  | Bot -> false
  
  (** val diff : t -> int **)
  
  let diff = function
  | V (lb, ub) ->
    (match lb with
     | Int i1 ->
       (match ub with
        | Int i2 -> Z.sub i2 i1
        | _ -> 0)
     | _ -> 0)
  | Bot -> 0
  
  (** val plus : t -> t -> t **)
  
  let plus x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) -> V ((plus' l1 l2), (plus' u1 u2))
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val minus : t -> t -> t **)
  
  let minus x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) -> V ((minus' l1 u2), (minus' u1 l2))
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val times : t -> t -> t **)
  
  let times x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         let x1 = times' l1 l2 in
         let x2 = times' l1 u2 in
         let x3 = times' u1 l2 in
         let x4 = times' u1 u2 in
         V ((min4' x1 x2 x3 x4), (max4' x1 x2 x3 x4))
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val divide : t -> t -> t **)
  
  let divide x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         if le_dec zero y
         then top
         else let x1 = divide' l1 l2 in
              let x2 = divide' l1 u2 in
              let x3 = divide' u1 l2 in
              let x4 = divide' u1 u2 in
              V ((min4' x1 x2 x3 x4), (max4' x1 x2 x3 x4))
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val and_itv : t -> t -> t **)
  
  let and_itv x y =
    if sumbool_or (eq_dec x Bot) (eq_dec y Bot)
    then Bot
    else if sumbool_or (eq_dec x false_itv) (eq_dec y false_itv)
         then false_itv
         else if sumbool_and (sumbool_not (le_dec false_itv x))
                   (sumbool_not (le_dec false_itv y))
              then true_itv
              else unknown_bool
  
  (** val or_itv : t -> t -> t **)
  
  let or_itv x y =
    if sumbool_or (eq_dec x Bot) (eq_dec y Bot)
    then Bot
    else if sumbool_and (eq_dec x false_itv) (eq_dec y false_itv)
         then false_itv
         else if sumbool_or (sumbool_not (le_dec false_itv x))
                   (sumbool_not (le_dec false_itv y))
              then true_itv
              else unknown_bool
  
  (** val eq_itv : t -> t -> t **)
  
  let eq_itv x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         if sumbool_and (eq'_dec l1 u1)
              (sumbool_and (eq'_dec u1 l2) (eq'_dec l2 u2))
         then true_itv
         else if sumbool_not (le'_dec l2 u1)
              then false_itv
              else if sumbool_not (le'_dec l1 u2)
                   then false_itv
                   else unknown_bool
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val ne_itv : t -> t -> t **)
  
  let ne_itv x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         if sumbool_and (eq'_dec l1 u1)
              (sumbool_and (eq'_dec u1 l2) (eq'_dec l2 u2))
         then false_itv
         else if sumbool_not (le'_dec l2 u1)
              then true_itv
              else if sumbool_not (le'_dec l1 u2)
                   then true_itv
                   else unknown_bool
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val lt_itv : t -> t -> t **)
  
  let lt_itv x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         if sumbool_not (le'_dec l2 u1)
         then true_itv
         else if le'_dec u2 l1 then false_itv else unknown_bool
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val le_itv : t -> t -> t **)
  
  let le_itv x y =
    match x with
    | V (l1, u1) ->
      (match y with
       | V (l2, u2) ->
         if le'_dec u1 l2
         then true_itv
         else if sumbool_not (le'_dec l1 u2) then false_itv else unknown_bool
       | Bot -> Bot)
    | Bot -> Bot
  
  (** val gt_itv : t -> t -> t **)
  
  let gt_itv x y =
    lt_itv y x
  
  (** val ge_itv : t -> t -> t **)
  
  let ge_itv x y =
    le_itv y x
  
  (** val not_itv : t -> t **)
  
  let not_itv x =
    if eq_dec x Bot
    then Bot
    else if eq_dec x false_itv
         then true_itv
         else if le_dec false_itv x then unknown_bool else false_itv
  
  (** val unknown_binary : t -> t -> t **)
  
  let unknown_binary x y =
    if sumbool_or (eq_dec x Bot) (eq_dec y Bot) then Bot else top
  
  (** val unknown_unary : t -> t **)
  
  let unknown_unary x =
    if eq_dec x Bot then Bot else top
  
  (** val l_shift_itv : t -> t -> t **)
  
  let l_shift_itv x y =
    unknown_binary x y
  
  (** val r_shift_itv : t -> t -> t **)
  
  let r_shift_itv x y =
    unknown_binary x y
  
  (** val b_xor_itv : t -> t -> t **)
  
  let b_xor_itv x y =
    unknown_binary x y
  
  (** val b_or_itv : t -> t -> t **)
  
  let b_or_itv x y =
    unknown_binary x y
  
  (** val b_and_itv : t -> t -> t **)
  
  let b_and_itv x y =
    unknown_binary x y
  
  (** val mod_itv : t -> t -> t **)
  
  let mod_itv x y =
    unknown_binary x y
  
  (** val b_not_itv : t -> t **)
  
  let b_not_itv x =
    unknown_unary x
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

