open BinInt
open ZArith_dec

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type Int = 
 sig 
  type t 
  
  val i2z : t -> int
  
  val _0 : t
  
  val _1 : t
  
  val _2 : t
  
  val _3 : t
  
  val plus : t -> t -> t
  
  val opp : t -> t
  
  val minus : t -> t -> t
  
  val mult : t -> t -> t
  
  val max : t -> t -> t
  
  val gt_le_dec : t -> t -> bool
  
  val ge_lt_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
 end

module MoreInt = 
 functor (I:Int) ->
 struct 
  type coq_ExprI =
  | EI0
  | EI1
  | EI2
  | EI3
  | EIplus of coq_ExprI * coq_ExprI
  | EIopp of coq_ExprI
  | EIminus of coq_ExprI * coq_ExprI
  | EImult of coq_ExprI * coq_ExprI
  | EImax of coq_ExprI * coq_ExprI
  | EIraw of I.t
  
  (** val coq_ExprI_rect :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 ->
      'a1) -> (coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI ->
      'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) ->
      (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (I.t -> 'a1) ->
      coq_ExprI -> 'a1 **)
  
  let rec coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 = function
  | EI0 -> f
  | EI1 -> f0
  | EI2 -> f1
  | EI3 -> f2
  | EIplus (e0, e1) ->
    f3 e0 (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EIopp e0 -> f4 e0 (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0)
  | EIminus (e0, e1) ->
    f5 e0 (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EImult (e0, e1) ->
    f6 e0 (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EImax (e0, e1) ->
    f7 e0 (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EIraw t0 -> f8 t0
  
  (** val coq_ExprI_rec :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 ->
      'a1) -> (coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI ->
      'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) ->
      (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (I.t -> 'a1) ->
      coq_ExprI -> 'a1 **)
  
  let rec coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 = function
  | EI0 -> f
  | EI1 -> f0
  | EI2 -> f1
  | EI3 -> f2
  | EIplus (e0, e1) ->
    f3 e0 (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EIopp e0 -> f4 e0 (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0)
  | EIminus (e0, e1) ->
    f5 e0 (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EImult (e0, e1) ->
    f6 e0 (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EImax (e0, e1) ->
    f7 e0 (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e0) e1
      (coq_ExprI_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 e1)
  | EIraw t0 -> f8 t0
  
  type coq_ExprZ =
  | EZplus of coq_ExprZ * coq_ExprZ
  | EZopp of coq_ExprZ
  | EZminus of coq_ExprZ * coq_ExprZ
  | EZmult of coq_ExprZ * coq_ExprZ
  | EZmax of coq_ExprZ * coq_ExprZ
  | EZofI of coq_ExprI
  | EZraw of int
  
  (** val coq_ExprZ_rect :
      (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 ->
      'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ ->
      'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ ->
      'a1 -> 'a1) -> (coq_ExprI -> 'a1) -> (int -> 'a1) -> coq_ExprZ -> 'a1 **)
  
  let rec coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 = function
  | EZplus (e0, e1) ->
    f e0 (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e1)
  | EZopp e0 -> f0 e0 (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e0)
  | EZminus (e0, e1) ->
    f1 e0 (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e1)
  | EZmult (e0, e1) ->
    f2 e0 (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e1)
  | EZmax (e0, e1) ->
    f3 e0 (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rect f f0 f1 f2 f3 f4 f5 e1)
  | EZofI e0 -> f4 e0
  | EZraw z -> f5 z
  
  (** val coq_ExprZ_rec :
      (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 ->
      'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ ->
      'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ ->
      'a1 -> 'a1) -> (coq_ExprI -> 'a1) -> (int -> 'a1) -> coq_ExprZ -> 'a1 **)
  
  let rec coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 = function
  | EZplus (e0, e1) ->
    f e0 (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e1)
  | EZopp e0 -> f0 e0 (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e0)
  | EZminus (e0, e1) ->
    f1 e0 (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e1)
  | EZmult (e0, e1) ->
    f2 e0 (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e1)
  | EZmax (e0, e1) ->
    f3 e0 (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e0) e1
      (coq_ExprZ_rec f f0 f1 f2 f3 f4 f5 e1)
  | EZofI e0 -> f4 e0
  | EZraw z -> f5 z
  
  type coq_ExprP =
  | EPeq of coq_ExprZ * coq_ExprZ
  | EPlt of coq_ExprZ * coq_ExprZ
  | EPle of coq_ExprZ * coq_ExprZ
  | EPgt of coq_ExprZ * coq_ExprZ
  | EPge of coq_ExprZ * coq_ExprZ
  | EPimpl of coq_ExprP * coq_ExprP
  | EPequiv of coq_ExprP * coq_ExprP
  | EPand of coq_ExprP * coq_ExprP
  | EPor of coq_ExprP * coq_ExprP
  | EPneg of coq_ExprP
  | EPraw
  
  (** val coq_ExprP_rect :
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP ->
      'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) ->
      (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 ->
      coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> 'a1) -> (__ -> 'a1) ->
      coq_ExprP -> 'a1 **)
  
  let rec coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
  | EPeq (e0, e1) -> f e0 e1
  | EPlt (e0, e1) -> f0 e0 e1
  | EPle (e0, e1) -> f1 e0 e1
  | EPgt (e0, e1) -> f2 e0 e1
  | EPge (e0, e1) -> f3 e0 e1
  | EPimpl (e0, e1) ->
    f4 e0 (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPequiv (e0, e1) ->
    f5 e0 (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPand (e0, e1) ->
    f6 e0 (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPor (e0, e1) ->
    f7 e0 (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPneg e0 -> f8 e0 (coq_ExprP_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0)
  | EPraw -> f9 __
  
  (** val coq_ExprP_rec :
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
      (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP ->
      'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) ->
      (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 ->
      coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> 'a1) -> (__ -> 'a1) ->
      coq_ExprP -> 'a1 **)
  
  let rec coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
  | EPeq (e0, e1) -> f e0 e1
  | EPlt (e0, e1) -> f0 e0 e1
  | EPle (e0, e1) -> f1 e0 e1
  | EPgt (e0, e1) -> f2 e0 e1
  | EPge (e0, e1) -> f3 e0 e1
  | EPimpl (e0, e1) ->
    f4 e0 (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPequiv (e0, e1) ->
    f5 e0 (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPand (e0, e1) ->
    f6 e0 (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPor (e0, e1) ->
    f7 e0 (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0) e1
      (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e1)
  | EPneg e0 -> f8 e0 (coq_ExprP_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 e0)
  | EPraw -> f9 __
  
  (** val ei2i : coq_ExprI -> I.t **)
  
  let rec ei2i = function
  | EI0 -> I._0
  | EI1 -> I._1
  | EI2 -> I._2
  | EI3 -> I._3
  | EIplus (e1, e2) -> I.plus (ei2i e1) (ei2i e2)
  | EIopp e0 -> I.opp (ei2i e0)
  | EIminus (e1, e2) -> I.minus (ei2i e1) (ei2i e2)
  | EImult (e1, e2) -> I.mult (ei2i e1) (ei2i e2)
  | EImax (e1, e2) -> I.max (ei2i e1) (ei2i e2)
  | EIraw i -> i
  
  (** val ez2z : coq_ExprZ -> int **)
  
  let rec ez2z = function
  | EZplus (e1, e2) -> Z.add (ez2z e1) (ez2z e2)
  | EZopp e0 -> Z.opp (ez2z e0)
  | EZminus (e1, e2) -> Z.sub (ez2z e1) (ez2z e2)
  | EZmult (e1, e2) -> Z.mul (ez2z e1) (ez2z e2)
  | EZmax (e1, e2) -> Z.max (ez2z e1) (ez2z e2)
  | EZofI e0 -> I.i2z (ei2i e0)
  | EZraw z -> z
  
  (** val norm_ei : coq_ExprI -> coq_ExprZ **)
  
  let rec norm_ei = function
  | EI0 -> EZraw 0
  | EI1 -> EZraw 1
  | EI2 -> EZraw ((fun p->2*p) 1)
  | EI3 -> EZraw ((fun p->1+2*p) 1)
  | EIplus (e1, e2) -> EZplus ((norm_ei e1), (norm_ei e2))
  | EIopp e0 -> EZopp (norm_ei e0)
  | EIminus (e1, e2) -> EZminus ((norm_ei e1), (norm_ei e2))
  | EImult (e1, e2) -> EZmult ((norm_ei e1), (norm_ei e2))
  | EImax (e1, e2) -> EZmax ((norm_ei e1), (norm_ei e2))
  | EIraw i -> EZofI (EIraw i)
  
  (** val norm_ez : coq_ExprZ -> coq_ExprZ **)
  
  let rec norm_ez = function
  | EZplus (e1, e2) -> EZplus ((norm_ez e1), (norm_ez e2))
  | EZopp e0 -> EZopp (norm_ez e0)
  | EZminus (e1, e2) -> EZminus ((norm_ez e1), (norm_ez e2))
  | EZmult (e1, e2) -> EZmult ((norm_ez e1), (norm_ez e2))
  | EZmax (e1, e2) -> EZmax ((norm_ez e1), (norm_ez e2))
  | EZofI e0 -> norm_ei e0
  | EZraw z -> EZraw z
  
  (** val norm_ep : coq_ExprP -> coq_ExprP **)
  
  let rec norm_ep = function
  | EPeq (e1, e2) -> EPeq ((norm_ez e1), (norm_ez e2))
  | EPlt (e1, e2) -> EPlt ((norm_ez e1), (norm_ez e2))
  | EPle (e1, e2) -> EPle ((norm_ez e1), (norm_ez e2))
  | EPgt (e1, e2) -> EPgt ((norm_ez e1), (norm_ez e2))
  | EPge (e1, e2) -> EPge ((norm_ez e1), (norm_ez e2))
  | EPimpl (e1, e2) -> EPimpl ((norm_ep e1), (norm_ep e2))
  | EPequiv (e1, e2) -> EPequiv ((norm_ep e1), (norm_ep e2))
  | EPand (e1, e2) -> EPand ((norm_ep e1), (norm_ep e2))
  | EPor (e1, e2) -> EPor ((norm_ep e1), (norm_ep e2))
  | EPneg e0 -> EPneg (norm_ep e0)
  | EPraw -> EPraw
 end

module Z_as_Int = 
 struct 
  type t = int
  
  (** val _0 : int **)
  
  let _0 =
    0
  
  (** val _1 : int **)
  
  let _1 =
    1
  
  (** val _2 : int **)
  
  let _2 =
    ((fun p->2*p) 1)
  
  (** val _3 : int **)
  
  let _3 =
    ((fun p->1+2*p) 1)
  
  (** val plus : int -> int -> int **)
  
  let plus =
    Z.add
  
  (** val opp : int -> int **)
  
  let opp =
    Z.opp
  
  (** val minus : int -> int -> int **)
  
  let minus =
    Z.sub
  
  (** val mult : int -> int -> int **)
  
  let mult =
    Z.mul
  
  (** val max : int -> int -> int **)
  
  let max =
    Z.max
  
  (** val gt_le_dec : int -> int -> bool **)
  
  let gt_le_dec =
    coq_Z_gt_le_dec
  
  (** val ge_lt_dec : int -> int -> bool **)
  
  let ge_lt_dec =
    coq_Z_ge_lt_dec
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec =
    Z.eq_dec
  
  (** val i2z : t -> int **)
  
  let i2z n =
    n
 end

