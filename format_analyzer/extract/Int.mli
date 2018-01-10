open BinInt
open ZArith_dec

type __ = Obj.t

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

module MoreInt : 
 functor (I:Int) ->
 sig 
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
  
  val coq_ExprI_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1)
    -> (coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 ->
    'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI ->
    'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (I.t -> 'a1) -> coq_ExprI -> 'a1
  
  val coq_ExprI_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1)
    -> (coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 ->
    'a1) -> (coq_ExprI -> 'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (coq_ExprI ->
    'a1 -> coq_ExprI -> 'a1 -> 'a1) -> (I.t -> 'a1) -> coq_ExprI -> 'a1
  
  type coq_ExprZ =
  | EZplus of coq_ExprZ * coq_ExprZ
  | EZopp of coq_ExprZ
  | EZminus of coq_ExprZ * coq_ExprZ
  | EZmult of coq_ExprZ * coq_ExprZ
  | EZmax of coq_ExprZ * coq_ExprZ
  | EZofI of coq_ExprI
  | EZraw of int
  
  val coq_ExprZ_rect :
    (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 ->
    'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ ->
    'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1
    -> 'a1) -> (coq_ExprI -> 'a1) -> (int -> 'a1) -> coq_ExprZ -> 'a1
  
  val coq_ExprZ_rec :
    (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 ->
    'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ ->
    'a1 -> coq_ExprZ -> 'a1 -> 'a1) -> (coq_ExprZ -> 'a1 -> coq_ExprZ -> 'a1
    -> 'a1) -> (coq_ExprI -> 'a1) -> (int -> 'a1) -> coq_ExprZ -> 'a1
  
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
  
  val coq_ExprP_rect :
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1
    -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP ->
    'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1
    -> 'a1) -> (coq_ExprP -> 'a1 -> 'a1) -> (__ -> 'a1) -> coq_ExprP -> 'a1
  
  val coq_ExprP_rec :
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprZ -> coq_ExprZ -> 'a1) ->
    (coq_ExprZ -> coq_ExprZ -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1
    -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP ->
    'a1 -> coq_ExprP -> 'a1 -> 'a1) -> (coq_ExprP -> 'a1 -> coq_ExprP -> 'a1
    -> 'a1) -> (coq_ExprP -> 'a1 -> 'a1) -> (__ -> 'a1) -> coq_ExprP -> 'a1
  
  val ei2i : coq_ExprI -> I.t
  
  val ez2z : coq_ExprZ -> int
  
  val norm_ei : coq_ExprI -> coq_ExprZ
  
  val norm_ez : coq_ExprZ -> coq_ExprZ
  
  val norm_ep : coq_ExprP -> coq_ExprP
 end

module Z_as_Int : 
 sig 
  type t = int
  
  val _0 : int
  
  val _1 : int
  
  val _2 : int
  
  val _3 : int
  
  val plus : int -> int -> int
  
  val opp : int -> int
  
  val minus : int -> int -> int
  
  val mult : int -> int -> int
  
  val max : int -> int -> int
  
  val gt_le_dec : int -> int -> bool
  
  val ge_lt_dec : int -> int -> bool
  
  val eq_dec : int -> int -> bool
  
  val i2z : t -> int
 end

