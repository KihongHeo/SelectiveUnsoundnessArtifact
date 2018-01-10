open BinInt
open BinNat
open BinPos
open Compare_dec
open Datatypes
open Peano_dec

module type UsualOrderedType = 
 sig 
  type t 
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module UOT_to_OT = 
 functor (U:UsualOrderedType) ->
 U

module Nat_as_OT = 
 struct 
  type t = int
  
  (** val compare : int -> int -> int OrderedType.coq_Compare **)
  
  let compare x y =
    match nat_compare x y with
    | Eq -> OrderedType.EQ
    | Lt -> OrderedType.LT
    | Gt -> OrderedType.GT
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec =
    eq_nat_dec
 end

module Z_as_OT = 
 struct 
  type t = int
  
  (** val compare : int -> int -> int OrderedType.coq_Compare **)
  
  let compare x y =
    match Z.compare x y with
    | Eq -> OrderedType.EQ
    | Lt -> OrderedType.LT
    | Gt -> OrderedType.GT
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec =
    Z.eq_dec
 end

module Positive_as_OT = 
 struct 
  type t = int
  
  (** val compare : int -> int -> int OrderedType.coq_Compare **)
  
  let compare x y =
    match Pos.compare x y with
    | Eq -> OrderedType.EQ
    | Lt -> OrderedType.LT
    | Gt -> OrderedType.GT
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec =
    Pos.eq_dec
 end

module N_as_OT = 
 struct 
  type t = int
  
  (** val compare : int -> int -> int OrderedType.coq_Compare **)
  
  let compare x y =
    match N.compare x y with
    | Eq -> OrderedType.EQ
    | Lt -> OrderedType.LT
    | Gt -> OrderedType.GT
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec =
    N.eq_dec
 end

module PairOrderedType = 
 functor (O1:OrderedType.OrderedType) ->
 functor (O2:OrderedType.OrderedType) ->
 struct 
  module MO1 = OrderedType.OrderedTypeFacts(O1)
  
  module MO2 = OrderedType.OrderedTypeFacts(O2)
  
  type t = O1.t * O2.t
  
  (** val compare : t -> t -> (O1.t * O2.t) OrderedType.coq_Compare **)
  
  let compare x y =
    let (x1, x2) = x in
    let (y1, y2) = y in
    let c = O1.compare x1 y1 in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = O2.compare x2 y2 in
       (match c0 with
        | OrderedType.LT -> OrderedType.LT
        | OrderedType.EQ -> OrderedType.EQ
        | OrderedType.GT -> OrderedType.GT)
     | OrderedType.GT -> OrderedType.GT)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    match compare x y with
    | OrderedType.EQ -> true
    | _ -> false
 end

module PositiveOrderedTypeBits = 
 struct 
  type t = int
  
  (** val compare : t -> t -> int OrderedType.coq_Compare **)
  
  let rec compare p y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun y0 ->
        compare p0 y0)
        (fun y0 ->
        OrderedType.GT)
        (fun _ ->
        OrderedType.GT)
        y)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun y0 ->
        OrderedType.LT)
        (fun y0 ->
        compare p0 y0)
        (fun _ ->
        OrderedType.LT)
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun y0 ->
        OrderedType.LT)
        (fun y0 ->
        OrderedType.GT)
        (fun _ ->
        OrderedType.EQ)
        y)
      p
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec x y =
    match Pos.compare x y with
    | Eq -> true
    | _ -> false
 end

