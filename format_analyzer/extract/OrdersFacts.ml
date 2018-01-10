open Basics
open Bool
open Datatypes
open Orders
open OrdersTac

module type CompareFacts = 
 functor (O:DecStrOrder') ->
 sig 
  
 end

module type OrderedTypeFullFacts = 
 functor (O:OrderedTypeFull') ->
 sig 
  module OrderTac : 
   sig 
    module TO : 
     sig 
      type t = O.t
      
      val compare : t -> t -> comparison
      
      val eq_dec : t -> t -> bool
     end
   end
 end

module OrderedTypeFacts = 
 functor (O:OrderedType') ->
 struct 
  module OrderTac = OT_to_OrderTac(O)
  
  (** val eq_dec : O.t -> O.t -> bool **)
  
  let eq_dec =
    O.eq_dec
  
  (** val lt_dec : O.t -> O.t -> bool **)
  
  let lt_dec x y =
    let c = coq_CompSpec2Type x y (O.compare x y) in
    (match c with
     | CompLtT -> true
     | _ -> false)
  
  (** val eqb : O.t -> O.t -> bool **)
  
  let eqb x y =
    if eq_dec x y then true else false
 end

module OrderedTypeTest = 
 functor (O:OrderedType') ->
 struct 
  module MO = OrderedTypeFacts(O)
 end

module OrderedTypeRev = 
 functor (O:OrderedTypeFull) ->
 struct 
  type t = O.t
  
  (** val eq_dec : O.t -> O.t -> bool **)
  
  let eq_dec =
    O.eq_dec
  
  (** val compare : O.t -> O.t -> comparison **)
  
  let compare =
    flip O.compare
 end

module type CompareBasedOrder = 
 functor (E:EqLtLe') ->
 functor (C:sig 
  val compare : E.t -> E.t -> comparison
 end) ->
 sig 
  
 end

module type CompareBasedOrderFacts = 
 functor (E:EqLtLe') ->
 functor (C:sig 
  val compare : E.t -> E.t -> comparison
 end) ->
 functor (O:sig 
  
 end) ->
 sig 
  
 end

module type BoolOrderFacts = 
 functor (E:EqLtLe') ->
 functor (C:sig 
  val compare : E.t -> E.t -> comparison
 end) ->
 functor (F:sig 
  val eqb : E.t -> E.t -> bool
  
  val ltb : E.t -> E.t -> bool
  
  val leb : E.t -> E.t -> bool
 end) ->
 functor (O:sig 
  
 end) ->
 functor (S:sig 
  
 end) ->
 sig 
  val leb_spec0 : E.t -> E.t -> reflect
  
  val ltb_spec0 : E.t -> E.t -> reflect
 end

