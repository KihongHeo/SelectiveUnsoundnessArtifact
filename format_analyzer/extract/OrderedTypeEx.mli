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

module UOT_to_OT : 
 functor (U:UsualOrderedType) ->
 sig 
  type t = U.t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Nat_as_OT : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module Z_as_OT : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module Positive_as_OT : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module N_as_OT : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module PairOrderedType : 
 functor (O1:OrderedType.OrderedType) ->
 functor (O2:OrderedType.OrderedType) ->
 sig 
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = O1.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : O1.t -> O1.t -> bool
    
    val lt_dec : O1.t -> O1.t -> bool
    
    val eqb : O1.t -> O1.t -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = O2.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : O2.t -> O2.t -> bool
    
    val lt_dec : O2.t -> O2.t -> bool
    
    val eqb : O2.t -> O2.t -> bool
   end
  
  type t = O1.t * O2.t
  
  val compare : t -> t -> (O1.t * O2.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module PositiveOrderedTypeBits : 
 sig 
  type t = int
  
  val compare : t -> t -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

