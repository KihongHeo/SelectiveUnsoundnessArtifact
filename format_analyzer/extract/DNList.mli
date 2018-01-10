open DLat
open VocabA

val size : int

module NListKey : 
 functor (A:KEY) ->
 sig 
  module F : 
   sig 
    module TO : 
     sig 
      type t = A.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : A.t -> A.t -> bool
    
    val lt_dec : A.t -> A.t -> bool
    
    val eqb : A.t -> A.t -> bool
   end
  
  type t = A.t list
  
  val compare' : int -> t -> t -> t OrderedType.coq_Compare
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec' : int -> t -> t -> bool
  
  val eq_dec : t -> t -> bool
 end

