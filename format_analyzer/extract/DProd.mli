open Basics
open DLat
open Datatypes
open OrderedTypeEx
open OrdersTac
open Sumbool
open VocabA

module ProdKey : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 sig 
  module MO1 : 
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
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = B.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : B.t -> B.t -> bool
    
    val lt_dec : B.t -> B.t -> bool
    
    val eqb : B.t -> B.t -> bool
   end
  
  type t = A.t * B.t
  
  val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Prod : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 sig 
  type t = A.t * B.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

module Get2 : 
 sig 
  val fst : ('a1 * 'a2) -> 'a1
  
  val snd : ('a1 * 'a2) -> 'a2
 end

module Get3 : 
 sig 
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module Get4 : 
 sig 
  val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1
  
  val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2
  
  val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3
  
  val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4
 end

module Get5 : 
 sig 
  val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1
  
  val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2
  
  val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3
  
  val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4
  
  val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5
 end

module ProdKey2 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 sig 
  module MO1 : 
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
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = B.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : B.t -> B.t -> bool
    
    val lt_dec : B.t -> B.t -> bool
    
    val eqb : B.t -> B.t -> bool
   end
  
  type t = A.t * B.t
  
  val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : ('a1 * 'a2) -> 'a1
  
  val snd : ('a1 * 'a2) -> 'a2
 end

module ProdKey3 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 sig 
  module E2 : 
   sig 
    module MO1 : 
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
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = B.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : B.t -> B.t -> bool
      
      val lt_dec : B.t -> B.t -> bool
      
      val eqb : B.t -> B.t -> bool
     end
    
    type t = A.t * B.t
    
    val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = A.t * B.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : (A.t * B.t) -> (A.t * B.t) -> bool
    
    val lt_dec : (A.t * B.t) -> (A.t * B.t) -> bool
    
    val eqb : (A.t * B.t) -> (A.t * B.t) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = C.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : C.t -> C.t -> bool
    
    val lt_dec : C.t -> C.t -> bool
    
    val eqb : C.t -> C.t -> bool
   end
  
  type t = (A.t * B.t) * C.t
  
  val compare : t -> t -> ((A.t * B.t) * C.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module ProdKey4 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 sig 
  module E2 : 
   sig 
    module MO1 : 
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
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = B.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : B.t -> B.t -> bool
      
      val lt_dec : B.t -> B.t -> bool
      
      val eqb : B.t -> B.t -> bool
     end
    
    type t = A.t * B.t
    
    val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E3 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = A.t * B.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : (A.t * B.t) -> (A.t * B.t) -> bool
      
      val lt_dec : (A.t * B.t) -> (A.t * B.t) -> bool
      
      val eqb : (A.t * B.t) -> (A.t * B.t) -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = C.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : C.t -> C.t -> bool
      
      val lt_dec : C.t -> C.t -> bool
      
      val eqb : C.t -> C.t -> bool
     end
    
    type t = (A.t * B.t) * C.t
    
    val compare : t -> t -> ((A.t * B.t) * C.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = (A.t * B.t) * C.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
    
    val lt_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
    
    val eqb : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = D.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : D.t -> D.t -> bool
    
    val lt_dec : D.t -> D.t -> bool
    
    val eqb : D.t -> D.t -> bool
   end
  
  type t = ((A.t * B.t) * C.t) * D.t
  
  val compare : t -> t -> (((A.t * B.t) * C.t) * D.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1
  
  val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2
  
  val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3
  
  val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4
 end

module ProdKey5 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 functor (E:KEY) ->
 sig 
  module E2 : 
   sig 
    module MO1 : 
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
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = B.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : B.t -> B.t -> bool
      
      val lt_dec : B.t -> B.t -> bool
      
      val eqb : B.t -> B.t -> bool
     end
    
    type t = A.t * B.t
    
    val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E3 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = A.t * B.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : (A.t * B.t) -> (A.t * B.t) -> bool
      
      val lt_dec : (A.t * B.t) -> (A.t * B.t) -> bool
      
      val eqb : (A.t * B.t) -> (A.t * B.t) -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = C.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : C.t -> C.t -> bool
      
      val lt_dec : C.t -> C.t -> bool
      
      val eqb : C.t -> C.t -> bool
     end
    
    type t = (A.t * B.t) * C.t
    
    val compare : t -> t -> ((A.t * B.t) * C.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E4 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = (A.t * B.t) * C.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
      
      val lt_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
      
      val eqb : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = D.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : D.t -> D.t -> bool
      
      val lt_dec : D.t -> D.t -> bool
      
      val eqb : D.t -> D.t -> bool
     end
    
    type t = ((A.t * B.t) * C.t) * D.t
    
    val compare :
      t -> t -> (((A.t * B.t) * C.t) * D.t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = ((A.t * B.t) * C.t) * D.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec :
      (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool
    
    val lt_dec :
      (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool
    
    val eqb :
      (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = E.t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : E.t -> E.t -> bool
    
    val lt_dec : E.t -> E.t -> bool
    
    val eqb : E.t -> E.t -> bool
   end
  
  type t = (((A.t * B.t) * C.t) * D.t) * E.t
  
  val compare :
    t -> t -> ((((A.t * B.t) * C.t) * D.t) * E.t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1
  
  val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2
  
  val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3
  
  val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4
  
  val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5
 end

module Prod2 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 sig 
  type t = A.t * B.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : ('a1 * 'a2) -> 'a1
  
  val snd : ('a1 * 'a2) -> 'a2
 end

module Prod3 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 sig 
  module E2 : 
   sig 
    type t = A.t * B.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t = E2.t * C.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module Prod4 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 sig 
  module E2 : 
   sig 
    type t = A.t * B.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E3 : 
   sig 
    type t = E2.t * C.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t = E3.t * D.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1
  
  val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2
  
  val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3
  
  val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4
 end

module Prod5 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 functor (E:LAT) ->
 sig 
  module E2 : 
   sig 
    type t = A.t * B.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E3 : 
   sig 
    type t = E2.t * C.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E4 : 
   sig 
    type t = E3.t * D.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t = E4.t * E.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1
  
  val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2
  
  val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3
  
  val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4
  
  val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5
 end

