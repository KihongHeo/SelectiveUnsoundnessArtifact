open DLat
open Datatypes
open TStr
open VocabA

val sum_narrow_msg : string_t

module SumKey : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 sig 
  type t = (A.t, B.t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Sum : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 sig 
  type t' =
  | Top
  | Inl of A.t
  | Inr of B.t
  | Bot
  
  val t'_rect : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  val t'_rec : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  type t = t'
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

module SumKey2 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 sig 
  type t = (A.t, B.t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module SumKey3 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 sig 
  module E2 : 
   sig 
    type t = (A.t, B.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  type t = ((A.t, B.t) sum, C.t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module SumKey4 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 sig 
  module E2 : 
   sig 
    type t = (A.t, B.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E3 : 
   sig 
    type t = ((A.t, B.t) sum, C.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  type t = (((A.t, B.t) sum, C.t) sum, D.t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module SumKey5 : 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 functor (E:KEY) ->
 sig 
  module E2 : 
   sig 
    type t = (A.t, B.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E3 : 
   sig 
    type t = ((A.t, B.t) sum, C.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module E4 : 
   sig 
    type t = (((A.t, B.t) sum, C.t) sum, D.t) sum
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  type t = ((((A.t, B.t) sum, C.t) sum, D.t) sum, E.t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Sum2 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 sig 
  type t' =
  | Top
  | Inl of A.t
  | Inr of B.t
  | Bot
  
  val t'_rect : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  val t'_rec : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  type t = t'
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

module Sum3 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 sig 
  module E2 : 
   sig 
    type t' =
    | Top
    | Inl of A.t
    | Inr of B.t
    | Bot
    
    val t'_rect : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t' =
  | Top
  | Inl of E2.t
  | Inr of C.t
  | Bot
  
  val t'_rect : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  val t'_rec : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  type t = t'
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

module Sum4 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 sig 
  module E2 : 
   sig 
    type t' =
    | Top
    | Inl of A.t
    | Inr of B.t
    | Bot
    
    val t'_rect : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E3 : 
   sig 
    type t' =
    | Top
    | Inl of E2.t
    | Inr of C.t
    | Bot
    
    val t'_rect : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t' =
  | Top
  | Inl of E3.t
  | Inr of D.t
  | Bot
  
  val t'_rect : 'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  val t'_rec : 'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  type t = t'
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

module Sum5 : 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 functor (E:LAT) ->
 sig 
  module E2 : 
   sig 
    type t' =
    | Top
    | Inl of A.t
    | Inr of B.t
    | Bot
    
    val t'_rect : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (A.t -> 'a1) -> (B.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E3 : 
   sig 
    type t' =
    | Top
    | Inl of E2.t
    | Inr of C.t
    | Bot
    
    val t'_rect : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (E2.t -> 'a1) -> (C.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E4 : 
   sig 
    type t' =
    | Top
    | Inl of E3.t
    | Inr of D.t
    | Bot
    
    val t'_rect : 'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    val t'_rec : 'a1 -> (E3.t -> 'a1) -> (D.t -> 'a1) -> 'a1 -> t' -> 'a1
    
    type t = t'
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val top : t
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t' =
  | Top
  | Inl of E4.t
  | Inr of E.t
  | Bot
  
  val t'_rect : 'a1 -> (E4.t -> 'a1) -> (E.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  val t'_rec : 'a1 -> (E4.t -> 'a1) -> (E.t -> 'a1) -> 'a1 -> t' -> 'a1
  
  type t = t'
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val top : t
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
 end

