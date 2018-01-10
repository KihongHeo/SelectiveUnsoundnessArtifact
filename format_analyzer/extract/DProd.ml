open Basics
open DLat
open Datatypes
open OrderedTypeEx
open OrdersTac
open Sumbool
open VocabA

module ProdKey = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 PairOrderedType(A)(B)

module Prod = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 struct 
  type t = A.t * B.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (A.le_dec x1 y1) (B.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (A.eq_dec x1 y1) (B.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (A.bot, B.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((A.join (fst x) (fst y)), (B.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((A.meet (fst x) (fst y)), (B.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((A.widen (fst x) (fst y)), (B.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((A.narrow (fst x) (fst y)), (B.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
 end

module Get2 = 
 struct 
  (** val fst : ('a1 * 'a2) -> 'a1 **)
  
  let fst =
    fst
  
  (** val snd : ('a1 * 'a2) -> 'a2 **)
  
  let snd =
    snd
 end

module Get3 = 
 struct 
  (** val fst : (('a1 * 'a2) * 'a3) -> 'a1 **)
  
  let fst x =
    compose Get2.fst fst x
  
  (** val snd : (('a1 * 'a2) * 'a3) -> 'a2 **)
  
  let snd x =
    compose Get2.snd Datatypes.fst x
  
  (** val thrd : (('a1 * 'a2) * 'a3) -> 'a3 **)
  
  let thrd x =
    Datatypes.snd x
 end

module Get4 = 
 struct 
  (** val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1 **)
  
  let fst x =
    compose Get3.fst fst x
  
  (** val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2 **)
  
  let snd x =
    compose Get3.snd Datatypes.fst x
  
  (** val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3 **)
  
  let thrd x =
    compose Get3.thrd Datatypes.fst x
  
  (** val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4 **)
  
  let frth x =
    Datatypes.snd x
 end

module Get5 = 
 struct 
  (** val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1 **)
  
  let fst x =
    compose Get4.fst fst x
  
  (** val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2 **)
  
  let snd x =
    compose Get4.snd Datatypes.fst x
  
  (** val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3 **)
  
  let thrd x =
    compose Get4.thrd Datatypes.fst x
  
  (** val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4 **)
  
  let frth x =
    compose Get4.frth Datatypes.fst x
  
  (** val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5 **)
  
  let fifth x =
    Datatypes.snd x
 end

module ProdKey2 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 struct 
  module MO1 = 
   struct 
    module TO = 
     struct 
      type t = A.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : A.t -> A.t -> bool **)
    
    let eq_dec =
      A.eq_dec
    
    (** val lt_dec : A.t -> A.t -> bool **)
    
    let lt_dec x y =
      match A.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : A.t -> A.t -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  module MO2 = 
   struct 
    module TO = 
     struct 
      type t = B.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : B.t -> B.t -> bool **)
    
    let eq_dec =
      B.eq_dec
    
    (** val lt_dec : B.t -> B.t -> bool **)
    
    let lt_dec x y =
      match B.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : B.t -> B.t -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  type t = A.t * B.t
  
  (** val compare : t -> t -> (A.t * B.t) OrderedType.coq_Compare **)
  
  let compare x y =
    let (x1, x2) = x in
    let (y1, y2) = y in
    let c = A.compare x1 y1 in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = B.compare x2 y2 in
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
  
  (** val fst : ('a1 * 'a2) -> 'a1 **)
  
  let fst =
    fst
  
  (** val snd : ('a1 * 'a2) -> 'a2 **)
  
  let snd =
    snd
 end

module ProdKey3 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 struct 
  module E2 = ProdKey(A)(B)
  
  module MO1 = 
   struct 
    module TO = 
     struct 
      type t = A.t * B.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : (A.t * B.t) -> (A.t * B.t) -> bool **)
    
    let eq_dec =
      E2.eq_dec
    
    (** val lt_dec : (A.t * B.t) -> (A.t * B.t) -> bool **)
    
    let lt_dec x y =
      match E2.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : (A.t * B.t) -> (A.t * B.t) -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  module MO2 = 
   struct 
    module TO = 
     struct 
      type t = C.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : C.t -> C.t -> bool **)
    
    let eq_dec =
      C.eq_dec
    
    (** val lt_dec : C.t -> C.t -> bool **)
    
    let lt_dec x y =
      match C.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : C.t -> C.t -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  type t = (A.t * B.t) * C.t
  
  (** val compare : t -> t -> ((A.t * B.t) * C.t) OrderedType.coq_Compare **)
  
  let compare x y =
    let (x1, x2) = x in
    let (y1, y2) = y in
    let c = E2.compare x1 y1 in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = C.compare x2 y2 in
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
  
  (** val fst : (('a1 * 'a2) * 'a3) -> 'a1 **)
  
  let fst x =
    compose Get2.fst fst x
  
  (** val snd : (('a1 * 'a2) * 'a3) -> 'a2 **)
  
  let snd x =
    compose Get2.snd Datatypes.fst x
  
  (** val thrd : (('a1 * 'a2) * 'a3) -> 'a3 **)
  
  let thrd x =
    Datatypes.snd x
 end

module ProdKey4 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 struct 
  module E2 = ProdKey(A)(B)
  
  module E3 = ProdKey(E2)(C)
  
  module MO1 = 
   struct 
    module TO = 
     struct 
      type t = (A.t * B.t) * C.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool **)
    
    let eq_dec =
      E3.eq_dec
    
    (** val lt_dec : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool **)
    
    let lt_dec x y =
      match E3.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : ((A.t * B.t) * C.t) -> ((A.t * B.t) * C.t) -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  module MO2 = 
   struct 
    module TO = 
     struct 
      type t = D.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : D.t -> D.t -> bool **)
    
    let eq_dec =
      D.eq_dec
    
    (** val lt_dec : D.t -> D.t -> bool **)
    
    let lt_dec x y =
      match D.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : D.t -> D.t -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  type t = ((A.t * B.t) * C.t) * D.t
  
  (** val compare :
      t -> t -> (((A.t * B.t) * C.t) * D.t) OrderedType.coq_Compare **)
  
  let compare x y =
    let (x1, x2) = x in
    let (y1, y2) = y in
    let c = E3.compare x1 y1 in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = D.compare x2 y2 in
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
  
  (** val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1 **)
  
  let fst x =
    compose Get3.fst fst x
  
  (** val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2 **)
  
  let snd x =
    compose Get3.snd Datatypes.fst x
  
  (** val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3 **)
  
  let thrd x =
    compose Get3.thrd Datatypes.fst x
  
  (** val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4 **)
  
  let frth x =
    Datatypes.snd x
 end

module ProdKey5 = 
 functor (A:KEY) ->
 functor (B:KEY) ->
 functor (C:KEY) ->
 functor (D:KEY) ->
 functor (E:KEY) ->
 struct 
  module E2 = ProdKey(A)(B)
  
  module E3 = ProdKey(E2)(C)
  
  module E4 = ProdKey(E3)(D)
  
  module MO1 = 
   struct 
    module TO = 
     struct 
      type t = ((A.t * B.t) * C.t) * D.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec :
        (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool **)
    
    let eq_dec =
      E4.eq_dec
    
    (** val lt_dec :
        (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool **)
    
    let lt_dec x y =
      match E4.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb :
        (((A.t * B.t) * C.t) * D.t) -> (((A.t * B.t) * C.t) * D.t) -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  module MO2 = 
   struct 
    module TO = 
     struct 
      type t = E.t
     end
    
    module IsTO = 
     struct 
      
     end
    
    module OrderTac = MakeOrderTac(TO)(IsTO)
    
    (** val eq_dec : E.t -> E.t -> bool **)
    
    let eq_dec =
      E.eq_dec
    
    (** val lt_dec : E.t -> E.t -> bool **)
    
    let lt_dec x y =
      match E.compare x y with
      | OrderedType.LT -> true
      | _ -> false
    
    (** val eqb : E.t -> E.t -> bool **)
    
    let eqb x y =
      if eq_dec x y then true else false
   end
  
  type t = (((A.t * B.t) * C.t) * D.t) * E.t
  
  (** val compare :
      t -> t -> ((((A.t * B.t) * C.t) * D.t) * E.t) OrderedType.coq_Compare **)
  
  let compare x y =
    let (x1, x2) = x in
    let (y1, y2) = y in
    let c = E4.compare x1 y1 in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = E.compare x2 y2 in
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
  
  (** val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1 **)
  
  let fst x =
    compose Get4.fst fst x
  
  (** val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2 **)
  
  let snd x =
    compose Get4.snd Datatypes.fst x
  
  (** val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3 **)
  
  let thrd x =
    compose Get4.thrd Datatypes.fst x
  
  (** val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4 **)
  
  let frth x =
    compose Get4.frth Datatypes.fst x
  
  (** val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5 **)
  
  let fifth x =
    Datatypes.snd x
 end

module Prod2 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 struct 
  type t = A.t * B.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (A.le_dec x1 y1) (B.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (A.eq_dec x1 y1) (B.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (A.bot, B.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((A.join (fst x) (fst y)), (B.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((A.meet (fst x) (fst y)), (B.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((A.widen (fst x) (fst y)), (B.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((A.narrow (fst x) (fst y)), (B.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val fst : ('a1 * 'a2) -> 'a1 **)
  
  let fst =
    fst
  
  (** val snd : ('a1 * 'a2) -> 'a2 **)
  
  let snd =
    snd
 end

module Prod3 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 struct 
  module E2 = Prod(A)(B)
  
  type t = E2.t * C.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E2.le_dec x1 y1) (C.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E2.eq_dec x1 y1) (C.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (E2.bot, C.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((E2.join (fst x) (fst y)), (C.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((E2.meet (fst x) (fst y)), (C.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((E2.widen (fst x) (fst y)), (C.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((E2.narrow (fst x) (fst y)), (C.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val fst : (('a1 * 'a2) * 'a3) -> 'a1 **)
  
  let fst x =
    compose Get2.fst fst x
  
  (** val snd : (('a1 * 'a2) * 'a3) -> 'a2 **)
  
  let snd x =
    compose Get2.snd Datatypes.fst x
  
  (** val thrd : (('a1 * 'a2) * 'a3) -> 'a3 **)
  
  let thrd x =
    Datatypes.snd x
 end

module Prod4 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 struct 
  module E2 = Prod(A)(B)
  
  module E3 = Prod(E2)(C)
  
  type t = E3.t * D.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E3.le_dec x1 y1) (D.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E3.eq_dec x1 y1) (D.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (E3.bot, D.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((E3.join (fst x) (fst y)), (D.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((E3.meet (fst x) (fst y)), (D.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((E3.widen (fst x) (fst y)), (D.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((E3.narrow (fst x) (fst y)), (D.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1 **)
  
  let fst x =
    compose Get3.fst fst x
  
  (** val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2 **)
  
  let snd x =
    compose Get3.snd Datatypes.fst x
  
  (** val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3 **)
  
  let thrd x =
    compose Get3.thrd Datatypes.fst x
  
  (** val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4 **)
  
  let frth x =
    Datatypes.snd x
 end

module Prod5 = 
 functor (A:LAT) ->
 functor (B:LAT) ->
 functor (C:LAT) ->
 functor (D:LAT) ->
 functor (E:LAT) ->
 struct 
  module E2 = Prod(A)(B)
  
  module E3 = Prod(E2)(C)
  
  module E4 = Prod(E3)(D)
  
  type t = E4.t * E.t
  
  (** val le_dec : t -> t -> bool **)
  
  let le_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E4.le_dec x1 y1) (E.le_dec x2 y2)
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y
    then true
    else let (x1, x2) = x in
         let (y1, y2) = y in sumbool_and (E4.eq_dec x1 y1) (E.eq_dec x2 y2)
  
  (** val bot : t **)
  
  let bot =
    (E4.bot, E.bot)
  
  (** val join : t -> t -> t **)
  
  let join x y =
    if le_dec x y
    then y
    else if le_dec y x
         then x
         else ((E4.join (fst x) (fst y)), (E.join (snd x) (snd y)))
  
  (** val meet : t -> t -> t **)
  
  let meet x y =
    if le_dec x y
    then x
    else if le_dec y x
         then y
         else ((E4.meet (fst x) (fst y)), (E.meet (snd x) (snd y)))
  
  (** val widen : t -> t -> t **)
  
  let widen x y =
    if structural_eq x y
    then x
    else ((E4.widen (fst x) (fst y)), (E.widen (snd x) (snd y)))
  
  (** val narrow : t -> t -> t **)
  
  let narrow x y =
    if structural_eq x y
    then x
    else ((E4.narrow (fst x) (fst y)), (E.narrow (snd x) (snd y)))
  
  (** val coq_ILat : t coq_TCLat **)
  
  let coq_ILat =
    { DLat.le_dec = le_dec; DLat.eq_dec = eq_dec; DLat.bot = bot; DLat.join =
      join; DLat.meet = meet; DLat.widen = widen; DLat.narrow = narrow }
  
  (** val fst : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a1 **)
  
  let fst x =
    compose Get4.fst fst x
  
  (** val snd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a2 **)
  
  let snd x =
    compose Get4.snd Datatypes.fst x
  
  (** val thrd : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a3 **)
  
  let thrd x =
    compose Get4.thrd Datatypes.fst x
  
  (** val frth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a4 **)
  
  let frth x =
    compose Get4.frth Datatypes.fst x
  
  (** val fifth : (((('a1 * 'a2) * 'a3) * 'a4) * 'a5) -> 'a5 **)
  
  let fifth x =
    Datatypes.snd x
 end

