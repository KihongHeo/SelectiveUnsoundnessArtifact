open BinInt
open DFMapAVL
open DList
open DNat
open DProd
open DSum
open DUnit
open Datatypes
open InterNode
open IntraNode
open Syn
open TStr

type __ = Obj.t

module Step : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module CallId : 
 sig 
  type t = int
  
  val compare : int -> int -> int OrderedType.coq_Compare
  
  val eq_dec : int -> int -> bool
 end

module Proc : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module GVar : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module LVar : 
 sig 
  module E2 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = int
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : int -> int -> bool
      
      val lt_dec : int -> int -> bool
      
      val eqb : int -> int -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = string_t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : string_t -> string_t -> bool
      
      val lt_dec : string_t -> string_t -> bool
      
      val eqb : string_t -> string_t -> bool
     end
    
    type t = int * string_t
    
    val compare : t -> t -> (int * string_t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = int * string_t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : (int * string_t) -> (int * string_t) -> bool
    
    val lt_dec : (int * string_t) -> (int * string_t) -> bool
    
    val eqb : (int * string_t) -> (int * string_t) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = string_t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : string_t -> string_t -> bool
    
    val lt_dec : string_t -> string_t -> bool
    
    val eqb : string_t -> string_t -> bool
   end
  
  type t = (int * string_t) * string_t
  
  val compare :
    t -> t -> ((int * string_t) * string_t) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module Var : 
 sig 
  type t = (string_t, (int * string_t) * string_t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module ExtAllocsite : 
 sig 
  type t = (unit, string_t) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Allocsite : 
 sig 
  type t = (string_t * t', (unit, string_t) sum) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module OSS : 
 sig 
  module E2 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = int
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : int -> int -> bool
      
      val lt_dec : int -> int -> bool
      
      val eqb : int -> int -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = int
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : int -> int -> bool
      
      val lt_dec : int -> int -> bool
      
      val eqb : int -> int -> bool
     end
    
    type t = int * int
    
    val compare : t -> t -> (int * int) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = int * int
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : (int * int) -> (int * int) -> bool
    
    val lt_dec : (int * int) -> (int * int) -> bool
    
    val eqb : (int * int) -> (int * int) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = int
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : int -> int -> bool
    
    val lt_dec : int -> int -> bool
    
    val eqb : int -> int -> bool
   end
  
  type t = (int * int) * int
  
  val compare : t -> t -> ((int * int) * int) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module Region : 
 sig 
  module E2 : 
   sig 
    module MO1 : 
     sig 
      module TO : 
       sig 
        type t = int
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : int -> int -> bool
      
      val lt_dec : int -> int -> bool
      
      val eqb : int -> int -> bool
     end
    
    module MO2 : 
     sig 
      module TO : 
       sig 
        type t = (string_t * t', (unit, string_t) sum) sum
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec :
        (string_t * t', (unit, string_t) sum) sum -> (string_t * t', (unit,
        string_t) sum) sum -> bool
      
      val lt_dec :
        (string_t * t', (unit, string_t) sum) sum -> (string_t * t', (unit,
        string_t) sum) sum -> bool
      
      val eqb :
        (string_t * t', (unit, string_t) sum) sum -> (string_t * t', (unit,
        string_t) sum) sum -> bool
     end
    
    type t = int * (string_t * t', (unit, string_t) sum) sum
    
    val compare :
      t -> t -> (int * (string_t * t', (unit, string_t) sum) sum)
      OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t = int * (string_t * t', (unit, string_t) sum) sum
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec :
      (int * (string_t * t', (unit, string_t) sum) sum) ->
      (int * (string_t * t', (unit, string_t) sum) sum) -> bool
    
    val lt_dec :
      (int * (string_t * t', (unit, string_t) sum) sum) ->
      (int * (string_t * t', (unit, string_t) sum) sum) -> bool
    
    val eqb :
      (int * (string_t * t', (unit, string_t) sum) sum) ->
      (int * (string_t * t', (unit, string_t) sum) sum) -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = (int * int) * int
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : ((int * int) * int) -> ((int * int) * int) -> bool
    
    val lt_dec : ((int * int) * int) -> ((int * int) * int) -> bool
    
    val eqb : ((int * int) * int) -> ((int * int) * int) -> bool
   end
  
  type t =
    (int * (string_t * t', (unit, string_t) sum) sum) * ((int * int) * int)
  
  val compare :
    t -> t -> ((int * (string_t * t', (unit, string_t) sum)
    sum) * ((int * int) * int)) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : (('a1 * 'a2) * 'a3) -> 'a1
  
  val snd : (('a1 * 'a2) * 'a3) -> 'a2
  
  val thrd : (('a1 * 'a2) * 'a3) -> 'a3
 end

module VarRegion : 
 sig 
  type t =
    ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
    (unit, string_t) sum) sum) * ((int * int) * int)) sum
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Field : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Fields : 
 sig 
  module F : 
   sig 
    module TO : 
     sig 
      type t = string_t
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : string_t -> string_t -> bool
    
    val lt_dec : string_t -> string_t -> bool
    
    val eqb : string_t -> string_t -> bool
   end
  
  type t = string_t list
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module Loc : 
 sig 
  module MO1 : 
   sig 
    module TO : 
     sig 
      type t =
        ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec :
      ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum -> ((string_t,
      (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
      string_t) sum) sum) * ((int * int) * int)) sum -> bool
    
    val lt_dec :
      ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum -> ((string_t,
      (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
      string_t) sum) sum) * ((int * int) * int)) sum -> bool
    
    val eqb :
      ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum -> ((string_t,
      (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
      string_t) sum) sum) * ((int * int) * int)) sum -> bool
   end
  
  module MO2 : 
   sig 
    module TO : 
     sig 
      type t = string_t list
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : string_t list -> string_t list -> bool
    
    val lt_dec : string_t list -> string_t list -> bool
    
    val eqb : string_t list -> string_t list -> bool
   end
  
  type t =
    ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
    (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list
  
  val compare :
    t -> t -> (((string_t, (int * string_t) * string_t) sum,
    (int * (string_t * t', (unit, string_t) sum) sum) * ((int * int) * int))
    sum * string_t list) OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
  
  val fst : ('a1 * 'a2) -> 'a1
  
  val snd : ('a1 * 'a2) -> 'a2
 end

type val_t = ((Z.t, Loc.t) sum, Proc.t) sum

module M : 
 sig 
  module E : 
   sig 
    type t =
      ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list
    
    val compare :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
      (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
      string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      OrderedType.coq_Compare
    
    val eq_dec :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> bool
   end
  
  module Raw : 
   sig 
    type key =
      ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list
    
    type 'elt tree =
    | Leaf
    | Node of 'elt tree * key * 'elt * 'elt tree * Int.Z_as_Int.t
    
    val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
      Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2
    
    val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
      Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2
    
    val height : 'a1 tree -> Int.Z_as_Int.t
    
    val cardinal : 'a1 tree -> int
    
    val empty : 'a1 tree
    
    val is_empty : 'a1 tree -> bool
    
    val mem :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> 'a1 tree -> bool
    
    val find :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> 'a1 tree -> 'a1 option
    
    val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
    
    val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
    
    val remove :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> 'a1 tree -> 'a1 tree
    
    val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                         t_right : 'elt tree }
    
    val triple_rect :
      ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2
    
    val triple_rec :
      ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2
    
    val t_left : 'a1 triple -> 'a1 tree
    
    val t_opt : 'a1 triple -> 'a1 option
    
    val t_right : 'a1 triple -> 'a1 tree
    
    val split :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> 'a1 tree -> 'a1 triple
    
    val concat : 'a1 tree -> 'a1 tree -> 'a1 tree
    
    val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list
    
    val elements : 'a1 tree -> (key * 'a1) list
    
    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2
    
    type 'elt enumeration =
    | End
    | More of key * 'elt * 'elt tree * 'elt enumeration
    
    val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2
    
    val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2
    
    val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration
    
    val equal_more :
      ('a1 -> 'a1 -> bool) -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> 'a1 -> ('a1
      enumeration -> bool) -> 'a1 enumeration -> bool
    
    val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool
    
    val equal_end : 'a1 enumeration -> bool
    
    val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool
    
    val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree
    
    val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree
    
    val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree
    
    val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree
    
    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree
    
    module Proofs : 
     sig 
      module MX : 
       sig 
        module TO : 
         sig 
          type t =
            ((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list
         end
        
        module IsTO : 
         sig 
          
         end
        
        module OrderTac : 
         sig 
          
         end
        
        val eq_dec :
          (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          bool
        
        val lt_dec :
          (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          bool
        
        val eqb :
          (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          bool
       end
      
      module PX : 
       sig 
        module MO : 
         sig 
          module TO : 
           sig 
            type t =
              ((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
          
          val lt_dec :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
          
          val eqb :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
         end
       end
      
      module L : 
       sig 
        module MX : 
         sig 
          module TO : 
           sig 
            type t =
              ((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
          
          val lt_dec :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
          
          val eqb :
            (((string_t, (int * string_t) * string_t) sum,
            (int * (string_t * t', (unit, string_t) sum)
            sum) * ((int * int) * int)) sum * string_t list) -> (((string_t,
            (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
            string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
            -> bool
         end
        
        module PX : 
         sig 
          module MO : 
           sig 
            module TO : 
             sig 
              type t =
                ((string_t, (int * string_t) * string_t) sum,
                (int * (string_t * t', (unit, string_t) sum)
                sum) * ((int * int) * int)) sum * string_t list
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec :
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) ->
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) -> bool
            
            val lt_dec :
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) ->
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) -> bool
            
            val eqb :
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) ->
              (((string_t, (int * string_t) * string_t) sum,
              (int * (string_t * t', (unit, string_t) sum)
              sum) * ((int * int) * int)) sum * string_t list) -> bool
           end
         end
        
        type key =
          ((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list
        
        type 'elt t =
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        
        val empty : 'a1 t
        
        val is_empty : 'a1 t -> bool
        
        val mem : key -> 'a1 t -> bool
        
        type 'elt coq_R_mem =
        | R_mem_0 of 'elt t
        | R_mem_1 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_mem_2 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_mem_3 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * bool * 'elt coq_R_mem
        
        val coq_R_mem_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
          bool -> 'a1 coq_R_mem -> 'a2
        
        val coq_R_mem_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
          bool -> 'a1 coq_R_mem -> 'a2
        
        val mem_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val mem_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
        
        val find : key -> 'a1 t -> 'a1 option
        
        type 'elt coq_R_find =
        | R_find_0 of 'elt t
        | R_find_1 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_find_2 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_find_3 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * 'elt option * 'elt coq_R_find
        
        val coq_R_find_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t
          -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
        val coq_R_find_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t
          -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
        val find_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val find_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_find_correct : key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
        
        val add : key -> 'a1 -> 'a1 t -> 'a1 t
        
        type 'elt coq_R_add =
        | R_add_0 of 'elt t
        | R_add_1 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_add_2 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_add_3 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * 'elt t * 'elt coq_R_add
        
        val coq_R_add_rect :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
          'a1 t -> 'a1 coq_R_add -> 'a2
        
        val coq_R_add_rec :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
          'a1 t -> 'a1 coq_R_add -> 'a2
        
        val add_rect :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val add_rec :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_add_correct : key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
        
        val remove : key -> 'a1 t -> 'a1 t
        
        type 'elt coq_R_remove =
        | R_remove_0 of 'elt t
        | R_remove_1 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_remove_2 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
        | R_remove_3 of 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * 'elt t * 'elt coq_R_remove
        
        val coq_R_remove_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t ->
          'a1 t -> 'a1 coq_R_remove -> 'a2
        
        val coq_R_remove_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t ->
          'a1 t -> 'a1 coq_R_remove -> 'a2
        
        val remove_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val remove_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_remove_correct : key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
        
        val elements : 'a1 t -> 'a1 t
        
        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
        
        type ('elt, 'a) coq_R_fold =
        | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
        | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * 'a * ('elt, 'a) coq_R_fold
        
        val coq_R_fold_rect :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3
          -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
        
        val coq_R_fold_rec :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3
          -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
        
        val fold_rect :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
        
        val fold_rec :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
        
        val coq_R_fold_correct :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
          coq_R_fold
        
        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
        
        type 'elt coq_R_equal =
        | R_equal_0 of 'elt t * 'elt t
        | R_equal_1 of 'elt t * 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * bool * 'elt coq_R_equal
        | R_equal_2 of 'elt t * 'elt t
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt
           * ((((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list) * 'elt) list
           * (((string_t, (int * string_t) * string_t) sum,
             (int * (string_t * t', (unit, string_t) sum)
             sum) * ((int * int) * int)) sum * string_t list)
             OrderedType.coq_Compare
        | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
        
        val coq_R_equal_rect :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t ->
          'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list)
          OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
          'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool
          -> 'a1 coq_R_equal -> 'a2
        
        val coq_R_equal_rec :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t ->
          'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list)
          OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
          'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool
          -> 'a1 coq_R_equal -> 'a2
        
        val equal_rect :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list)
          OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
          'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
        
        val equal_rec :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> (((string_t,
          (int * string_t) * string_t) sum, (int * (string_t * t', (unit,
          string_t) sum) sum) * ((int * int) * int)) sum * string_t list) ->
          'a1 -> ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) -> 'a1 ->
          ((((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list) * 'a1) list -> __
          -> (((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list)
          OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
          'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
        
        val coq_R_equal_correct :
          ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal
        
        val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
        
        val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t
        
        val option_cons :
          key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list
        
        val map2_l :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t
        
        val map2_r :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t
        
        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
        
        val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t
        
        val fold_right_pair :
          ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3
        
        val map2_alt :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
          (key * 'a3) list
        
        val at_least_one :
          'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option
        
        val at_least_one_then_f :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
          option -> 'a3 option
       end
      
      type 'elt coq_R_mem =
      | R_mem_0 of 'elt tree
      | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * bool * 'elt coq_R_mem
      | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t
      | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * bool * 'elt coq_R_mem
      
      val coq_R_mem_rect :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
        coq_R_mem -> 'a2
      
      val coq_R_mem_rec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
        coq_R_mem -> 'a2
      
      type 'elt coq_R_find =
      | R_find_0 of 'elt tree
      | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
      | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t
      | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
      
      val coq_R_find_rect :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option ->
        'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
        -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree ->
        'a1 option -> 'a1 coq_R_find -> 'a2
      
      val coq_R_find_rec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option ->
        'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
        -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree ->
        'a1 option -> 'a1 coq_R_find -> 'a2
      
      type 'elt coq_R_bal =
      | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t
      | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t
      | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t
      | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t
      | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t
      | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t
      | R_bal_8 of 'elt tree * key * 'elt * 'elt tree
      
      val coq_R_bal_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_bal -> 'a2
      
      val coq_R_bal_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_bal -> 'a2
      
      type 'elt coq_R_add =
      | R_add_0 of 'elt tree
      | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
      | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t
      | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
      
      val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
        'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
        -> __ -> 'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 coq_R_add -> 'a2
      
      val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
        'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
        -> __ -> 'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 coq_R_add -> 'a2
      
      type 'elt coq_R_remove_min =
      | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
      | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
         * key * 'elt * 'elt tree * Int.Z_as_Int.t
         * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min * 'elt tree
         * (key * 'elt)
      
      val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
        coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1
        coq_R_remove_min -> 'a2
      
      val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
        coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1
        coq_R_remove_min -> 'a2
      
      type 'elt coq_R_merge =
      | R_merge_0 of 'elt tree * 'elt tree
      | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t
      | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt
      
      val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2)
        -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
        -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge
        -> 'a2
      
      val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2)
        -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
        -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge
        -> 'a2
      
      type 'elt coq_R_remove =
      | R_remove_0 of 'elt tree
      | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
      | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t
      | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
      
      val coq_R_remove_rect :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
        __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
      
      val coq_R_remove_rec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
        __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
      
      type 'elt coq_R_concat =
      | R_concat_0 of 'elt tree * 'elt tree
      | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t
      | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
      
      val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2)
        -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1
        tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
      
      val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2)
        -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1
        tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
      
      type 'elt coq_R_split =
      | R_split_0 of 'elt tree
      | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree
      | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t
      | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree
      
      val coq_R_split_rect :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple ->
        'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
        triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
        tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split ->
        'a2
      
      val coq_R_split_rec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple ->
        'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
        triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
        tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split ->
        'a2
      
      type ('elt, 'elt') coq_R_map_option =
      | R_map_option_0 of 'elt tree
      | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt' * 'elt' tree
         * ('elt, 'elt') coq_R_map_option * 'elt' tree
         * ('elt, 'elt') coq_R_map_option
      | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Int.Z_as_Int.t * 'elt' tree * ('elt, 'elt') coq_R_map_option
         * 'elt' tree * ('elt, 'elt') coq_R_map_option
      
      val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 ->
        __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2 tree
        -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3
      
      val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 ->
        __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2 tree
        -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3
      
      type ('elt, 'elt', 'elt'') coq_R_map2_opt =
      | R_map2_opt_0 of 'elt tree * 'elt' tree
      | R_map2_opt_1 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t
      | R_map2_opt_2 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 'elt' * 'elt' tree
         * Int.Z_as_Int.t * 'elt' tree * 'elt' option * 'elt' tree * 
         'elt'' * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
         * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
      | R_map2_opt_3 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
         * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 'elt' * 'elt' tree
         * Int.Z_as_Int.t * 'elt' tree * 'elt' option * 'elt' tree
         * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
         * ('elt, 'elt', 'elt'') coq_R_map2_opt
      
      val coq_R_map2_opt_rect :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
        key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
        option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
        -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4
      
      val coq_R_map2_opt_rec :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
        key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
        option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
        -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4
      
      val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2
      
      val flatten_e : 'a1 enumeration -> (key * 'a1) list
     end
   end
  
  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)
  
  val bst_rect : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2
  
  val bst_rec : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2
  
  val this : 'a1 bst -> 'a1 Raw.tree
  
  type 'elt t = 'elt bst
  
  type key =
    ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
    (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list
  
  val empty : 'a1 t
  
  val is_empty : 'a1 t -> bool
  
  val add : key -> 'a1 -> 'a1 t -> 'a1 t
  
  val remove : key -> 'a1 t -> 'a1 t
  
  val mem : key -> 'a1 t -> bool
  
  val find : key -> 'a1 t -> 'a1 option
  
  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
  
  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t
  
  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
  
  val elements : 'a1 t -> (key * 'a1) list
  
  val cardinal : 'a1 t -> int
  
  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
  
  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
  
  module ME : 
   sig 
    module TO : 
     sig 
      type t =
        ((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> bool
    
    val lt_dec :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> bool
    
    val eqb :
      (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
      (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t list)
      -> (((string_t, (int * string_t) * string_t) sum,
      (int * (string_t * t', (unit, string_t) sum)
      sum) * ((int * int) * int)) sum * string_t list) -> bool
   end
  
  module O : 
   sig 
    module MO : 
     sig 
      module TO : 
       sig 
        type t =
          ((string_t, (int * string_t) * string_t) sum,
          (int * (string_t * t', (unit, string_t) sum)
          sum) * ((int * int) * int)) sum * string_t list
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> (((string_t, (int * string_t) * string_t) sum,
        (int * (string_t * t', (unit, string_t) sum)
        sum) * ((int * int) * int)) sum * string_t list) -> bool
      
      val lt_dec :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> (((string_t, (int * string_t) * string_t) sum,
        (int * (string_t * t', (unit, string_t) sum)
        sum) * ((int * int) * int)) sum * string_t list) -> bool
      
      val eqb :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> (((string_t, (int * string_t) * string_t) sum,
        (int * (string_t * t', (unit, string_t) sum)
        sum) * ((int * int) * int)) sum * string_t list) -> bool
     end
   end
  
  module P : 
   sig 
    module F : 
     sig 
      val eqb :
        (((string_t, (int * string_t) * string_t) sum, (int * (string_t * t',
        (unit, string_t) sum) sum) * ((int * int) * int)) sum * string_t
        list) -> (((string_t, (int * string_t) * string_t) sum,
        (int * (string_t * t', (unit, string_t) sum)
        sum) * ((int * int) * int)) sum * string_t list) -> bool
      
      val coq_In_dec : 'a1 t -> key -> bool
      
      val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option
     end
    
    val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3
    
    val of_list : (key * 'a1) list -> 'a1 t
    
    val to_list : 'a1 t -> (key * 'a1) list
    
    val fold_rec :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> __ -> 'a3) ->
      (key -> 'a1 -> 'a2 -> 'a1 t -> 'a1 t -> __ -> __ -> __ -> 'a3 -> 'a3)
      -> 'a3
    
    val fold_rec_bis :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t -> 'a2 ->
      __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> __ ->
      'a3 -> 'a3) -> 'a3
    
    val fold_rec_nodep :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1 -> 'a2
      -> __ -> 'a3 -> 'a3) -> 'a3
    
    val fold_rec_weak :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __ ->
      'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3 -> 'a3)
      -> 'a1 t -> 'a3
    
    val fold_rel :
      (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2 -> 'a3
      -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4 -> 'a4) ->
      'a4
    
    val map_induction :
      ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 -> __ ->
      __ -> 'a2) -> 'a1 t -> 'a2
    
    val map_induction_bis :
      ('a1 t -> 'a1 t -> __ -> 'a2 -> 'a2) -> 'a2 -> (key -> 'a1 -> 'a1 t ->
      __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
    
    val cardinal_inv_2 : 'a1 t -> int -> (key * 'a1)
    
    val cardinal_inv_2b : 'a1 t -> (key * 'a1)
    
    val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
    
    val for_all : (key -> 'a1 -> bool) -> 'a1 t -> bool
    
    val exists_ : (key -> 'a1 -> bool) -> 'a1 t -> bool
    
    val partition : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t * 'a1 t
    
    val update : 'a1 t -> 'a1 t -> 'a1 t
    
    val restrict : 'a1 t -> 'a1 t -> 'a1 t
    
    val diff : 'a1 t -> 'a1 t -> 'a1 t
    
    val coq_Partition_In : 'a1 t -> 'a1 t -> 'a1 t -> key -> bool
    
    val update_dec : 'a1 t -> 'a1 t -> key -> 'a1 -> bool
    
    val filter_dom : (key -> bool) -> 'a1 t -> 'a1 t
    
    val filter_range : ('a1 -> bool) -> 'a1 t -> 'a1 t
    
    val for_all_dom : (key -> bool) -> 'a1 t -> bool
    
    val for_all_range : ('a1 -> bool) -> 'a1 t -> bool
    
    val exists_dom : (key -> bool) -> 'a1 t -> bool
    
    val exists_range : ('a1 -> bool) -> 'a1 t -> bool
    
    val partition_dom : (key -> bool) -> 'a1 t -> 'a1 t * 'a1 t
    
    val partition_range : ('a1 -> bool) -> 'a1 t -> 'a1 t * 'a1 t
   end
  
  val gtb : (key * 'a1) -> (key * 'a1) -> bool
  
  val leb : (key * 'a1) -> (key * 'a1) -> bool
  
  val elements_lt : (key * 'a1) -> 'a1 t -> (key * 'a1) list
  
  val elements_ge : (key * 'a1) -> 'a1 t -> (key * 'a1) list
  
  val max_elt_aux : (key * 'a1) list -> (key * 'a1) option
  
  val max_elt : 'a1 t -> (key * 'a1) option
  
  val min_elt : 'a1 t -> (key * 'a1) option
  
  val map_induction_max :
    ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
    (int * string_t) * string_t) sum, (int * (string_t * t', (unit, string_t)
    sum) sum) * ((int * int) * int)) sum * string_t list) -> 'a1 -> __ -> __
    -> 'a2) -> 'a1 t -> 'a2
  
  val map_induction_min :
    ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
    (int * string_t) * string_t) sum, (int * (string_t * t', (unit, string_t)
    sum) sum) * ((int * int) * int)) sum * string_t list) -> 'a1 -> __ -> __
    -> 'a2) -> 'a1 t -> 'a2
  
  module Raw2 : 
   sig 
    val for_all :
      (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 Raw.tree -> bool
    
    val strong_le :
      ('a1 -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool
    
    val filter : (key -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree
    
    type 'elt coq_R_for_all =
    | R_for_all_0 of 'elt Raw.tree
    | R_for_all_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
       * 'elt Raw.tree * Int.Z_as_Int.t * bool * 'elt coq_R_for_all * 
       bool * 'elt coq_R_for_all
    
    val coq_R_for_all_rect :
      (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ ->
      'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2 -> bool ->
      'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool -> 'a1
      coq_R_for_all -> 'a2
    
    val coq_R_for_all_rec :
      (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ ->
      'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2 -> bool ->
      'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool -> 'a1
      coq_R_for_all -> 'a2
    
    type 'elt coq_R_strong_le =
    | R_strong_le_0 of 'elt Raw.tree * 'elt Raw.tree
    | R_strong_le_1 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t
    | R_strong_le_2 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * bool
       * 'elt coq_R_strong_le * bool * 'elt coq_R_strong_le
    | R_strong_le_3 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * bool
       * 'elt coq_R_strong_le * bool * 'elt coq_R_strong_le
    | R_strong_le_4 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
       * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t * bool
       * 'elt coq_R_strong_le * bool * 'elt coq_R_strong_le
    
    val coq_R_strong_le_rect :
      ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2) ->
      ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
      Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
      Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1
      Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2
    
    val coq_R_strong_le_rec :
      ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2) ->
      ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
      Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
      Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
      Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree
      -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
      __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1
      Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2
    
    type 'elt coq_R_filter =
    | R_filter_0 of 'elt Raw.tree
    | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
       * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
       * 'elt Raw.tree * 'elt coq_R_filter
    | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
       * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
       * 'elt Raw.tree * 'elt coq_R_filter
    
    val coq_R_filter_rect :
      (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __
      -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
      coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree ->
      Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree
      -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
      -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter ->
      'a2
    
    val coq_R_filter_rec :
      (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree ->
      'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __
      -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
      coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree ->
      Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree
      -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
      -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter ->
      'a2
   end
  
  val for_all :
    (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool
  
  val strong_le : ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool
  
  val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
 end

type mem_t = val_t M.t

type retLoc_t = ((InterNode.t * Loc.t option) * CallId.t) * Proc.t

type dump_t = retLoc_t list

type mem_pos =
| Inputof
| Outputof

val mem_pos_rect : 'a1 -> 'a1 -> mem_pos -> 'a1

val mem_pos_rec : 'a1 -> 'a1 -> mem_pos -> 'a1

type state_t =
  ((((mem_pos * InterNode.t) * CallId.t) * Step.t) * mem_t) * dump_t

val val_of_z : int -> val_t

val val_of_loc : Loc.t -> val_t

val val_of_proc : Proc.t -> val_t

val loc_of_gvar : vid_t -> vid_t list -> Loc.t

val loc_of_lvar : CallId.t -> Proc.t -> vid_t -> vid_t list -> Loc.t

val loc_of_alloc : Step.t -> Allocsite.t -> OSS.t -> vid_t list -> Loc.t

