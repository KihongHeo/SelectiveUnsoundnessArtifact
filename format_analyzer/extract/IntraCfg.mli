open DFMapAVL
open DFSetAVL
open Datatypes
open IntraNode
open Syn

type __ = Obj.t

module NodeMap : 
 sig 
  module E : 
   sig 
    type t = t'
    
    val compare : t' -> t' -> t' OrderedType.coq_Compare
    
    val eq_dec : t' -> t' -> bool
   end
  
  module Raw : 
   sig 
    type key = t'
    
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
    
    val mem : t' -> 'a1 tree -> bool
    
    val find : t' -> 'a1 tree -> 'a1 option
    
    val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
    
    val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
    
    val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
    
    val remove : t' -> 'a1 tree -> 'a1 tree
    
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
    
    val split : t' -> 'a1 tree -> 'a1 triple
    
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
      ('a1 -> 'a1 -> bool) -> t' -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool
    
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
          type t = t'
         end
        
        module IsTO : 
         sig 
          
         end
        
        module OrderTac : 
         sig 
          
         end
        
        val eq_dec : t' -> t' -> bool
        
        val lt_dec : t' -> t' -> bool
        
        val eqb : t' -> t' -> bool
       end
      
      module PX : 
       sig 
        module MO : 
         sig 
          module TO : 
           sig 
            type t = t'
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec : t' -> t' -> bool
          
          val lt_dec : t' -> t' -> bool
          
          val eqb : t' -> t' -> bool
         end
       end
      
      module L : 
       sig 
        module MX : 
         sig 
          module TO : 
           sig 
            type t = t'
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec : t' -> t' -> bool
          
          val lt_dec : t' -> t' -> bool
          
          val eqb : t' -> t' -> bool
         end
        
        module PX : 
         sig 
          module MO : 
           sig 
            module TO : 
             sig 
              type t = t'
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec : t' -> t' -> bool
            
            val lt_dec : t' -> t' -> bool
            
            val eqb : t' -> t' -> bool
           end
         end
        
        type key = t'
        
        type 'elt t = (t' * 'elt) list
        
        val empty : 'a1 t
        
        val is_empty : 'a1 t -> bool
        
        val mem : key -> 'a1 t -> bool
        
        type 'elt coq_R_mem =
        | R_mem_0 of 'elt t
        | R_mem_1 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_mem_2 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_mem_3 of 'elt t * t' * 'elt * (t' * 'elt) list * bool
           * 'elt coq_R_mem
        
        val coq_R_mem_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
          'a1 t -> bool -> 'a1 coq_R_mem -> 'a2
        
        val coq_R_mem_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
          'a1 t -> bool -> 'a1 coq_R_mem -> 'a2
        
        val mem_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val mem_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
        
        val find : key -> 'a1 t -> 'a1 option
        
        type 'elt coq_R_find =
        | R_find_0 of 'elt t
        | R_find_1 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_find_2 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_find_3 of 'elt t * t' * 'elt * (t' * 'elt) list * 'elt option
           * 'elt coq_R_find
        
        val coq_R_find_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
          'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
        val coq_R_find_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
          'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
        val find_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val find_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_find_correct : key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
        
        val add : key -> 'a1 -> 'a1 t -> 'a1 t
        
        type 'elt coq_R_add =
        | R_add_0 of 'elt t
        | R_add_1 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_add_2 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_add_3 of 'elt t * t' * 'elt * (t' * 'elt) list * 'elt t
           * 'elt coq_R_add
        
        val coq_R_add_rect :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2
          -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2
        
        val coq_R_add_rec :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2
          -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2
        
        val add_rect :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val add_rec :
          key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_add_correct : key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
        
        val remove : key -> 'a1 t -> 'a1 t
        
        type 'elt coq_R_remove =
        | R_remove_0 of 'elt t
        | R_remove_1 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_remove_2 of 'elt t * t' * 'elt * (t' * 'elt) list
        | R_remove_3 of 'elt t * t' * 'elt * (t' * 'elt) list * 'elt t
           * 'elt coq_R_remove
        
        val coq_R_remove_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2)
          -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
        
        val coq_R_remove_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2)
          -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
        
        val remove_rect :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val remove_rec :
          key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
        val coq_R_remove_correct : key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
        
        val elements : 'a1 t -> 'a1 t
        
        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
        
        type ('elt, 'a) coq_R_fold =
        | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
        | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * t' * 
           'elt * (t' * 'elt) list * 'a * ('elt, 'a) coq_R_fold
        
        val coq_R_fold_rect :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2)
          -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3)
          coq_R_fold -> 'a2
        
        val coq_R_fold_rec :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2)
          -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3)
          coq_R_fold -> 'a2
        
        val fold_rect :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3)
          -> 'a1 t -> 'a3 -> 'a2
        
        val fold_rec :
          (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__
          -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3)
          -> 'a1 t -> 'a3 -> 'a2
        
        val coq_R_fold_correct :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
          coq_R_fold
        
        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
        
        type 'elt coq_R_equal =
        | R_equal_0 of 'elt t * 'elt t
        | R_equal_1 of 'elt t * 'elt t * t' * 'elt * (t' * 'elt) list * 
           t' * 'elt * (t' * 'elt) list * bool * 'elt coq_R_equal
        | R_equal_2 of 'elt t * 'elt t * t' * 'elt * (t' * 'elt) list * 
           t' * 'elt * (t' * 'elt) list * t' OrderedType.coq_Compare
        | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
        
        val coq_R_equal_rect :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> bool -> 'a1 coq_R_equal -> 'a2
          -> 'a2) -> ('a1 t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ ->
          t' -> 'a1 -> (t' * 'a1) list -> __ -> t' OrderedType.coq_Compare ->
          __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __
          -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
        
        val coq_R_equal_rec :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> bool -> 'a1 coq_R_equal -> 'a2
          -> 'a2) -> ('a1 t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ ->
          t' -> 'a1 -> (t' * 'a1) list -> __ -> t' OrderedType.coq_Compare ->
          __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __
          -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
        
        val equal_rect :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
          -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> t' OrderedType.coq_Compare -> __ -> __ -> 'a2) ->
          ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1
          t -> 'a1 t -> 'a2
        
        val equal_rec :
          ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1
          t -> 'a1 t -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 ->
          (t' * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
          -> t' -> 'a1 -> (t' * 'a1) list -> __ -> t' -> 'a1 -> (t' * 'a1)
          list -> __ -> t' OrderedType.coq_Compare -> __ -> __ -> 'a2) ->
          ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1
          t -> 'a1 t -> 'a2
        
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
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
        coq_R_mem -> 'a2
      
      val coq_R_mem_rec :
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
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
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
        coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        option -> 'a1 coq_R_find -> 'a2
      
      val coq_R_find_rec :
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
        coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        option -> 'a1 coq_R_find -> 'a2
      
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
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2
      
      val coq_R_remove_rec :
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
        __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2
      
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
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
        triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
        tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split ->
        'a2
      
      val coq_R_split_rec :
        t' -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
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
  
  type key = t'
  
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
      type t = t'
     end
    
    module IsTO : 
     sig 
      
     end
    
    module OrderTac : 
     sig 
      
     end
    
    val eq_dec : t' -> t' -> bool
    
    val lt_dec : t' -> t' -> bool
    
    val eqb : t' -> t' -> bool
   end
  
  module O : 
   sig 
    module MO : 
     sig 
      module TO : 
       sig 
        type t = t'
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : t' -> t' -> bool
      
      val lt_dec : t' -> t' -> bool
      
      val eqb : t' -> t' -> bool
     end
   end
  
  module P : 
   sig 
    module F : 
     sig 
      val eqb : t' -> t' -> bool
      
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
    ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> t' -> 'a1 -> __ -> __
    -> 'a2) -> 'a1 t -> 'a2
  
  val map_induction_min :
    ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> t' -> 'a1 -> __ -> __
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

module NodeSet : 
 sig 
  module S : 
   sig 
    module X' : 
     sig 
      type t = t'
      
      val eq_dec : t' -> t' -> bool
      
      val compare : t' -> t' -> comparison
     end
    
    module MSet : 
     sig 
      module Raw : 
       sig 
        type elt = t'
        
        type tree =
        | Leaf
        | Node of Int.Z_as_Int.t * tree * t' * tree
        
        val empty : tree
        
        val is_empty : tree -> bool
        
        val mem : t' -> tree -> bool
        
        val min_elt : tree -> elt option
        
        val max_elt : tree -> elt option
        
        val choose : tree -> elt option
        
        val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
        
        val elements_aux : t' list -> tree -> t' list
        
        val elements : tree -> t' list
        
        val rev_elements_aux : t' list -> tree -> t' list
        
        val rev_elements : tree -> t' list
        
        val cardinal : tree -> int
        
        val maxdepth : tree -> int
        
        val mindepth : tree -> int
        
        val for_all : (elt -> bool) -> tree -> bool
        
        val exists_ : (elt -> bool) -> tree -> bool
        
        type enumeration =
        | End
        | More of elt * tree * enumeration
        
        val cons : tree -> enumeration -> enumeration
        
        val compare_more :
          t' -> (enumeration -> comparison) -> enumeration -> comparison
        
        val compare_cont :
          tree -> (enumeration -> comparison) -> enumeration -> comparison
        
        val compare_end : enumeration -> comparison
        
        val compare : tree -> tree -> comparison
        
        val equal : tree -> tree -> bool
        
        val subsetl : (tree -> bool) -> t' -> tree -> bool
        
        val subsetr : (tree -> bool) -> t' -> tree -> bool
        
        val subset : tree -> tree -> bool
        
        type t = tree
        
        val height : t -> Int.Z_as_Int.t
        
        val singleton : t' -> tree
        
        val create : t -> t' -> t -> tree
        
        val assert_false : t -> t' -> t -> tree
        
        val bal : t -> t' -> t -> tree
        
        val add : t' -> tree -> tree
        
        val join : tree -> elt -> t -> t
        
        val remove_min : tree -> elt -> t -> t * elt
        
        val merge : tree -> tree -> tree
        
        val remove : t' -> tree -> tree
        
        val concat : tree -> tree -> tree
        
        type triple = { t_left : t; t_in : bool; t_right : t }
        
        val t_left : triple -> t
        
        val t_in : triple -> bool
        
        val t_right : triple -> t
        
        val split : t' -> tree -> triple
        
        val inter : tree -> tree -> tree
        
        val diff : tree -> tree -> tree
        
        val union : tree -> tree -> tree
        
        val filter : (elt -> bool) -> tree -> tree
        
        val partition : (elt -> bool) -> t -> t * t
        
        val ltb_tree : t' -> tree -> bool
        
        val gtb_tree : t' -> tree -> bool
        
        val isok : tree -> bool
        
        module MX : 
         sig 
          module OrderTac : 
           sig 
            module OTF : 
             sig 
              type t = t'
              
              val compare : t' -> t' -> comparison
              
              val eq_dec : t' -> t' -> bool
             end
            
            module TO : 
             sig 
              type t = t'
              
              val compare : t' -> t' -> comparison
              
              val eq_dec : t' -> t' -> bool
             end
           end
          
          val eq_dec : t' -> t' -> bool
          
          val lt_dec : t' -> t' -> bool
          
          val eqb : t' -> t' -> bool
         end
        
        type coq_R_min_elt =
        | R_min_elt_0 of tree
        | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * t' * tree
        | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * elt option * coq_R_min_elt
        
        type coq_R_max_elt =
        | R_max_elt_0 of tree
        | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * t' * tree
        | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * elt option * coq_R_max_elt
        
        module L : 
         sig 
          module MO : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t = t'
                
                val compare : t' -> t' -> comparison
                
                val eq_dec : t' -> t' -> bool
               end
              
              module TO : 
               sig 
                type t = t'
                
                val compare : t' -> t' -> comparison
                
                val eq_dec : t' -> t' -> bool
               end
             end
            
            val eq_dec : t' -> t' -> bool
            
            val lt_dec : t' -> t' -> bool
            
            val eqb : t' -> t' -> bool
           end
         end
        
        val flatten_e : enumeration -> elt list
        
        type coq_R_bal =
        | R_bal_0 of t * t' * t
        | R_bal_1 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_2 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_3 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_4 of t * t' * t
        | R_bal_5 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_6 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_7 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree
        | R_bal_8 of t * t' * t
        
        type coq_R_remove_min =
        | R_remove_min_0 of tree * elt * t
        | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree * 
           t' * tree * (t * elt) * coq_R_remove_min * t * elt
        
        type coq_R_merge =
        | R_merge_0 of tree * tree
        | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
        | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * elt
        
        type coq_R_concat =
        | R_concat_0 of tree * tree
        | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
        | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * elt
        
        type coq_R_inter =
        | R_inter_0 of tree * tree
        | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
        | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
           * coq_R_inter * tree * coq_R_inter
        | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
           * coq_R_inter * tree * coq_R_inter
        
        type coq_R_diff =
        | R_diff_0 of tree * tree
        | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
        | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
           * coq_R_diff * tree * coq_R_diff
        | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
           * coq_R_diff * tree * coq_R_diff
        
        type coq_R_union =
        | R_union_0 of tree * tree
        | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
        | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
           * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
           * coq_R_union * tree * coq_R_union
       end
      
      module E : 
       sig 
        type t = t'
        
        val compare : t' -> t' -> comparison
        
        val eq_dec : t' -> t' -> bool
       end
      
      type elt = t'
      
      type t_ =
        Raw.t
        (* singleton inductive, whose constructor was Mkt *)
      
      val this : t_ -> Raw.t
      
      type t = t_
      
      val mem : elt -> t -> bool
      
      val add : elt -> t -> t
      
      val remove : elt -> t -> t
      
      val singleton : elt -> t
      
      val union : t -> t -> t
      
      val inter : t -> t -> t
      
      val diff : t -> t -> t
      
      val equal : t -> t -> bool
      
      val subset : t -> t -> bool
      
      val empty : t
      
      val is_empty : t -> bool
      
      val elements : t -> elt list
      
      val choose : t -> elt option
      
      val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
      
      val cardinal : t -> int
      
      val filter : (elt -> bool) -> t -> t
      
      val for_all : (elt -> bool) -> t -> bool
      
      val exists_ : (elt -> bool) -> t -> bool
      
      val partition : (elt -> bool) -> t -> t * t
      
      val eq_dec : t -> t -> bool
      
      val compare : t -> t -> comparison
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
     end
    
    type elt = t'
    
    type t = MSet.t
    
    val empty : t
    
    val is_empty : t -> bool
    
    val mem : elt -> t -> bool
    
    val add : elt -> t -> t
    
    val singleton : elt -> t
    
    val remove : elt -> t -> t
    
    val union : t -> t -> t
    
    val inter : t -> t -> t
    
    val diff : t -> t -> t
    
    val eq_dec : t -> t -> bool
    
    val equal : t -> t -> bool
    
    val subset : t -> t -> bool
    
    val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val for_all : (elt -> bool) -> t -> bool
    
    val exists_ : (elt -> bool) -> t -> bool
    
    val filter : (elt -> bool) -> t -> t
    
    val partition : (elt -> bool) -> t -> t * t
    
    val cardinal : t -> int
    
    val elements : t -> elt list
    
    val choose : t -> elt option
    
    module MF : 
     sig 
      val eqb : t' -> t' -> bool
     end
    
    val min_elt : t -> elt option
    
    val max_elt : t -> elt option
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    module E : 
     sig 
      type t = t'
      
      val compare : t' -> t' -> t' OrderedType.coq_Compare
      
      val eq_dec : t' -> t' -> bool
     end
   end
  
  module X' : 
   sig 
    type t = t'
    
    val eq_dec : t' -> t' -> bool
    
    val compare : t' -> t' -> comparison
   end
  
  module MSet : 
   sig 
    module Raw : 
     sig 
      type elt = t'
      
      type tree = S.MSet.Raw.tree =
      | Leaf
      | Node of Int.Z_as_Int.t * tree * t' * tree
      
      val empty : tree
      
      val is_empty : tree -> bool
      
      val mem : t' -> tree -> bool
      
      val min_elt : tree -> elt option
      
      val max_elt : tree -> elt option
      
      val choose : tree -> elt option
      
      val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
      
      val elements_aux : t' list -> tree -> t' list
      
      val elements : tree -> t' list
      
      val rev_elements_aux : t' list -> tree -> t' list
      
      val rev_elements : tree -> t' list
      
      val cardinal : tree -> int
      
      val maxdepth : tree -> int
      
      val mindepth : tree -> int
      
      val for_all : (elt -> bool) -> tree -> bool
      
      val exists_ : (elt -> bool) -> tree -> bool
      
      type enumeration =
      | End
      | More of elt * tree * enumeration
      
      val cons : tree -> enumeration -> enumeration
      
      val compare_more :
        t' -> (enumeration -> comparison) -> enumeration -> comparison
      
      val compare_cont :
        tree -> (enumeration -> comparison) -> enumeration -> comparison
      
      val compare_end : enumeration -> comparison
      
      val compare : tree -> tree -> comparison
      
      val equal : tree -> tree -> bool
      
      val subsetl : (tree -> bool) -> t' -> tree -> bool
      
      val subsetr : (tree -> bool) -> t' -> tree -> bool
      
      val subset : tree -> tree -> bool
      
      type t = tree
      
      val height : t -> Int.Z_as_Int.t
      
      val singleton : t' -> tree
      
      val create : t -> t' -> t -> tree
      
      val assert_false : t -> t' -> t -> tree
      
      val bal : t -> t' -> t -> tree
      
      val add : t' -> tree -> tree
      
      val join : tree -> elt -> t -> t
      
      val remove_min : tree -> elt -> t -> t * elt
      
      val merge : tree -> tree -> tree
      
      val remove : t' -> tree -> tree
      
      val concat : tree -> tree -> tree
      
      type triple = { t_left : t; t_in : bool; t_right : t }
      
      val t_left : triple -> t
      
      val t_in : triple -> bool
      
      val t_right : triple -> t
      
      val split : t' -> tree -> triple
      
      val inter : tree -> tree -> tree
      
      val diff : tree -> tree -> tree
      
      val union : tree -> tree -> tree
      
      val filter : (elt -> bool) -> tree -> tree
      
      val partition : (elt -> bool) -> t -> t * t
      
      val ltb_tree : t' -> tree -> bool
      
      val gtb_tree : t' -> tree -> bool
      
      val isok : tree -> bool
      
      module MX : 
       sig 
        module OrderTac : 
         sig 
          module OTF : 
           sig 
            type t = t'
            
            val compare : t' -> t' -> comparison
            
            val eq_dec : t' -> t' -> bool
           end
          
          module TO : 
           sig 
            type t = t'
            
            val compare : t' -> t' -> comparison
            
            val eq_dec : t' -> t' -> bool
           end
         end
        
        val eq_dec : t' -> t' -> bool
        
        val lt_dec : t' -> t' -> bool
        
        val eqb : t' -> t' -> bool
       end
      
      type coq_R_min_elt =
      | R_min_elt_0 of tree
      | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * t' * tree
      | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * elt option * coq_R_min_elt
      
      type coq_R_max_elt =
      | R_max_elt_0 of tree
      | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * t' * tree
      | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * elt option * coq_R_max_elt
      
      module L : 
       sig 
        module MO : 
         sig 
          module OrderTac : 
           sig 
            module OTF : 
             sig 
              type t = t'
              
              val compare : t' -> t' -> comparison
              
              val eq_dec : t' -> t' -> bool
             end
            
            module TO : 
             sig 
              type t = t'
              
              val compare : t' -> t' -> comparison
              
              val eq_dec : t' -> t' -> bool
             end
           end
          
          val eq_dec : t' -> t' -> bool
          
          val lt_dec : t' -> t' -> bool
          
          val eqb : t' -> t' -> bool
         end
       end
      
      val flatten_e : enumeration -> elt list
      
      type coq_R_bal =
      | R_bal_0 of t * t' * t
      | R_bal_1 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_2 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_3 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_4 of t * t' * t
      | R_bal_5 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_6 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_7 of t * t' * t * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree
      | R_bal_8 of t * t' * t
      
      type coq_R_remove_min =
      | R_remove_min_0 of tree * elt * t
      | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree * t' * 
         tree * (t * elt) * coq_R_remove_min * t * elt
      
      type coq_R_merge =
      | R_merge_0 of tree * tree
      | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
      | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * elt
      
      type coq_R_concat =
      | R_concat_0 of tree * tree
      | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
      | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * elt
      
      type coq_R_inter =
      | R_inter_0 of tree * tree
      | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
      | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
         * coq_R_inter * tree * coq_R_inter
      | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
         * coq_R_inter * tree * coq_R_inter
      
      type coq_R_diff =
      | R_diff_0 of tree * tree
      | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
      | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
         * coq_R_diff * tree * coq_R_diff
      | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
         * coq_R_diff * tree * coq_R_diff
      
      type coq_R_union =
      | R_union_0 of tree * tree
      | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
      | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * t' * tree
         * Int.Z_as_Int.t * tree * t' * tree * t * bool * t * tree
         * coq_R_union * tree * coq_R_union
     end
    
    module E : 
     sig 
      type t = t'
      
      val compare : t' -> t' -> comparison
      
      val eq_dec : t' -> t' -> bool
     end
    
    type elt = t'
    
    type t_ =
      Raw.t
      (* singleton inductive, whose constructor was Mkt *)
    
    val this : t_ -> Raw.t
    
    type t = t_
    
    val mem : elt -> t -> bool
    
    val add : elt -> t -> t
    
    val remove : elt -> t -> t
    
    val singleton : elt -> t
    
    val union : t -> t -> t
    
    val inter : t -> t -> t
    
    val diff : t -> t -> t
    
    val equal : t -> t -> bool
    
    val subset : t -> t -> bool
    
    val empty : t
    
    val is_empty : t -> bool
    
    val elements : t -> elt list
    
    val choose : t -> elt option
    
    val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val cardinal : t -> int
    
    val filter : (elt -> bool) -> t -> t
    
    val for_all : (elt -> bool) -> t -> bool
    
    val exists_ : (elt -> bool) -> t -> bool
    
    val partition : (elt -> bool) -> t -> t * t
    
    val eq_dec : t -> t -> bool
    
    val compare : t -> t -> comparison
    
    val min_elt : t -> elt option
    
    val max_elt : t -> elt option
   end
  
  type elt = t'
  
  type t = MSet.t
  
  val empty : t
  
  val is_empty : t -> bool
  
  val mem : elt -> t -> bool
  
  val add : elt -> t -> t
  
  val singleton : elt -> t
  
  val remove : elt -> t -> t
  
  val union : t -> t -> t
  
  val inter : t -> t -> t
  
  val diff : t -> t -> t
  
  val eq_dec : t -> t -> bool
  
  val equal : t -> t -> bool
  
  val subset : t -> t -> bool
  
  val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
  
  val for_all : (elt -> bool) -> t -> bool
  
  val exists_ : (elt -> bool) -> t -> bool
  
  val filter : (elt -> bool) -> t -> t
  
  val partition : (elt -> bool) -> t -> t * t
  
  val cardinal : t -> int
  
  val elements : t -> elt list
  
  val choose : t -> elt option
  
  module MF : 
   sig 
    val eqb : t' -> t' -> bool
   end
  
  val min_elt : t -> elt option
  
  val max_elt : t -> elt option
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  module E : 
   sig 
    type t = t'
    
    val compare : t' -> t' -> t' OrderedType.coq_Compare
    
    val eq_dec : t' -> t' -> bool
   end
  
  module SF : 
   sig 
    val eqb : t' -> t' -> bool
   end
  
  val choose_only : t -> elt option
  
  val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
  
  val cond_eq_rect :
    (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
  
  val cond_eq_rec :
    (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
 end

type t = { args : vid_t list; cmds : cmd NodeMap.t;
           succ : NodeSet.t NodeMap.t; pred : NodeSet.t NodeMap.t;
           nodes : NodeSet.t }

val t_rect :
  (vid_t list -> cmd NodeMap.t -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t
  -> NodeSet.t -> 'a1) -> t -> 'a1

val t_rec :
  (vid_t list -> cmd NodeMap.t -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t
  -> NodeSet.t -> 'a1) -> t -> 'a1

val args : t -> vid_t list

val cmds : t -> cmd NodeMap.t

val succ : t -> NodeSet.t NodeMap.t

val pred : t -> NodeSet.t NodeMap.t

val nodes : t -> NodeSet.t

val get_cmd : t -> IntraNode.t -> cmd option

val is_call_node : t -> IntraNode.t -> bool

val returnof : t -> IntraNode.t -> IntraNode.t option

val callof : t -> IntraNode.t -> IntraNode.t option

val is_return_node : t -> IntraNode.t -> bool

val remove_node_pred :
  NodeMap.key -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t

val remove_node : IntraNode.t -> t -> t

