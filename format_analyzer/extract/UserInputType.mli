open AUGER_Monad
open DLat
open DMap
open DPow
open Datatypes
open DomCon
open InterCfg
open InterNode
open IntraNode
open Syn
open TStr

type __ = Obj.t

type update_mode =
| Weak
| Strong

val update_mode_rect : 'a1 -> 'a1 -> update_mode -> 'a1

val update_mode_rec : 'a1 -> 'a1 -> update_mode -> 'a1

type phase_t =
| PrePhase
| MainPhase
| ValiPhase

val phase_t_rect : 'a1 -> 'a1 -> 'a1 -> phase_t -> 'a1

val phase_t_rec : 'a1 -> 'a1 -> 'a1 -> phase_t -> 'a1

type loc_type =
| GVarLoc
| LVarLoc of pid_t
| OtherLoc

val loc_type_rect : 'a1 -> (pid_t -> 'a1) -> 'a1 -> loc_type -> 'a1

val loc_type_rec : 'a1 -> (pid_t -> 'a1) -> 'a1 -> loc_type -> 'a1

type ('monad_f, 'loc_t, 'mem_t, 'val_t, 'mem_powa_t) coq_MemBasic = { 
mem_find : ('loc_t -> 'mem_t -> 'monad_f);
mem_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f);
mem_pre_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f);
mem_main_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f);
mem_weak_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f);
mem_pre_weak_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f);
mem_main_weak_add : ('mem_powa_t -> 'loc_t -> 'val_t -> 'mem_t -> 'monad_f) }

val coq_MemBasic_rect :
  (('a2 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> 'a6) -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a6

val coq_MemBasic_rec :
  (('a2 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1) -> ('a5 -> 'a2 ->
  'a4 -> 'a3 -> 'a1) -> 'a6) -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a6

val mem_find : ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a2 -> 'a3 -> 'a1

val mem_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

val mem_pre_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

val mem_main_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

val mem_weak_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

val mem_pre_weak_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

val mem_main_weak_add :
  ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 -> 'a3 -> 'a1

module type INPUT = 
 sig 
  module Loc : 
   KEY
  
  module Val : 
   LAT
  
  module Dump : 
   Global.DUMP
  
  val coq_Loc_g : DomCon.Loc.t -> Loc.t
  
  module PowLoc : 
   sig 
    module A : 
     sig 
      type t = Loc.t
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      val eq_dec : t -> t -> bool
     end
    
    module SS : 
     sig 
      module S : 
       sig 
        module X' : 
         sig 
          type t = Loc.t
          
          val eq_dec : t -> t -> bool
          
          val compare : Loc.t -> Loc.t -> comparison
         end
        
        module MSet : 
         sig 
          module Raw : 
           sig 
            type elt = Loc.t
            
            type tree =
            | Leaf
            | Node of Int.Z_as_Int.t * tree * Loc.t * tree
            
            val empty : tree
            
            val is_empty : tree -> bool
            
            val mem : Loc.t -> tree -> bool
            
            val min_elt : tree -> elt option
            
            val max_elt : tree -> elt option
            
            val choose : tree -> elt option
            
            val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
            
            val elements_aux : Loc.t list -> tree -> Loc.t list
            
            val elements : tree -> Loc.t list
            
            val rev_elements_aux : Loc.t list -> tree -> Loc.t list
            
            val rev_elements : tree -> Loc.t list
            
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
              Loc.t -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_cont :
              tree -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_end : enumeration -> comparison
            
            val compare : tree -> tree -> comparison
            
            val equal : tree -> tree -> bool
            
            val subsetl : (tree -> bool) -> Loc.t -> tree -> bool
            
            val subsetr : (tree -> bool) -> Loc.t -> tree -> bool
            
            val subset : tree -> tree -> bool
            
            type t = tree
            
            val height : t -> Int.Z_as_Int.t
            
            val singleton : Loc.t -> tree
            
            val create : t -> Loc.t -> t -> tree
            
            val assert_false : t -> Loc.t -> t -> tree
            
            val bal : t -> Loc.t -> t -> tree
            
            val add : Loc.t -> tree -> tree
            
            val join : tree -> elt -> t -> t
            
            val remove_min : tree -> elt -> t -> t * elt
            
            val merge : tree -> tree -> tree
            
            val remove : Loc.t -> tree -> tree
            
            val concat : tree -> tree -> tree
            
            type triple = { t_left : t; t_in : bool; t_right : t }
            
            val t_left : triple -> t
            
            val t_in : triple -> bool
            
            val t_right : triple -> t
            
            val split : Loc.t -> tree -> triple
            
            val inter : tree -> tree -> tree
            
            val diff : tree -> tree -> tree
            
            val union : tree -> tree -> tree
            
            val filter : (elt -> bool) -> tree -> tree
            
            val partition : (elt -> bool) -> t -> t * t
            
            val ltb_tree : Loc.t -> tree -> bool
            
            val gtb_tree : Loc.t -> tree -> bool
            
            val isok : tree -> bool
            
            module MX : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = Loc.t
                  
                  val compare : Loc.t -> Loc.t -> comparison
                  
                  val eq_dec : Loc.t -> Loc.t -> bool
                 end
                
                module TO : 
                 sig 
                  type t = Loc.t
                  
                  val compare : Loc.t -> Loc.t -> comparison
                  
                  val eq_dec : Loc.t -> Loc.t -> bool
                 end
               end
              
              val eq_dec : Loc.t -> Loc.t -> bool
              
              val lt_dec : Loc.t -> Loc.t -> bool
              
              val eqb : Loc.t -> Loc.t -> bool
             end
            
            type coq_R_min_elt =
            | R_min_elt_0 of tree
            | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
               * Int.Z_as_Int.t * tree * Loc.t * tree * elt option
               * coq_R_min_elt
            
            type coq_R_max_elt =
            | R_max_elt_0 of tree
            | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
               * Int.Z_as_Int.t * tree * Loc.t * tree * elt option
               * coq_R_max_elt
            
            module L : 
             sig 
              module MO : 
               sig 
                module OrderTac : 
                 sig 
                  module OTF : 
                   sig 
                    type t = Loc.t
                    
                    val compare : Loc.t -> Loc.t -> comparison
                    
                    val eq_dec : Loc.t -> Loc.t -> bool
                   end
                  
                  module TO : 
                   sig 
                    type t = Loc.t
                    
                    val compare : Loc.t -> Loc.t -> comparison
                    
                    val eq_dec : Loc.t -> Loc.t -> bool
                   end
                 end
                
                val eq_dec : Loc.t -> Loc.t -> bool
                
                val lt_dec : Loc.t -> Loc.t -> bool
                
                val eqb : Loc.t -> Loc.t -> bool
               end
             end
            
            val flatten_e : enumeration -> elt list
            
            type coq_R_bal =
            | R_bal_0 of t * Loc.t * t
            | R_bal_1 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_2 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_3 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_4 of t * Loc.t * t
            | R_bal_5 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_6 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_7 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_bal_8 of t * Loc.t * t
            
            type coq_R_remove_min =
            | R_remove_min_0 of tree * elt * t
            | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
               * Loc.t * tree * (t * elt) * coq_R_remove_min * t * elt
            
            type coq_R_merge =
            | R_merge_0 of tree * tree
            | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * elt
            
            type coq_R_concat =
            | R_concat_0 of tree * tree
            | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t
               * tree
            | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t
               * tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * elt
            
            type coq_R_inter =
            | R_inter_0 of tree * tree
            | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
               t * tree * coq_R_inter * tree * coq_R_inter
            | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
               t * tree * coq_R_inter * tree * coq_R_inter
            
            type coq_R_diff =
            | R_diff_0 of tree * tree
            | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
               t * tree * coq_R_diff * tree * coq_R_diff
            | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
               t * tree * coq_R_diff * tree * coq_R_diff
            
            type coq_R_union =
            | R_union_0 of tree * tree
            | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
            | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
               tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
               t * tree * coq_R_union * tree * coq_R_union
           end
          
          module E : 
           sig 
            type t = Loc.t
            
            val compare : Loc.t -> Loc.t -> comparison
            
            val eq_dec : Loc.t -> Loc.t -> bool
           end
          
          type elt = Loc.t
          
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
        
        type elt = Loc.t
        
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
          val eqb : Loc.t -> Loc.t -> bool
         end
        
        val min_elt : t -> elt option
        
        val max_elt : t -> elt option
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        module E : 
         sig 
          type t = Loc.t
          
          val compare : t -> t -> t OrderedType.coq_Compare
          
          val eq_dec : t -> t -> bool
         end
       end
      
      module X' : 
       sig 
        type t = Loc.t
        
        val eq_dec : t -> t -> bool
        
        val compare : Loc.t -> Loc.t -> comparison
       end
      
      module MSet : 
       sig 
        module Raw : 
         sig 
          type elt = X'.t
          
          type tree = S.MSet.Raw.tree =
          | Leaf
          | Node of Int.Z_as_Int.t * tree * Loc.t * tree
          
          val empty : tree
          
          val is_empty : tree -> bool
          
          val mem : Loc.t -> tree -> bool
          
          val min_elt : tree -> elt option
          
          val max_elt : tree -> elt option
          
          val choose : tree -> elt option
          
          val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
          
          val elements_aux : Loc.t list -> tree -> Loc.t list
          
          val elements : tree -> Loc.t list
          
          val rev_elements_aux : Loc.t list -> tree -> Loc.t list
          
          val rev_elements : tree -> Loc.t list
          
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
            Loc.t -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_cont :
            tree -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_end : enumeration -> comparison
          
          val compare : tree -> tree -> comparison
          
          val equal : tree -> tree -> bool
          
          val subsetl : (tree -> bool) -> Loc.t -> tree -> bool
          
          val subsetr : (tree -> bool) -> Loc.t -> tree -> bool
          
          val subset : tree -> tree -> bool
          
          type t = tree
          
          val height : t -> Int.Z_as_Int.t
          
          val singleton : Loc.t -> tree
          
          val create : t -> Loc.t -> t -> tree
          
          val assert_false : t -> Loc.t -> t -> tree
          
          val bal : t -> Loc.t -> t -> tree
          
          val add : Loc.t -> tree -> tree
          
          val join : tree -> elt -> t -> t
          
          val remove_min : tree -> elt -> t -> t * elt
          
          val merge : tree -> tree -> tree
          
          val remove : Loc.t -> tree -> tree
          
          val concat : tree -> tree -> tree
          
          type triple = { t_left : t; t_in : bool; t_right : t }
          
          val t_left : triple -> t
          
          val t_in : triple -> bool
          
          val t_right : triple -> t
          
          val split : Loc.t -> tree -> triple
          
          val inter : tree -> tree -> tree
          
          val diff : tree -> tree -> tree
          
          val union : tree -> tree -> tree
          
          val filter : (elt -> bool) -> tree -> tree
          
          val partition : (elt -> bool) -> t -> t * t
          
          val ltb_tree : Loc.t -> tree -> bool
          
          val gtb_tree : Loc.t -> tree -> bool
          
          val isok : tree -> bool
          
          module MX : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t = X'.t
                
                val compare : Loc.t -> Loc.t -> comparison
                
                val eq_dec : Loc.t -> Loc.t -> bool
               end
              
              module TO : 
               sig 
                type t = Loc.t
                
                val compare : Loc.t -> Loc.t -> comparison
                
                val eq_dec : Loc.t -> Loc.t -> bool
               end
             end
            
            val eq_dec : Loc.t -> Loc.t -> bool
            
            val lt_dec : Loc.t -> Loc.t -> bool
            
            val eqb : Loc.t -> Loc.t -> bool
           end
          
          type coq_R_min_elt =
          | R_min_elt_0 of tree
          | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
             * Int.Z_as_Int.t * tree * Loc.t * tree * elt option
             * coq_R_min_elt
          
          type coq_R_max_elt =
          | R_max_elt_0 of tree
          | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * Loc.t * tree
             * Int.Z_as_Int.t * tree * Loc.t * tree * elt option
             * coq_R_max_elt
          
          module L : 
           sig 
            module MO : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = X'.t
                  
                  val compare : Loc.t -> Loc.t -> comparison
                  
                  val eq_dec : Loc.t -> Loc.t -> bool
                 end
                
                module TO : 
                 sig 
                  type t = Loc.t
                  
                  val compare : Loc.t -> Loc.t -> comparison
                  
                  val eq_dec : Loc.t -> Loc.t -> bool
                 end
               end
              
              val eq_dec : Loc.t -> Loc.t -> bool
              
              val lt_dec : Loc.t -> Loc.t -> bool
              
              val eqb : Loc.t -> Loc.t -> bool
             end
           end
          
          val flatten_e : enumeration -> elt list
          
          type coq_R_bal =
          | R_bal_0 of t * Loc.t * t
          | R_bal_1 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_2 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_3 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_4 of t * Loc.t * t
          | R_bal_5 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_6 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_7 of t * Loc.t * t * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_bal_8 of t * Loc.t * t
          
          type coq_R_remove_min =
          | R_remove_min_0 of tree * elt * t
          | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree * 
             Loc.t * tree * (t * elt) * coq_R_remove_min * t * elt
          
          type coq_R_merge =
          | R_merge_0 of tree * tree
          | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * elt
          
          type coq_R_concat =
          | R_concat_0 of tree * tree
          | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * elt
          
          type coq_R_inter =
          | R_inter_0 of tree * tree
          | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          
          type coq_R_diff =
          | R_diff_0 of tree * tree
          | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          
          type coq_R_union =
          | R_union_0 of tree * tree
          | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * tree
          | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * Loc.t * 
             tree * Int.Z_as_Int.t * tree * Loc.t * tree * t * bool * 
             t * tree * coq_R_union * tree * coq_R_union
         end
        
        module E : 
         sig 
          type t = Loc.t
          
          val compare : Loc.t -> Loc.t -> comparison
          
          val eq_dec : Loc.t -> Loc.t -> bool
         end
        
        type elt = X'.t
        
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
      
      type elt = Loc.t
      
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
        val eqb : Loc.t -> Loc.t -> bool
       end
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      module E : 
       sig 
        type t = Loc.t
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        val eq_dec : t -> t -> bool
       end
      
      module SF : 
       sig 
        val eqb : Loc.t -> Loc.t -> bool
       end
      
      val choose_only : t -> elt option
      
      val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
      
      val cond_eq_rect :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
      
      val cond_eq_rec :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
     end
    
    type elt = Loc.t
    
    type t = SS.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val empty : t
    
    val is_empty : SS.t -> bool
    
    val add : Loc.t -> t -> t
    
    val singleton : Loc.t -> t
    
    val mem : SS.elt -> SS.t -> bool
    
    val remove : Loc.t -> t -> t
    
    val union : SS.t -> SS.t -> SS.t
    
    val union_small_big : SS.t -> SS.t -> SS.t
    
    val intersect : SS.t -> SS.t -> SS.t
    
    val diff : SS.t -> SS.t -> SS.t
    
    val subset : SS.t -> SS.t -> bool
    
    val filter : (Loc.t -> bool) -> t -> t
    
    val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1
    
    val iter : (Loc.t -> unit) -> t -> unit
    
    val elements : SS.t -> SS.elt list
    
    val cardinal : SS.t -> int
    
    val choose : SS.t -> SS.elt option
    
    val choose_only : SS.t -> SS.elt option
    
    val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool
    
    val coq_ILat : t coq_TCLat
    
    val coq_ISet : (Loc.t, t) coq_TCSet
   end
  
  val classify_loc : Loc.t -> loc_type
  
  module Acc : 
   sig 
    type t = PowLoc.t * PowLoc.t
    
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
    
    type access_mode =
    | DEF
    | USE
    | ALL
    
    val access_mode_rect : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1
    
    val access_mode_rec : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1
    
    val defof : t -> PowLoc.t
    
    val useof : t -> PowLoc.t
    
    val accessof : t -> PowLoc.t
    
    val empty : t
    
    val add : access_mode -> PowLoc.elt -> t -> t
    
    val add_set : access_mode -> PowLoc.t -> t -> t
    
    val singleton : access_mode -> PowLoc.elt -> t
    
    val from_set : access_mode -> PowLoc.t -> t
    
    val mem : PowLoc.elt -> t -> bool
    
    val remove : PowLoc.elt -> t -> t
    
    val remove_set : PowLoc.t -> t -> t
    
    val add_list : access_mode -> PowLoc.elt list -> t -> t
    
    val union : t -> t -> t
    
    val restrict : PowLoc.t -> t -> t
    
    val filter_out : PowLoc.t -> t -> t
   end
  
  module Mem : 
   sig 
    module A : 
     sig 
      type t = Loc.t
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      val eq_dec : t -> t -> bool
     end
    
    module B : 
     sig 
      type t = Val.t
      
      val le_dec : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join : t -> t -> t
      
      val meet : t -> t -> t
      
      val widen : t -> t -> t
      
      val narrow : t -> t -> t
     end
    
    module Mem : 
     sig 
      module A : 
       sig 
        type t = Loc.t
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        val eq_dec : t -> t -> bool
       end
      
      module B : 
       sig 
        type t = Val.t
        
        val le_dec : t -> t -> bool
        
        val eq_dec : t -> t -> bool
        
        val bot : t
        
        val join : t -> t -> t
        
        val meet : t -> t -> t
        
        val widen : t -> t -> t
        
        val narrow : t -> t -> t
       end
      
      module M : 
       sig 
        module E : 
         sig 
          type t = Loc.t
          
          val compare : t -> t -> t OrderedType.coq_Compare
          
          val eq_dec : t -> t -> bool
         end
        
        module Raw : 
         sig 
          type key = Loc.t
          
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
          
          val mem : Loc.t -> 'a1 tree -> bool
          
          val find : Loc.t -> 'a1 tree -> 'a1 option
          
          val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val remove_min :
            'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
          
          val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
          
          val remove : Loc.t -> 'a1 tree -> 'a1 tree
          
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
          
          val split : Loc.t -> 'a1 tree -> 'a1 triple
          
          val concat : 'a1 tree -> 'a1 tree -> 'a1 tree
          
          val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list
          
          val elements : 'a1 tree -> (key * 'a1) list
          
          val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2
          
          type 'elt enumeration =
          | End
          | More of key * 'elt * 'elt tree * 'elt enumeration
          
          val enumeration_rect :
            'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2)
            -> 'a1 enumeration -> 'a2
          
          val enumeration_rec :
            'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2)
            -> 'a1 enumeration -> 'a2
          
          val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration
          
          val equal_more :
            ('a1 -> 'a1 -> bool) -> Loc.t -> 'a1 -> ('a1 enumeration -> bool)
            -> 'a1 enumeration -> bool
          
          val equal_cont :
            ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) ->
            'a1 enumeration -> bool
          
          val equal_end : 'a1 enumeration -> bool
          
          val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool
          
          val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree
          
          val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree
          
          val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree
          
          val map2_opt :
            (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
            tree) -> ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3
            tree
          
          val map2 :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree
            -> 'a3 tree
          
          module Proofs : 
           sig 
            module MX : 
             sig 
              module TO : 
               sig 
                type t = Loc.t
               end
              
              module IsTO : 
               sig 
                
               end
              
              module OrderTac : 
               sig 
                
               end
              
              val eq_dec : Loc.t -> Loc.t -> bool
              
              val lt_dec : Loc.t -> Loc.t -> bool
              
              val eqb : Loc.t -> Loc.t -> bool
             end
            
            module PX : 
             sig 
              module MO : 
               sig 
                module TO : 
                 sig 
                  type t = Loc.t
                 end
                
                module IsTO : 
                 sig 
                  
                 end
                
                module OrderTac : 
                 sig 
                  
                 end
                
                val eq_dec : Loc.t -> Loc.t -> bool
                
                val lt_dec : Loc.t -> Loc.t -> bool
                
                val eqb : Loc.t -> Loc.t -> bool
               end
             end
            
            module L : 
             sig 
              module MX : 
               sig 
                module TO : 
                 sig 
                  type t = Loc.t
                 end
                
                module IsTO : 
                 sig 
                  
                 end
                
                module OrderTac : 
                 sig 
                  
                 end
                
                val eq_dec : Loc.t -> Loc.t -> bool
                
                val lt_dec : Loc.t -> Loc.t -> bool
                
                val eqb : Loc.t -> Loc.t -> bool
               end
              
              module PX : 
               sig 
                module MO : 
                 sig 
                  module TO : 
                   sig 
                    type t = Loc.t
                   end
                  
                  module IsTO : 
                   sig 
                    
                   end
                  
                  module OrderTac : 
                   sig 
                    
                   end
                  
                  val eq_dec : Loc.t -> Loc.t -> bool
                  
                  val lt_dec : Loc.t -> Loc.t -> bool
                  
                  val eqb : Loc.t -> Loc.t -> bool
                 end
               end
              
              type key = Loc.t
              
              type 'elt t = (Loc.t * 'elt) list
              
              val empty : 'a1 t
              
              val is_empty : 'a1 t -> bool
              
              val mem : key -> 'a1 t -> bool
              
              type 'elt coq_R_mem =
              | R_mem_0 of 'elt t
              | R_mem_1 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_mem_2 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_mem_3 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list * 
                 bool * 'elt coq_R_mem
              
              val coq_R_mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
                bool -> 'a1 coq_R_mem -> 'a2
              
              val coq_R_mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
                bool -> 'a1 coq_R_mem -> 'a2
              
              val mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
              
              val find : key -> 'a1 t -> 'a1 option
              
              type 'elt coq_R_find =
              | R_find_0 of 'elt t
              | R_find_1 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_find_2 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_find_3 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
                 * 'elt option * 'elt coq_R_find
              
              val coq_R_find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t
                -> 'a1 option -> 'a1 coq_R_find -> 'a2
              
              val coq_R_find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t
                -> 'a1 option -> 'a1 coq_R_find -> 'a2
              
              val find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_find_correct :
                key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
              
              val add : key -> 'a1 -> 'a1 t -> 'a1 t
              
              type 'elt coq_R_add =
              | R_add_0 of 'elt t
              | R_add_1 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_add_2 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_add_3 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
                 * 'elt t * 'elt coq_R_add
              
              val coq_R_add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1
                -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 t -> 'a1 coq_R_add -> 'a2
              
              val coq_R_add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1
                -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 t -> 'a1 coq_R_add -> 'a2
              
              val add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1
                -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1
                -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_add_correct :
                key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
              
              val remove : key -> 'a1 t -> 'a1 t
              
              type 'elt coq_R_remove =
              | R_remove_0 of 'elt t
              | R_remove_1 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_remove_2 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
              | R_remove_3 of 'elt t * Loc.t * 'elt * (Loc.t * 'elt) list
                 * 'elt t * 'elt coq_R_remove
              
              val coq_R_remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 t -> 'a1 coq_R_remove -> 'a2
              
              val coq_R_remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 t -> 'a1 coq_R_remove -> 'a2
              
              val remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ -> 'a2)
                -> ('a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __
                -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_remove_correct :
                key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
              
              val elements : 'a1 t -> 'a1 t
              
              val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
              
              type ('elt, 'a) coq_R_fold =
              | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
              | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * 
                 Loc.t * 'elt * (Loc.t * 'elt) list * 'a
                 * ('elt, 'a) coq_R_fold
              
              val coq_R_fold_rect :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> Loc.t ->
                'a1 -> (Loc.t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold
                -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3
                -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
              
              val coq_R_fold_rec :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> Loc.t ->
                'a1 -> (Loc.t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold
                -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3
                -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
              
              val fold_rect :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> Loc.t ->
                'a1 -> (Loc.t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1
                -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
              
              val fold_rec :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> Loc.t ->
                'a1 -> (Loc.t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1
                -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
              
              val coq_R_fold_correct :
                (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1,
                'a2) coq_R_fold
              
              val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
              
              type 'elt coq_R_equal =
              | R_equal_0 of 'elt t * 'elt t
              | R_equal_1 of 'elt t * 'elt t * Loc.t * 'elt
                 * (Loc.t * 'elt) list * Loc.t * 'elt * (Loc.t * 'elt) list
                 * bool * 'elt coq_R_equal
              | R_equal_2 of 'elt t * 'elt t * Loc.t * 'elt
                 * (Loc.t * 'elt) list * Loc.t * 'elt * (Loc.t * 'elt) list
                 * Loc.t OrderedType.coq_Compare
              | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
              
              val coq_R_equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list ->
                __ -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ ->
                bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> Loc.t OrderedType.coq_Compare ->
                __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
                -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1
                coq_R_equal -> 'a2
              
              val coq_R_equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list ->
                __ -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ ->
                bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
                Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> Loc.t OrderedType.coq_Compare ->
                __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
                -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1
                coq_R_equal -> 'a2
              
              val equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list ->
                __ -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ ->
                'a2 -> 'a2) -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> Loc.t -> 'a1 -> (Loc.t * 'a1)
                list -> __ -> Loc.t OrderedType.coq_Compare -> __ -> __ ->
                'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __
                -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
              
              val equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 -> (Loc.t * 'a1) list ->
                __ -> Loc.t -> 'a1 -> (Loc.t * 'a1) list -> __ -> __ -> __ ->
                'a2 -> 'a2) -> ('a1 t -> 'a1 t -> Loc.t -> 'a1 ->
                (Loc.t * 'a1) list -> __ -> Loc.t -> 'a1 -> (Loc.t * 'a1)
                list -> __ -> Loc.t OrderedType.coq_Compare -> __ -> __ ->
                'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __
                -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
              
              val coq_R_equal_correct :
                ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1
                coq_R_equal
              
              val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
              
              val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t
              
              val option_cons :
                key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list
              
              val map2_l :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t
              
              val map2_r :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t
              
              val map2 :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
                'a3 t
              
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
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem ->
              'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2
            
            val coq_R_mem_rec :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem ->
              'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2
            
            type 'elt coq_R_find =
            | R_find_0 of 'elt tree
            | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
            | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t
            | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
            
            val coq_R_find_rect :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
              coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1
              coq_R_find -> 'a2
            
            val coq_R_find_rec :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
              coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1
              coq_R_find -> 'a2
            
            type 'elt coq_R_bal =
            | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
            | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
            | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * 
               key * 'elt * 'elt tree * Int.Z_as_Int.t
            | R_bal_8 of 'elt tree * key * 'elt * 'elt tree
            
            val coq_R_bal_rect :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2)
              -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
              'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
              tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
              key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
              -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
              -> 'a1 coq_R_bal -> 'a2
            
            val coq_R_bal_rec :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2)
              -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
              'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
              tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
              key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
              -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
              -> 'a1 coq_R_bal -> 'a2
            
            type 'elt coq_R_add =
            | R_add_0 of 'elt tree
            | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
            | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t
            | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
            
            val coq_R_add_rect :
              key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_add -> 'a2
            
            val coq_R_add_rec :
              key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
              -> 'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_add -> 'a2
            
            type 'elt coq_R_remove_min =
            | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
            | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree
               * 'elt tree * key * 'elt * 'elt tree * Int.Z_as_Int.t
               * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min
               * 'elt tree * (key * 'elt)
            
            val coq_R_remove_min_rect :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
              coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ ->
              'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1
              tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2
            
            val coq_R_remove_min_rec :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
              coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ ->
              'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1
              tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2
            
            type 'elt coq_R_merge =
            | R_merge_0 of 'elt tree * 'elt tree
            | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t
            | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
               * key * 'elt
            
            val coq_R_merge_rect :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1)
              -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree ->
              'a1 tree -> 'a1 coq_R_merge -> 'a2
            
            val coq_R_merge_rec :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1)
              -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree ->
              'a1 tree -> 'a1 coq_R_merge -> 'a2
            
            type 'elt coq_R_remove =
            | R_remove_0 of 'elt tree
            | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
            | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t
            | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
            
            val coq_R_remove_rect :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2
            
            val coq_R_remove_rec :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2
            
            type 'elt coq_R_concat =
            | R_concat_0 of 'elt tree * 'elt tree
            | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t
            | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
            
            val coq_R_concat_rect :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1)
              -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_concat -> 'a2
            
            val coq_R_concat_rec :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
              __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1
              -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1)
              -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_concat -> 'a2
            
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
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option
              -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2
            
            val coq_R_split_rec :
              Loc.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
              'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option
              -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2
            
            type ('elt, 'elt') coq_R_map_option =
            | R_map_option_0 of 'elt tree
            | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt
               * 'elt tree * Int.Z_as_Int.t * 'elt' * 'elt' tree
               * ('elt, 'elt') coq_R_map_option * 'elt' tree
               * ('elt, 'elt') coq_R_map_option
            | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt
               * 'elt tree * Int.Z_as_Int.t * 'elt' tree
               * ('elt, 'elt') coq_R_map_option * 'elt' tree
               * ('elt, 'elt') coq_R_map_option
            
            val coq_R_map_option_rect :
              (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
              __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
              'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3)
              -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
              coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
              coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
              ('a1, 'a2) coq_R_map_option -> 'a3
            
            val coq_R_map_option_rec :
              (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
              __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
              'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3)
              -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
              coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
              coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
              ('a1, 'a2) coq_R_map_option -> 'a3
            
            type ('elt, 'elt', 'elt'') coq_R_map2_opt =
            | R_map2_opt_0 of 'elt tree * 'elt' tree
            | R_map2_opt_1 of 'elt tree * 'elt' tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t
            | R_map2_opt_2 of 'elt tree * 'elt' tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 
               'elt' * 'elt' tree * Int.Z_as_Int.t * 'elt' tree
               * 'elt' option * 'elt' tree * 'elt'' * 'elt'' tree
               * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
               * ('elt, 'elt', 'elt'') coq_R_map2_opt
            | R_map2_opt_3 of 'elt tree * 'elt' tree * 'elt tree * key * 
               'elt * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 
               'elt' * 'elt' tree * Int.Z_as_Int.t * 'elt' tree
               * 'elt' option * 'elt' tree * 'elt'' tree
               * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
               * ('elt, 'elt', 'elt'') coq_R_map2_opt
            
            val coq_R_map2_opt_rect :
              (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
              tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __
              -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree ->
              'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree ->
              Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
              __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
              -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
              'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
              tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2
              tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
              -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
              'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3)
              coq_R_map2_opt -> 'a4
            
            val coq_R_map2_opt_rec :
              (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
              tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __
              -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree ->
              'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree ->
              Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
              __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
              -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
              'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
              tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2
              tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
              -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
              'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3)
              coq_R_map2_opt -> 'a4
            
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
        
        type key = E.t
        
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
            type t = Loc.t
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec : Loc.t -> Loc.t -> bool
          
          val lt_dec : Loc.t -> Loc.t -> bool
          
          val eqb : Loc.t -> Loc.t -> bool
         end
        
        module O : 
         sig 
          module MO : 
           sig 
            module TO : 
             sig 
              type t = Loc.t
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec : Loc.t -> Loc.t -> bool
            
            val lt_dec : Loc.t -> Loc.t -> bool
            
            val eqb : Loc.t -> Loc.t -> bool
           end
         end
        
        module P : 
         sig 
          module F : 
           sig 
            val eqb : Loc.t -> Loc.t -> bool
            
            val coq_In_dec : 'a1 t -> key -> bool
            
            val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option
           end
          
          val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3
          
          val of_list : (key * 'a1) list -> 'a1 t
          
          val to_list : 'a1 t -> (key * 'a1) list
          
          val fold_rec :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> __ ->
            'a3) -> (key -> 'a1 -> 'a2 -> 'a1 t -> 'a1 t -> __ -> __ -> __ ->
            'a3 -> 'a3) -> 'a3
          
          val fold_rec_bis :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t ->
            'a2 -> __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t ->
            __ -> __ -> 'a3 -> 'a3) -> 'a3
          
          val fold_rec_nodep :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1
            -> 'a2 -> __ -> 'a3 -> 'a3) -> 'a3
          
          val fold_rec_weak :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __
            -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3
            -> 'a3) -> 'a1 t -> 'a3
          
          val fold_rel :
            (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2
            -> 'a3 -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4
            -> 'a4) -> 'a4
          
          val map_induction :
            ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 ->
            __ -> __ -> 'a2) -> 'a1 t -> 'a2
          
          val map_induction_bis :
            ('a1 t -> 'a1 t -> __ -> 'a2 -> 'a2) -> 'a2 -> (key -> 'a1 -> 'a1
            t -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
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
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> Loc.t -> 'a1 ->
          __ -> __ -> 'a2) -> 'a1 t -> 'a2
        
        val map_induction_min :
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> Loc.t -> 'a1 ->
          __ -> __ -> 'a2) -> 'a1 t -> 'a2
        
        module Raw2 : 
         sig 
          val for_all :
            (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 Raw.tree ->
            bool
          
          val strong_le :
            ('a1 -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool
          
          val filter : (key -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree
          
          type 'elt coq_R_for_all =
          | R_for_all_0 of 'elt Raw.tree
          | R_for_all_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 
             'elt * 'elt Raw.tree * Int.Z_as_Int.t * bool
             * 'elt coq_R_for_all * bool * 'elt coq_R_for_all
          
          val coq_R_for_all_rect :
            (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree ->
            __ -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
            'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all
            -> 'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1
            Raw.tree -> bool -> 'a1 coq_R_for_all -> 'a2
          
          val coq_R_for_all_rec :
            (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree ->
            __ -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
            'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all
            -> 'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1
            Raw.tree -> bool -> 'a1 coq_R_for_all -> 'a2
          
          type 'elt coq_R_strong_le =
          | R_strong_le_0 of 'elt Raw.tree * 'elt Raw.tree
          | R_strong_le_1 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
             * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t
          | R_strong_le_2 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
             * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t
             * 'elt Raw.tree * Raw.key * 'elt * 'elt Raw.tree
             * Int.Z_as_Int.t * bool * 'elt coq_R_strong_le * bool
             * 'elt coq_R_strong_le
          | R_strong_le_3 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
             * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t
             * 'elt Raw.tree * Raw.key * 'elt * 'elt Raw.tree
             * Int.Z_as_Int.t * bool * 'elt coq_R_strong_le * bool
             * 'elt coq_R_strong_le
          | R_strong_le_4 of 'elt Raw.tree * 'elt Raw.tree * 'elt Raw.tree
             * Raw.key * 'elt * 'elt Raw.tree * Int.Z_as_Int.t
             * 'elt Raw.tree * Raw.key * 'elt * 'elt Raw.tree
             * Int.Z_as_Int.t * bool * 'elt coq_R_strong_le * bool
             * 'elt coq_R_strong_le
          
          val coq_R_strong_le_rect :
            ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ ->
            'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key
            -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) ->
            ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1
            -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree ->
            Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool ->
            'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
            coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree
            -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
            Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
            -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2)
            -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key ->
            'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree ->
            Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool ->
            'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
            coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
            bool -> 'a1 coq_R_strong_le -> 'a2
          
          val coq_R_strong_le_rec :
            ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ ->
            'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key
            -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) ->
            ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1
            -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree ->
            Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool ->
            'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
            coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree
            -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
            Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
            -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2)
            -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key ->
            'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree ->
            Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool ->
            'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
            coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
            bool -> 'a1 coq_R_strong_le -> 'a2
          
          type 'elt coq_R_filter =
          | R_filter_0 of 'elt Raw.tree
          | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
             * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
             * 'elt coq_R_filter * 'elt Raw.tree * 'elt coq_R_filter
          | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
             * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
             * 'elt coq_R_filter * 'elt Raw.tree * 'elt coq_R_filter
          
          val coq_R_filter_rect :
            (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
            Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
            -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
            Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
            -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
            Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
          
          val coq_R_filter_rec :
            (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
            Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
            -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
            Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
            Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
            -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
            Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
         end
        
        val for_all :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool
        
        val strong_le : ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool
        
        val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
       end
      
      type t = Val.t M.t
      
      val empty : t
      
      val is_empty : t -> bool
      
      val find : Loc.t -> t -> Val.t
      
      val add : Loc.t -> Val.t -> t -> t
      
      val weak_add : Loc.t -> Val.t -> t -> t
      
      val fast_weak_add : Loc.t -> Val.t -> t -> t
      
      val remove : Loc.t -> t -> t
      
      val map : (Val.t -> Val.t) -> t -> t
      
      val mapi : (Loc.t -> Val.t -> Val.t) -> t -> t
      
      val fold : (Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
      
      val foldi : (Loc.t -> Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
      
      val filteri : (Loc.t -> Val.t -> bool) -> t -> t
      
      val elements : t -> (Loc.t * Val.t) list
      
      val cardinal : t -> int
      
      val le_than : t -> Loc.t -> Val.t -> bool
      
      val for_all :
        (Loc.t -> Val.t -> unit) -> (Loc.t -> Val.t -> bool) -> t -> bool
      
      val unstables :
        t -> t -> (Val.t -> Val.t -> bool) -> Loc.t list ->
        ((Loc.t * Val.t) * Val.t) list
      
      val meet_big_small : t -> t -> t
      
      val le_dec : t -> t -> bool
      
      val strong_le : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join' : Val.t option -> Val.t option -> Val.t option
      
      val join : t -> t -> t
      
      val meet' : Val.t option -> Val.t option -> Val.t option
      
      val meet : t -> t -> t
      
      val widen' : Val.t option -> Val.t option -> Val.t option
      
      val widen : t -> t -> t
      
      val narrow' : Val.t option -> Val.t option -> Val.t option
      
      val narrow : t -> t -> t
      
      val coq_ILat : t coq_TCLat
      
      val coq_IMap : (Loc.t, Val.t, t) coq_TCMap
     end
    
    module PowA : 
     sig 
      type t = PowLoc.t
      
      val le_dec : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join : t -> t -> t
      
      val meet : t -> t -> t
      
      val widen : t -> t -> t
      
      val narrow : t -> t -> t
      
      type elt = Loc.t
      
      val empty : t
      
      val is_empty : t -> bool
      
      val add : elt -> t -> t
      
      val singleton : elt -> t
      
      val mem : elt -> t -> bool
      
      val remove : elt -> t -> t
      
      val union : t -> t -> t
      
      val union_small_big : t -> t -> t
      
      val intersect : t -> t -> t
      
      val diff : t -> t -> t
      
      val subset : t -> t -> bool
      
      val filter : (elt -> bool) -> t -> t
      
      val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
      
      val iter : (elt -> unit) -> t -> unit
      
      val elements : t -> elt list
      
      val cardinal : t -> int
      
      val choose : t -> elt option
      
      val choose_only : t -> elt option
      
      val for_all : (elt -> unit) -> (elt -> bool) -> t -> bool
     end
    
    type t' = { base : Mem.t; spec : Mem.t }
    
    val t'_rect : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1
    
    val t'_rec : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1
    
    val base : t' -> Mem.t
    
    val spec : t' -> Mem.t
    
    type t = t'
    
    val all : t -> Mem.t
    
    val empty : t
    
    val is_empty : t -> bool
    
    val find : Loc.t -> t -> Val.t
    
    val add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val pre_add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val main_add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val weak_add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val pre_weak_add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val main_weak_add : PowLoc.t -> Loc.t -> Val.t -> t -> t
    
    val remove : Loc.t -> t -> t
    
    val map : (Val.t -> Val.t) -> t -> t
    
    val mapi : (Loc.t -> Val.t -> Val.t) -> t -> t
    
    val filteri : (Loc.t -> Val.t -> bool) -> t -> t
    
    val fold : (Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val foldi : (Loc.t -> Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val cardinal : t -> int
    
    val unstables :
      t -> t -> (Val.t -> Val.t -> bool) -> Loc.t list ->
      ((Loc.t * Val.t) * Val.t) list
    
    val meet_big_small : t -> t -> t
    
    val le_than : t -> Loc.t -> Val.t -> bool
    
    val le_dec : t -> t -> bool
    
    val strong_le : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module G : 
   sig 
    type t = { icfg : InterCfg.t; callgraph : Global.Callgraph.t;
               dump : Dump.t }
    
    val t_rect :
      (InterCfg.t -> Global.Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1
    
    val t_rec :
      (InterCfg.t -> Global.Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1
    
    val icfg : t -> InterCfg.t
    
    val callgraph : t -> Global.Callgraph.t
    
    val dump : t -> Dump.t
    
    val is_undef : pid_t -> t -> bool
    
    val is_call_node : t -> InterNode.t -> bool
    
    val is_exit_node : InterNode.t -> bool
    
    val is_rec : pid_t -> t -> bool
    
    val get_callees : t -> InterNode.t -> PidSet.t
    
    val remove_function : pid_t -> t -> t
    
    val remove_node : InterNode.t -> t -> t
   end
  
  type access_map = Acc.t PidMap.t
  
  module Index : 
   sig 
    module Pid : 
     sig 
      type t = string_t
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      val eq_dec : t -> t -> bool
     end
    
    module MO1 : 
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
    
    module MO2 : 
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
    
    type t = string_t * t'
    
    val compare : t -> t -> (string_t * t') OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
    
    val get_pid : t -> pid_t
    
    val get_node : t -> IntraNode.t
    
    val is_entry_node : t -> bool
    
    val is_exit_node : t -> bool
    
    val entryof : pid_t -> t
    
    val exitof : pid_t -> t
   end
  
  module Table : 
   sig 
    module A : 
     sig 
      type t = string_t * t'
      
      val compare :
        (string_t * t') -> (string_t * t') -> (string_t * t')
        OrderedType.coq_Compare
      
      val eq_dec : (string_t * t') -> (string_t * t') -> bool
     end
    
    module B : 
     sig 
      type t = Mem.t
      
      val le_dec : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join : t -> t -> t
      
      val meet : t -> t -> t
      
      val widen : t -> t -> t
      
      val narrow : t -> t -> t
     end
    
    module M : 
     sig 
      module E : 
       sig 
        type t = string_t * t'
        
        val compare :
          (string_t * t') -> (string_t * t') -> (string_t * t')
          OrderedType.coq_Compare
        
        val eq_dec : (string_t * t') -> (string_t * t') -> bool
       end
      
      module Raw : 
       sig 
        type key = string_t * t'
        
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
        
        val mem : (string_t * t') -> 'a1 tree -> bool
        
        val find : (string_t * t') -> 'a1 tree -> 'a1 option
        
        val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val remove_min :
          'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
        
        val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
        
        val remove : (string_t * t') -> 'a1 tree -> 'a1 tree
        
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
        
        val split : (string_t * t') -> 'a1 tree -> 'a1 triple
        
        val concat : 'a1 tree -> 'a1 tree -> 'a1 tree
        
        val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list
        
        val elements : 'a1 tree -> (key * 'a1) list
        
        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2
        
        type 'elt enumeration =
        | End
        | More of key * 'elt * 'elt tree * 'elt enumeration
        
        val enumeration_rect :
          'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
          'a1 enumeration -> 'a2
        
        val enumeration_rec :
          'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
          'a1 enumeration -> 'a2
        
        val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration
        
        val equal_more :
          ('a1 -> 'a1 -> bool) -> (string_t * t') -> 'a1 -> ('a1 enumeration
          -> bool) -> 'a1 enumeration -> bool
        
        val equal_cont :
          ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) ->
          'a1 enumeration -> bool
        
        val equal_end : 'a1 enumeration -> bool
        
        val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool
        
        val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree
        
        val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree
        
        val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree
        
        val map2_opt :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree
        
        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree ->
          'a3 tree
        
        module Proofs : 
         sig 
          module MX : 
           sig 
            module TO : 
             sig 
              type t = string_t * t'
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec : (string_t * t') -> (string_t * t') -> bool
            
            val lt_dec : (string_t * t') -> (string_t * t') -> bool
            
            val eqb : (string_t * t') -> (string_t * t') -> bool
           end
          
          module PX : 
           sig 
            module MO : 
             sig 
              module TO : 
               sig 
                type t = string_t * t'
               end
              
              module IsTO : 
               sig 
                
               end
              
              module OrderTac : 
               sig 
                
               end
              
              val eq_dec : (string_t * t') -> (string_t * t') -> bool
              
              val lt_dec : (string_t * t') -> (string_t * t') -> bool
              
              val eqb : (string_t * t') -> (string_t * t') -> bool
             end
           end
          
          module L : 
           sig 
            module MX : 
             sig 
              module TO : 
               sig 
                type t = string_t * t'
               end
              
              module IsTO : 
               sig 
                
               end
              
              module OrderTac : 
               sig 
                
               end
              
              val eq_dec : (string_t * t') -> (string_t * t') -> bool
              
              val lt_dec : (string_t * t') -> (string_t * t') -> bool
              
              val eqb : (string_t * t') -> (string_t * t') -> bool
             end
            
            module PX : 
             sig 
              module MO : 
               sig 
                module TO : 
                 sig 
                  type t = string_t * t'
                 end
                
                module IsTO : 
                 sig 
                  
                 end
                
                module OrderTac : 
                 sig 
                  
                 end
                
                val eq_dec : (string_t * t') -> (string_t * t') -> bool
                
                val lt_dec : (string_t * t') -> (string_t * t') -> bool
                
                val eqb : (string_t * t') -> (string_t * t') -> bool
               end
             end
            
            type key = string_t * t'
            
            type 'elt t = ((string_t * t') * 'elt) list
            
            val empty : 'a1 t
            
            val is_empty : 'a1 t -> bool
            
            val mem : key -> 'a1 t -> bool
            
            type 'elt coq_R_mem =
            | R_mem_0 of 'elt t
            | R_mem_1 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_mem_2 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_mem_3 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * bool * 'elt coq_R_mem
            
            val coq_R_mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> bool
              -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1
              coq_R_mem -> 'a2
            
            val coq_R_mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> bool
              -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1
              coq_R_mem -> 'a2
            
            val mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
            
            val find : key -> 'a1 t -> 'a1 option
            
            type 'elt coq_R_find =
            | R_find_0 of 'elt t
            | R_find_1 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_find_2 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_find_3 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * 'elt option
               * 'elt coq_R_find
            
            val coq_R_find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a1
              option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option
              -> 'a1 coq_R_find -> 'a2
            
            val coq_R_find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a1
              option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option
              -> 'a1 coq_R_find -> 'a2
            
            val find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val coq_R_find_correct :
              key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
            
            val add : key -> 'a1 -> 'a1 t -> 'a1 t
            
            type 'elt coq_R_add =
            | R_add_0 of 'elt t
            | R_add_1 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_add_2 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_add_3 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * 'elt t * 'elt coq_R_add
            
            val coq_R_add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t')
              -> 'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1
              t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1
              t -> 'a1 t -> 'a1 coq_R_add -> 'a2
            
            val coq_R_add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t')
              -> 'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1
              t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1
              t -> 'a1 t -> 'a1 coq_R_add -> 'a2
            
            val add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t')
              -> 'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1
              t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t')
              -> 'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1
              t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val coq_R_add_correct :
              key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
            
            val remove : key -> 'a1 t -> 'a1 t
            
            type 'elt coq_R_remove =
            | R_remove_0 of 'elt t
            | R_remove_1 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_remove_2 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
            | R_remove_3 of 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * 'elt t * 'elt coq_R_remove
            
            val coq_R_remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a1 t
              -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
              coq_R_remove -> 'a2
            
            val coq_R_remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a1 t
              -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
              coq_R_remove -> 'a2
            
            val remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (string_t * t') -> 'a1
              -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2) ->
              ('a1 t -> (string_t * t') -> 'a1 -> ((string_t * t') * 'a1)
              list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> (string_t * t') ->
              'a1 -> ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 ->
              'a2) -> 'a1 t -> 'a2
            
            val coq_R_remove_correct :
              key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
            
            val elements : 'a1 t -> 'a1 t
            
            val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
            
            type ('elt, 'a) coq_R_fold =
            | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
            | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
               * (string_t * t') * 'elt * ((string_t * t') * 'elt) list * 
               'a * ('elt, 'a) coq_R_fold
            
            val coq_R_fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3
              -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
            
            val coq_R_fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3
              -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
            
            val fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 ->
              'a2
            
            val fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 ->
              'a2
            
            val coq_R_fold_correct :
              (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
              coq_R_fold
            
            val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
            
            type 'elt coq_R_equal =
            | R_equal_0 of 'elt t * 'elt t
            | R_equal_1 of 'elt t * 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * bool * 'elt coq_R_equal
            | R_equal_2 of 'elt t * 'elt t * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list * (string_t * t') * 'elt
               * ((string_t * t') * 'elt) list
               * (string_t * t') OrderedType.coq_Compare
            | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
            
            val coq_R_equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              (string_t * t') OrderedType.coq_Compare -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) ->
              'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
            
            val coq_R_equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              (string_t * t') -> 'a1 -> ((string_t * t') * 'a1) list -> __ ->
              (string_t * t') OrderedType.coq_Compare -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) ->
              'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
            
            val equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t')
              OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t
              -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t
              -> 'a2
            
            val equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> ('a1 t -> 'a1 t -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t') -> 'a1 ->
              ((string_t * t') * 'a1) list -> __ -> (string_t * t')
              OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t
              -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t
              -> 'a2
            
            val coq_R_equal_correct :
              ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1
              coq_R_equal
            
            val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
            
            val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t
            
            val option_cons :
              key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list
            
            val map2_l :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t
            
            val map2_r :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t
            
            val map2 :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
              'a3 t
            
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
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
            -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2
          
          val coq_R_mem_rec :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
            -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2
          
          type 'elt coq_R_find =
          | R_find_0 of 'elt tree
          | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
          | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t
          | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
          
          val coq_R_find_rect :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
            coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1
            coq_R_find -> 'a2
          
          val coq_R_find_rec :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
            coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1
            coq_R_find -> 'a2
          
          type 'elt coq_R_bal =
          | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
          | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
             'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
          | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
             key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
             'elt * 'elt tree * Int.Z_as_Int.t
          | R_bal_8 of 'elt tree * key * 'elt * 'elt tree
          
          val coq_R_bal_rect :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
            __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
            -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
            tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
            'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1
            -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
            tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
            __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
            -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
          
          val coq_R_bal_rec :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
            __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
            -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
            tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
            'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1
            -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
            tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
            __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
            -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
          
          type 'elt coq_R_add =
          | R_add_0 of 'elt tree
          | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
          | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t
          | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
          
          val coq_R_add_rect :
            key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
            'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2
          
          val coq_R_add_rec :
            key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
            'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2
          
          type 'elt coq_R_remove_min =
          | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
          | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
             * key * 'elt * 'elt tree * Int.Z_as_Int.t
             * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min * 'elt tree
             * (key * 'elt)
          
          val coq_R_remove_min_rect :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
            key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
            coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2)
            -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1))
            -> 'a1 coq_R_remove_min -> 'a2
          
          val coq_R_remove_min_rec :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
            key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
            coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2)
            -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1))
            -> 'a1 coq_R_remove_min -> 'a2
          
          type 'elt coq_R_merge =
          | R_merge_0 of 'elt tree * 'elt tree
          | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t
          | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * 
             key * 'elt
          
          val coq_R_merge_rect :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
            'a1 coq_R_merge -> 'a2
          
          val coq_R_merge_rec :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
            'a1 coq_R_merge -> 'a2
          
          type 'elt coq_R_remove =
          | R_remove_0 of 'elt tree
          | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
          | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t
          | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
             * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
          
          val coq_R_remove_rect :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
            coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
            coq_R_remove -> 'a2
          
          val coq_R_remove_rec :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
            coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
            coq_R_remove -> 'a2
          
          type 'elt coq_R_concat =
          | R_concat_0 of 'elt tree * 'elt tree
          | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t
          | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt
             * 'elt tree * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
          
          val coq_R_concat_rect :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat ->
            'a2
          
          val coq_R_concat_rec :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat ->
            'a2
          
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
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
            -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1
            tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1
            triple -> 'a1 coq_R_split -> 'a2
          
          val coq_R_split_rec :
            (string_t * t') -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
            -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1
            tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1
            triple -> 'a1 coq_R_split -> 'a2
          
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
            (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
            -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
            tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree ->
            'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
          
          val coq_R_map_option_rec :
            (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
            -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
            tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree ->
            'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
          
          type ('elt, 'elt', 'elt'') coq_R_map2_opt =
          | R_map2_opt_0 of 'elt tree * 'elt' tree
          | R_map2_opt_1 of 'elt tree * 'elt' tree * 'elt tree * key * 
             'elt * 'elt tree * Int.Z_as_Int.t
          | R_map2_opt_2 of 'elt tree * 'elt' tree * 'elt tree * key * 
             'elt * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 'elt'
             * 'elt' tree * Int.Z_as_Int.t * 'elt' tree * 'elt' option
             * 'elt' tree * 'elt'' * 'elt'' tree
             * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
             * ('elt, 'elt', 'elt'') coq_R_map2_opt
          | R_map2_opt_3 of 'elt tree * 'elt' tree * 'elt tree * key * 
             'elt * 'elt tree * Int.Z_as_Int.t * 'elt' tree * key * 'elt'
             * 'elt' tree * Int.Z_as_Int.t * 'elt' tree * 'elt' option
             * 'elt' tree * 'elt'' tree
             * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
             * ('elt, 'elt', 'elt'') coq_R_map2_opt
          
          val coq_R_map2_opt_rect :
            (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
            tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ ->
            'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __
            -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree ->
            ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __
            -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree ->
            ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2,
            'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4
          
          val coq_R_map2_opt_rec :
            (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
            tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ ->
            'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __
            -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree ->
            ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __
            -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree ->
            ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2,
            'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4
          
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
      
      type key = string_t * t'
      
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
          type t = string_t * t'
         end
        
        module IsTO : 
         sig 
          
         end
        
        module OrderTac : 
         sig 
          
         end
        
        val eq_dec : (string_t * t') -> (string_t * t') -> bool
        
        val lt_dec : (string_t * t') -> (string_t * t') -> bool
        
        val eqb : (string_t * t') -> (string_t * t') -> bool
       end
      
      module O : 
       sig 
        module MO : 
         sig 
          module TO : 
           sig 
            type t = string_t * t'
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec : (string_t * t') -> (string_t * t') -> bool
          
          val lt_dec : (string_t * t') -> (string_t * t') -> bool
          
          val eqb : (string_t * t') -> (string_t * t') -> bool
         end
       end
      
      module P : 
       sig 
        module F : 
         sig 
          val eqb : (string_t * t') -> (string_t * t') -> bool
          
          val coq_In_dec : 'a1 t -> key -> bool
          
          val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option
         end
        
        val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3
        
        val of_list : (key * 'a1) list -> 'a1 t
        
        val to_list : 'a1 t -> (key * 'a1) list
        
        val fold_rec :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> __ -> 'a3)
          -> (key -> 'a1 -> 'a2 -> 'a1 t -> 'a1 t -> __ -> __ -> __ -> 'a3 ->
          'a3) -> 'a3
        
        val fold_rec_bis :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t ->
          'a2 -> __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t ->
          __ -> __ -> 'a3 -> 'a3) -> 'a3
        
        val fold_rec_nodep :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1 ->
          'a2 -> __ -> 'a3 -> 'a3) -> 'a3
        
        val fold_rec_weak :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __
          -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3
          -> 'a3) -> 'a1 t -> 'a3
        
        val fold_rel :
          (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2 ->
          'a3 -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4 ->
          'a4) -> 'a4
        
        val map_induction :
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 -> __
          -> __ -> 'a2) -> 'a1 t -> 'a2
        
        val map_induction_bis :
          ('a1 t -> 'a1 t -> __ -> 'a2 -> 'a2) -> 'a2 -> (key -> 'a1 -> 'a1 t
          -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
        
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
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (string_t * t') ->
        'a1 -> __ -> __ -> 'a2) -> 'a1 t -> 'a2
      
      val map_induction_min :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (string_t * t') ->
        'a1 -> __ -> __ -> 'a2) -> 'a1 t -> 'a2
      
      module Raw2 : 
       sig 
        val for_all :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 Raw.tree ->
          bool
        
        val strong_le :
          ('a1 -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool
        
        val filter : (key -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree
        
        type 'elt coq_R_for_all =
        | R_for_all_0 of 'elt Raw.tree
        | R_for_all_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
           * 'elt Raw.tree * Int.Z_as_Int.t * bool * 'elt coq_R_for_all
           * bool * 'elt coq_R_for_all
        
        val coq_R_for_all_rect :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
          -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all ->
          'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          bool -> 'a1 coq_R_for_all -> 'a2
        
        val coq_R_for_all_rec :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
          -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all ->
          'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          bool -> 'a1 coq_R_for_all -> 'a2
        
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
          ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2)
          -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1
          -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1
          -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
          coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le
          -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree ->
          Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1
          Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __
          -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
          coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree ->
          'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t ->
          __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
          __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          'a1 Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2
        
        val coq_R_strong_le_rec :
          ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2)
          -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1
          -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1
          -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
          coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le
          -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree ->
          Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1
          Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __
          -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1
          coq_R_strong_le -> 'a2 -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree ->
          'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t ->
          __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> __ ->
          __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          'a1 Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2
        
        type 'elt coq_R_filter =
        | R_filter_0 of 'elt Raw.tree
        | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
           * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
           * 'elt coq_R_filter * 'elt Raw.tree * 'elt coq_R_filter
        | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
           * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree
           * 'elt coq_R_filter * 'elt Raw.tree * 'elt coq_R_filter
        
        val coq_R_filter_rect :
          (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
        
        val coq_R_filter_rec :
          (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2
       end
      
      val for_all :
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool
      
      val strong_le : ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool
      
      val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
     end
    
    type t = Mem.t M.t
    
    val empty : t
    
    val is_empty : t -> bool
    
    val find : (string_t * t') -> t -> Mem.t
    
    val add : (string_t * t') -> Mem.t -> t -> t
    
    val weak_add : (string_t * t') -> Mem.t -> t -> t
    
    val fast_weak_add : (string_t * t') -> Mem.t -> t -> t
    
    val remove : (string_t * t') -> t -> t
    
    val map : (Mem.t -> Mem.t) -> t -> t
    
    val mapi : ((string_t * t') -> Mem.t -> Mem.t) -> t -> t
    
    val fold : (Mem.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val foldi : ((string_t * t') -> Mem.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val filteri : ((string_t * t') -> Mem.t -> bool) -> t -> t
    
    val elements : t -> ((string_t * t') * Mem.t) list
    
    val cardinal : t -> int
    
    val le_than : t -> (string_t * t') -> Mem.t -> bool
    
    val for_all :
      ((string_t * t') -> Mem.t -> unit) -> ((string_t * t') -> Mem.t ->
      bool) -> t -> bool
    
    val unstables :
      t -> t -> (Mem.t -> Mem.t -> bool) -> (string_t * t') list ->
      (((string_t * t') * Mem.t) * Mem.t) list
    
    val meet_big_small : t -> t -> t
    
    val le_dec : t -> t -> bool
    
    val strong_le : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join' : Mem.t option -> Mem.t option -> Mem.t option
    
    val join : t -> t -> t
    
    val meet' : Mem.t option -> Mem.t option -> Mem.t option
    
    val meet : t -> t -> t
    
    val widen' : Mem.t option -> Mem.t option -> Mem.t option
    
    val widen : t -> t -> t
    
    val narrow' : Mem.t option -> Mem.t option -> Mem.t option
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
    
    val coq_IMap : (string_t * t', Mem.t, t) coq_TCMap
   end
  
  type param = (update_mode * phase_t) * Mem.PowA.t
  
  val param_mode : param -> update_mode
  
  val param_phase : param -> phase_t
  
  val param_locs : param -> Mem.PowA.t
  
  type 't coq_AccPair = 't * Acc.t
  
  val coq_MAcc : __ coq_AccPair coq_Monad
  
  val get_v : 'a1 coq_AccPair -> 'a1
  
  val get_acc : 'a1 coq_AccPair -> Acc.t
  
  val coq_MId : __ coq_Monad
  
  val coq_AccMem :
    (__ coq_AccPair, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic
  
  val coq_IdMem : (__, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic
  
  val can_strong_update_lv : (pid_t -> bool) -> Loc.t -> bool
  
  val can_strong_update : param -> G.t -> Mem.PowA.t -> bool
  
  val mem_lookup :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    Mem.PowA.t -> Mem.t -> 'a1
  
  val add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1
  
  val weak_add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1
  
  val mem_update :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> G.t -> Mem.PowA.t -> Val.t -> Mem.t -> 'a1
  
  val run :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> cmd -> (Mem.t * G.t) -> 'a1
  
  val run_access :
    param -> InterNode.t -> cmd -> (Mem.t * G.t) -> (Mem.t * G.t) coq_AccPair
  
  val run_only : param -> InterNode.t -> cmd -> (Mem.t * G.t) -> Mem.t * G.t
  
  type query 
  
  type status 
  
  val collect_query : cmd -> (query * DPos.DPos.t) list
  
  val check_query :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    pid_t -> Mem.t -> query -> 'a1
 end

