open AUGER_Monad
open DLat
open DMap
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomCon
open DomMem
open InterCfg
open InterNode
open IntraNode
open List0
open Pos
open Query
open SemAbs
open SemEval
open SemMem
open StringFun
open Sumbool
open Syn
open TStr
open UserInputType
open VocabA

type __ = Obj.t

module Input : 
 sig 
  module Proc : 
   sig 
    type t = string_t
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module PowProc : 
   sig 
    module A : 
     sig 
      type t = string_t
      
      val compare : string_t -> string_t -> string_t OrderedType.coq_Compare
      
      val eq_dec : string_t -> string_t -> bool
     end
    
    module SS : 
     sig 
      module S : 
       sig 
        module X' : 
         sig 
          type t = string_t
          
          val eq_dec : string_t -> string_t -> bool
          
          val compare : string_t -> string_t -> comparison
         end
        
        module MSet : 
         sig 
          module Raw : 
           sig 
            type elt = string_t
            
            type tree = PowProc.SS.S.MSet.Raw.tree =
            | Leaf
            | Node of Int.Z_as_Int.t * tree * string_t * tree
            
            val empty : tree
            
            val is_empty : tree -> bool
            
            val mem : string_t -> tree -> bool
            
            val min_elt : tree -> elt option
            
            val max_elt : tree -> elt option
            
            val choose : tree -> elt option
            
            val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
            
            val elements_aux : string_t list -> tree -> string_t list
            
            val elements : tree -> string_t list
            
            val rev_elements_aux : string_t list -> tree -> string_t list
            
            val rev_elements : tree -> string_t list
            
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
              string_t -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_cont :
              tree -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_end : enumeration -> comparison
            
            val compare : tree -> tree -> comparison
            
            val equal : tree -> tree -> bool
            
            val subsetl : (tree -> bool) -> string_t -> tree -> bool
            
            val subsetr : (tree -> bool) -> string_t -> tree -> bool
            
            val subset : tree -> tree -> bool
            
            type t = tree
            
            val height : t -> Int.Z_as_Int.t
            
            val singleton : string_t -> tree
            
            val create : t -> string_t -> t -> tree
            
            val assert_false : t -> string_t -> t -> tree
            
            val bal : t -> string_t -> t -> tree
            
            val add : string_t -> tree -> tree
            
            val join : tree -> elt -> t -> t
            
            val remove_min : tree -> elt -> t -> t * elt
            
            val merge : tree -> tree -> tree
            
            val remove : string_t -> tree -> tree
            
            val concat : tree -> tree -> tree
            
            type triple = { t_left : t; t_in : bool; t_right : t }
            
            val t_left : triple -> t
            
            val t_in : triple -> bool
            
            val t_right : triple -> t
            
            val split : string_t -> tree -> triple
            
            val inter : tree -> tree -> tree
            
            val diff : tree -> tree -> tree
            
            val union : tree -> tree -> tree
            
            val filter : (elt -> bool) -> tree -> tree
            
            val partition : (elt -> bool) -> t -> t * t
            
            val ltb_tree : string_t -> tree -> bool
            
            val gtb_tree : string_t -> tree -> bool
            
            val isok : tree -> bool
            
            module MX : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = string_t
                  
                  val compare : string_t -> string_t -> comparison
                  
                  val eq_dec : string_t -> string_t -> bool
                 end
                
                module TO : 
                 sig 
                  type t = string_t
                  
                  val compare : string_t -> string_t -> comparison
                  
                  val eq_dec : string_t -> string_t -> bool
                 end
               end
              
              val eq_dec : string_t -> string_t -> bool
              
              val lt_dec : string_t -> string_t -> bool
              
              val eqb : string_t -> string_t -> bool
             end
            
            type coq_R_min_elt =
            | R_min_elt_0 of tree
            | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * string_t * tree
            | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * string_t * 
               tree * Int.Z_as_Int.t * tree * string_t * tree * elt option
               * coq_R_min_elt
            
            type coq_R_max_elt =
            | R_max_elt_0 of tree
            | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * string_t * tree
            | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * string_t * 
               tree * Int.Z_as_Int.t * tree * string_t * tree * elt option
               * coq_R_max_elt
            
            module L : 
             sig 
              module MO : 
               sig 
                module OrderTac : 
                 sig 
                  module OTF : 
                   sig 
                    type t = string_t
                    
                    val compare : string_t -> string_t -> comparison
                    
                    val eq_dec : string_t -> string_t -> bool
                   end
                  
                  module TO : 
                   sig 
                    type t = string_t
                    
                    val compare : string_t -> string_t -> comparison
                    
                    val eq_dec : string_t -> string_t -> bool
                   end
                 end
                
                val eq_dec : string_t -> string_t -> bool
                
                val lt_dec : string_t -> string_t -> bool
                
                val eqb : string_t -> string_t -> bool
               end
             end
            
            val flatten_e : enumeration -> elt list
            
            type coq_R_bal =
            | R_bal_0 of t * string_t * t
            | R_bal_1 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_bal_2 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_bal_3 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree
            | R_bal_4 of t * string_t * t
            | R_bal_5 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_bal_6 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_bal_7 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree
            | R_bal_8 of t * string_t * t
            
            type coq_R_remove_min =
            | R_remove_min_0 of tree * elt * t
            | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
               * string_t * tree * (t * elt) * coq_R_remove_min * t * 
               elt
            
            type coq_R_merge =
            | R_merge_0 of tree * tree
            | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               elt
            
            type coq_R_concat =
            | R_concat_0 of tree * tree
            | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               elt
            
            type coq_R_inter =
            | R_inter_0 of tree * tree
            | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               bool * t * tree * coq_R_inter * tree * coq_R_inter
            | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               bool * t * tree * coq_R_inter * tree * coq_R_inter
            
            type coq_R_diff =
            | R_diff_0 of tree * tree
            | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               bool * t * tree * coq_R_diff * tree * coq_R_diff
            | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               bool * t * tree * coq_R_diff * tree * coq_R_diff
            
            type coq_R_union =
            | R_union_0 of tree * tree
            | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree
            | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
               * tree * Int.Z_as_Int.t * tree * string_t * tree * t * 
               bool * t * tree * coq_R_union * tree * coq_R_union
           end
          
          module E : 
           sig 
            type t = string_t
            
            val compare : string_t -> string_t -> comparison
            
            val eq_dec : string_t -> string_t -> bool
           end
          
          type elt = string_t
          
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
        
        type elt = string_t
        
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
          val eqb : string_t -> string_t -> bool
         end
        
        val min_elt : t -> elt option
        
        val max_elt : t -> elt option
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        module E : 
         sig 
          type t = string_t
          
          val compare :
            string_t -> string_t -> string_t OrderedType.coq_Compare
          
          val eq_dec : string_t -> string_t -> bool
         end
       end
      
      module X' : 
       sig 
        type t = string_t
        
        val eq_dec : string_t -> string_t -> bool
        
        val compare : string_t -> string_t -> comparison
       end
      
      module MSet : 
       sig 
        module Raw : 
         sig 
          type elt = string_t
          
          type tree = PowProc.SS.S.MSet.Raw.tree =
          | Leaf
          | Node of Int.Z_as_Int.t * tree * string_t * tree
          
          val empty : tree
          
          val is_empty : tree -> bool
          
          val mem : string_t -> tree -> bool
          
          val min_elt : tree -> elt option
          
          val max_elt : tree -> elt option
          
          val choose : tree -> elt option
          
          val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
          
          val elements_aux : string_t list -> tree -> string_t list
          
          val elements : tree -> string_t list
          
          val rev_elements_aux : string_t list -> tree -> string_t list
          
          val rev_elements : tree -> string_t list
          
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
            string_t -> (enumeration -> comparison) -> enumeration ->
            comparison
          
          val compare_cont :
            tree -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_end : enumeration -> comparison
          
          val compare : tree -> tree -> comparison
          
          val equal : tree -> tree -> bool
          
          val subsetl : (tree -> bool) -> string_t -> tree -> bool
          
          val subsetr : (tree -> bool) -> string_t -> tree -> bool
          
          val subset : tree -> tree -> bool
          
          type t = tree
          
          val height : t -> Int.Z_as_Int.t
          
          val singleton : string_t -> tree
          
          val create : t -> string_t -> t -> tree
          
          val assert_false : t -> string_t -> t -> tree
          
          val bal : t -> string_t -> t -> tree
          
          val add : string_t -> tree -> tree
          
          val join : tree -> elt -> t -> t
          
          val remove_min : tree -> elt -> t -> t * elt
          
          val merge : tree -> tree -> tree
          
          val remove : string_t -> tree -> tree
          
          val concat : tree -> tree -> tree
          
          type triple = { t_left : t; t_in : bool; t_right : t }
          
          val t_left : triple -> t
          
          val t_in : triple -> bool
          
          val t_right : triple -> t
          
          val split : string_t -> tree -> triple
          
          val inter : tree -> tree -> tree
          
          val diff : tree -> tree -> tree
          
          val union : tree -> tree -> tree
          
          val filter : (elt -> bool) -> tree -> tree
          
          val partition : (elt -> bool) -> t -> t * t
          
          val ltb_tree : string_t -> tree -> bool
          
          val gtb_tree : string_t -> tree -> bool
          
          val isok : tree -> bool
          
          module MX : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t = string_t
                
                val compare : string_t -> string_t -> comparison
                
                val eq_dec : string_t -> string_t -> bool
               end
              
              module TO : 
               sig 
                type t = string_t
                
                val compare : string_t -> string_t -> comparison
                
                val eq_dec : string_t -> string_t -> bool
               end
             end
            
            val eq_dec : string_t -> string_t -> bool
            
            val lt_dec : string_t -> string_t -> bool
            
            val eqb : string_t -> string_t -> bool
           end
          
          type coq_R_min_elt =
          | R_min_elt_0 of tree
          | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * string_t * tree
          | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * string_t * 
             tree * Int.Z_as_Int.t * tree * string_t * tree * elt option
             * coq_R_min_elt
          
          type coq_R_max_elt =
          | R_max_elt_0 of tree
          | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * string_t * tree
          | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * string_t * 
             tree * Int.Z_as_Int.t * tree * string_t * tree * elt option
             * coq_R_max_elt
          
          module L : 
           sig 
            module MO : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = string_t
                  
                  val compare : string_t -> string_t -> comparison
                  
                  val eq_dec : string_t -> string_t -> bool
                 end
                
                module TO : 
                 sig 
                  type t = string_t
                  
                  val compare : string_t -> string_t -> comparison
                  
                  val eq_dec : string_t -> string_t -> bool
                 end
               end
              
              val eq_dec : string_t -> string_t -> bool
              
              val lt_dec : string_t -> string_t -> bool
              
              val eqb : string_t -> string_t -> bool
             end
           end
          
          val flatten_e : enumeration -> elt list
          
          type coq_R_bal =
          | R_bal_0 of t * string_t * t
          | R_bal_1 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_bal_2 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_bal_3 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree
          | R_bal_4 of t * string_t * t
          | R_bal_5 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_bal_6 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_bal_7 of t * string_t * t * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree
          | R_bal_8 of t * string_t * t
          
          type coq_R_remove_min =
          | R_remove_min_0 of tree * elt * t
          | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
             * string_t * tree * (t * elt) * coq_R_remove_min * t * elt
          
          type coq_R_merge =
          | R_merge_0 of tree * tree
          | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree * t * elt
          
          type coq_R_concat =
          | R_concat_0 of tree * tree
          | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree * t * elt
          
          type coq_R_inter =
          | R_inter_0 of tree * tree
          | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          
          type coq_R_diff =
          | R_diff_0 of tree * tree
          | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * string_t * tree
          | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * string_t * 
             tree * Int.Z_as_Int.t * tree * string_t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * string_t * 
             tree * Int.Z_as_Int.t * tree * string_t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          
          type coq_R_union =
          | R_union_0 of tree * tree
          | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree
          | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * string_t
             * tree * Int.Z_as_Int.t * tree * string_t * tree * t * bool * 
             t * tree * coq_R_union * tree * coq_R_union
         end
        
        module E : 
         sig 
          type t = string_t
          
          val compare : string_t -> string_t -> comparison
          
          val eq_dec : string_t -> string_t -> bool
         end
        
        type elt = string_t
        
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
      
      type elt = string_t
      
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
        val eqb : string_t -> string_t -> bool
       end
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      module E : 
       sig 
        type t = string_t
        
        val compare :
          string_t -> string_t -> string_t OrderedType.coq_Compare
        
        val eq_dec : string_t -> string_t -> bool
       end
      
      module SF : 
       sig 
        val eqb : string_t -> string_t -> bool
       end
      
      val choose_only : t -> elt option
      
      val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
      
      val cond_eq_rect :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
      
      val cond_eq_rec :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
     end
    
    type elt = string_t
    
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
    
    val add : string_t -> t -> t
    
    val singleton : string_t -> t
    
    val mem : SS.elt -> SS.t -> bool
    
    val remove : string_t -> t -> t
    
    val union : SS.t -> SS.t -> SS.t
    
    val union_small_big : SS.t -> SS.t -> SS.t
    
    val intersect : SS.t -> SS.t -> SS.t
    
    val diff : SS.t -> SS.t -> SS.t
    
    val subset : SS.t -> SS.t -> bool
    
    val filter : (string_t -> bool) -> t -> t
    
    val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1
    
    val iter : (string_t -> unit) -> t -> unit
    
    val elements : SS.t -> SS.elt list
    
    val cardinal : SS.t -> int
    
    val choose : SS.t -> SS.elt option
    
    val choose_only : SS.t -> SS.elt option
    
    val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool
    
    val coq_ILat : t coq_TCLat
    
    val coq_ISet : (string_t, t) coq_TCSet
   end
  
  module ProcMap : 
   sig 
    module E : 
     sig 
      type t = string_t
      
      val compare : string_t -> string_t -> string_t OrderedType.coq_Compare
      
      val eq_dec : string_t -> string_t -> bool
     end
    
    module Raw : 
     sig 
      type key = string_t
      
      type 'elt tree = 'elt ProcMap.Raw.tree =
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
      
      val mem : string_t -> 'a1 tree -> bool
      
      val find : string_t -> 'a1 tree -> 'a1 option
      
      val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val remove_min :
        'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
      
      val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
      
      val remove : string_t -> 'a1 tree -> 'a1 tree
      
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
      
      val split : string_t -> 'a1 tree -> 'a1 triple
      
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
        ('a1 -> 'a1 -> bool) -> string_t -> 'a1 -> ('a1 enumeration -> bool)
        -> 'a1 enumeration -> bool
      
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
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree ->
        'a3 tree
      
      module Proofs : 
       sig 
        module MX : 
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
        
        module PX : 
         sig 
          module MO : 
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
         end
        
        module L : 
         sig 
          module MX : 
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
          
          module PX : 
           sig 
            module MO : 
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
           end
          
          type key = string_t
          
          type 'elt t = (string_t * 'elt) list
          
          val empty : 'a1 t
          
          val is_empty : 'a1 t -> bool
          
          val mem : key -> 'a1 t -> bool
          
          type 'elt coq_R_mem =
          | R_mem_0 of 'elt t
          | R_mem_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_mem_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_mem_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
             * bool * 'elt coq_R_mem
          
          val coq_R_mem_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
            bool -> 'a1 coq_R_mem -> 'a2
          
          val coq_R_mem_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t ->
            bool -> 'a1 coq_R_mem -> 'a2
          
          val mem_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val mem_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
          
          val find : key -> 'a1 t -> 'a1 option
          
          type 'elt coq_R_find =
          | R_find_0 of 'elt t
          | R_find_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_find_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_find_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
             * 'elt option * 'elt coq_R_find
          
          val coq_R_find_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1
            t -> 'a1 option -> 'a1 coq_R_find -> 'a2
          
          val coq_R_find_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1
            t -> 'a1 option -> 'a1 coq_R_find -> 'a2
          
          val find_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val find_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val coq_R_find_correct :
            key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
          
          val add : key -> 'a1 -> 'a1 t -> 'a1 t
          
          type 'elt coq_R_add =
          | R_add_0 of 'elt t
          | R_add_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_add_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_add_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
             * 'elt t * 'elt coq_R_add
          
          val coq_R_add_rect :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
            'a1 t -> 'a1 coq_R_add -> 'a2
          
          val coq_R_add_rec :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t ->
            'a1 t -> 'a1 coq_R_add -> 'a2
          
          val add_rect :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val add_rec :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val coq_R_add_correct :
            key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
          
          val remove : key -> 'a1 t -> 'a1 t
          
          type 'elt coq_R_remove =
          | R_remove_0 of 'elt t
          | R_remove_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_remove_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
          | R_remove_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
             * 'elt t * 'elt coq_R_remove
          
          val coq_R_remove_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t
            -> 'a1 t -> 'a1 coq_R_remove -> 'a2
          
          val coq_R_remove_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t
            -> 'a1 t -> 'a1 coq_R_remove -> 'a2
          
          val remove_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val remove_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
          
          val coq_R_remove_correct :
            key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
          
          val elements : 'a1 t -> 'a1 t
          
          val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
          
          type ('elt, 'a) coq_R_fold =
          | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
          | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * string_t
             * 'elt * (string_t * 'elt) list * 'a * ('elt, 'a) coq_R_fold
          
          val coq_R_fold_rect :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold ->
            'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3
            -> ('a1, 'a3) coq_R_fold -> 'a2
          
          val coq_R_fold_rec :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold ->
            'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3
            -> ('a1, 'a3) coq_R_fold -> 'a2
          
          val fold_rect :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 ->
            'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
          
          val fold_rec :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 ->
            'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
          
          val coq_R_fold_correct :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
            coq_R_fold
          
          val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
          
          type 'elt coq_R_equal =
          | R_equal_0 of 'elt t * 'elt t
          | R_equal_1 of 'elt t * 'elt t * string_t * 'elt
             * (string_t * 'elt) list * string_t * 'elt
             * (string_t * 'elt) list * bool * 'elt coq_R_equal
          | R_equal_2 of 'elt t * 'elt t * string_t * 'elt
             * (string_t * 'elt) list * string_t * 'elt
             * (string_t * 'elt) list * string_t OrderedType.coq_Compare
          | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
          
          val coq_R_equal_rect :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> string_t
            OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
            'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t ->
            bool -> 'a1 coq_R_equal -> 'a2
          
          val coq_R_equal_rec :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
            string_t -> 'a1 -> (string_t * 'a1) list -> __ -> string_t -> 'a1
            -> (string_t * 'a1) list -> __ -> string_t
            OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t ->
            'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t ->
            bool -> 'a1 coq_R_equal -> 'a2
          
          val equal_rect :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2 -> 'a2) -> ('a1 t -> 'a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> string_t OrderedType.coq_Compare
            -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
            __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
          
          val equal_rec :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list -> __
            -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
            'a2 -> 'a2) -> ('a1 t -> 'a1 t -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> string_t -> 'a1 ->
            (string_t * 'a1) list -> __ -> string_t OrderedType.coq_Compare
            -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
            __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
          
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
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3
            t
          
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
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool ->
          'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
          -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
          tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree ->
          bool -> 'a1 coq_R_mem -> 'a2
        
        val coq_R_mem_rec :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool ->
          'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
          -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1
          tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree ->
          bool -> 'a1 coq_R_mem -> 'a2
        
        type 'elt coq_R_find =
        | R_find_0 of 'elt tree
        | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
        | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
        
        val coq_R_find_rect :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
        val coq_R_find_rec :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2
        
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
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
        
        val coq_R_bal_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
        
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
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2
        
        val coq_R_add_rec :
          key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
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
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2
        
        val coq_R_remove_min_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
          key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2
        
        type 'elt coq_R_merge =
        | R_merge_0 of 'elt tree * 'elt tree
        | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt
        
        val coq_R_merge_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2
        
        val coq_R_merge_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2
        
        type 'elt coq_R_remove =
        | R_remove_0 of 'elt tree
        | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        
        val coq_R_remove_rect :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree
          -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
          -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
        
        val coq_R_remove_rec :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree
          -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
          -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
        
        type 'elt coq_R_concat =
        | R_concat_0 of 'elt tree * 'elt tree
        | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
        
        val coq_R_concat_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
        
        val coq_R_concat_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
        
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
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
          tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
          tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
          option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
          coq_R_split -> 'a2
        
        val coq_R_split_rec :
          string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
          tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
          tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
          option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
          coq_R_split -> 'a2
        
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
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3
        
        val coq_R_map_option_rec :
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3
        
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
           * 'elt' tree * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
           * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
        
        val coq_R_map2_opt_rect :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4
        
        val coq_R_map2_opt_rec :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4
        
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
        type t = E.t
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
    
    module O : 
     sig 
      module MO : 
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
        
        val eq_dec : string_t -> string_t -> bool
        
        val lt_dec : string_t -> string_t -> bool
        
        val eqb : string_t -> string_t -> bool
       end
     end
    
    module P : 
     sig 
      module F : 
       sig 
        val eqb : string_t -> string_t -> bool
        
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
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t -> 'a2
        -> __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ ->
        __ -> 'a3 -> 'a3) -> 'a3
      
      val fold_rec_nodep :
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1 ->
        'a2 -> __ -> 'a3 -> 'a3) -> 'a3
      
      val fold_rec_weak :
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __ ->
        'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3 ->
        'a3) -> 'a1 t -> 'a3
      
      val fold_rel :
        (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2 ->
        'a3 -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4 -> 'a4)
        -> 'a4
      
      val map_induction :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 -> __ ->
        __ -> 'a2) -> 'a1 t -> 'a2
      
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
      ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> string_t -> 'a1 -> __
      -> __ -> 'a2) -> 'a1 t -> 'a2
    
    val map_induction_min :
      ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> string_t -> 'a1 -> __
      -> __ -> 'a2) -> 'a1 t -> 'a2
    
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
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
        -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2
        -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool
        -> 'a1 coq_R_for_all -> 'a2
      
      val coq_R_for_all_rec :
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
        -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2
        -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool
        -> 'a1 coq_R_for_all -> 'a2
      
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
        -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
        Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1
        Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
        -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) ->
        ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key ->
        'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
        coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool -> 'a1
        coq_R_strong_le -> 'a2
      
      val coq_R_strong_le_rec :
        ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2)
        -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
        Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1
        Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
        -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) ->
        ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key ->
        'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
        coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool -> 'a1
        coq_R_strong_le -> 'a2
      
      type 'elt coq_R_filter =
      | R_filter_0 of 'elt Raw.tree
      | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      
      val coq_R_filter_rect :
        (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t
        -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
        Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ ->
        'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
        coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2
      
      val coq_R_filter_rec :
        (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t
        -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
        Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ ->
        'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
        coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2
     end
    
    val for_all :
      (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool
    
    val strong_le : ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool
    
    val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
   end
  
  module GVar : 
   sig 
    type t = string_t
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module LVar : 
   sig 
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
    
    type t = string_t * string_t
    
    val compare : t -> t -> (string_t * string_t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
    
    val fst : ('a1 * 'a2) -> 'a1
    
    val snd : ('a1 * 'a2) -> 'a2
   end
  
  module Var : 
   sig 
    type t = (string_t, string_t * string_t) sum
    
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
    
    val compare' : int -> t -> t -> t OrderedType.coq_Compare
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec' : int -> t -> t -> bool
    
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
  
  module VarAllocsite : 
   sig 
    type t =
      ((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum
    
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
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec :
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum -> ((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum -> bool
      
      val lt_dec :
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum -> ((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum -> bool
      
      val eqb :
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum -> ((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum -> bool
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
      ((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list
    
    val compare :
      t -> t -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
      string_t) sum) sum) sum * string_t list) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
    
    val fst : ('a1 * 'a2) -> 'a1
    
    val snd : ('a1 * 'a2) -> 'a2
   end
  
  module PowLoc : 
   sig 
    module A : 
     sig 
      type t =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
      val compare :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> (((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
        OrderedType.coq_Compare
      
      val eq_dec :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
     end
    
    module SS : 
     sig 
      module S : 
       sig 
        module X' : 
         sig 
          type t =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
          
          val compare :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> comparison
         end
        
        module MSet : 
         sig 
          module Raw : 
           sig 
            type elt =
              ((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list
            
            type tree = PowLoc.SS.S.MSet.Raw.tree =
            | Leaf
            | Node of Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            
            val empty : tree
            
            val is_empty : tree -> bool
            
            val mem :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> bool
            
            val min_elt : tree -> elt option
            
            val max_elt : tree -> elt option
            
            val choose : tree -> elt option
            
            val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
            
            val elements_aux :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) list -> tree ->
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) list
            
            val elements :
              tree -> (((string_t, string_t * string_t) sum, (string_t * t',
              (unit, string_t) sum) sum) sum * string_t list) list
            
            val rev_elements_aux :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) list -> tree ->
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) list
            
            val rev_elements :
              tree -> (((string_t, string_t * string_t) sum, (string_t * t',
              (unit, string_t) sum) sum) sum * string_t list) list
            
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
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (enumeration ->
              comparison) -> enumeration -> comparison
            
            val compare_cont :
              tree -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_end : enumeration -> comparison
            
            val compare : tree -> tree -> comparison
            
            val equal : tree -> tree -> bool
            
            val subsetl :
              (tree -> bool) -> (((string_t, string_t * string_t) sum,
              (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
              -> tree -> bool
            
            val subsetr :
              (tree -> bool) -> (((string_t, string_t * string_t) sum,
              (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
              -> tree -> bool
            
            val subset : tree -> tree -> bool
            
            type t = tree
            
            val height : t -> Int.Z_as_Int.t
            
            val singleton :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree
            
            val create :
              t -> (((string_t, string_t * string_t) sum, (string_t * t',
              (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
            
            val assert_false :
              t -> (((string_t, string_t * string_t) sum, (string_t * t',
              (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
            
            val bal :
              t -> (((string_t, string_t * string_t) sum, (string_t * t',
              (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
            
            val add :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> tree
            
            val join : tree -> elt -> t -> t
            
            val remove_min : tree -> elt -> t -> t * elt
            
            val merge : tree -> tree -> tree
            
            val remove :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> tree
            
            val concat : tree -> tree -> tree
            
            type triple = { t_left : t; t_in : bool; t_right : t }
            
            val t_left : triple -> t
            
            val t_in : triple -> bool
            
            val t_right : triple -> t
            
            val split :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> triple
            
            val inter : tree -> tree -> tree
            
            val diff : tree -> tree -> tree
            
            val union : tree -> tree -> tree
            
            val filter : (elt -> bool) -> tree -> tree
            
            val partition : (elt -> bool) -> t -> t * t
            
            val ltb_tree :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> bool
            
            val gtb_tree :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> tree -> bool
            
            val isok : tree -> bool
            
            module MX : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                  
                  val compare :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    comparison
                  
                  val eq_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                 end
                
                module TO : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                  
                  val compare :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    comparison
                  
                  val eq_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                 end
               end
              
              val eq_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val lt_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val eqb :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
             end
            
            type coq_R_min_elt =
            | R_min_elt_0 of tree
            | R_min_elt_1 of tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_min_elt_2 of tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * elt option * coq_R_min_elt
            
            type coq_R_max_elt =
            | R_max_elt_0 of tree
            | R_max_elt_1 of tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_max_elt_2 of tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * elt option * coq_R_max_elt
            
            module L : 
             sig 
              module MO : 
               sig 
                module OrderTac : 
                 sig 
                  module OTF : 
                   sig 
                    type t =
                      ((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list
                    
                    val compare :
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      comparison
                    
                    val eq_dec :
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) -> bool
                   end
                  
                  module TO : 
                   sig 
                    type t =
                      ((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list
                    
                    val compare :
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      comparison
                    
                    val eq_dec :
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) ->
                      (((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list) -> bool
                   end
                 end
                
                val eq_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val lt_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val eqb :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
               end
             end
            
            val flatten_e : enumeration -> elt list
            
            type coq_R_bal =
            | R_bal_0 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
            | R_bal_1 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_2 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_3 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_4 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
            | R_bal_5 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_6 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_7 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_bal_8 of t
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * t
            
            type coq_R_remove_min =
            | R_remove_min_0 of tree * elt * t
            | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * (t * elt) * coq_R_remove_min * t * elt
            
            type coq_R_merge =
            | R_merge_0 of tree * tree
            | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * elt
            
            type coq_R_concat =
            | R_concat_0 of tree * tree
            | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * elt
            
            type coq_R_inter =
            | R_inter_0 of tree * tree
            | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * bool * t * tree * coq_R_inter * tree * coq_R_inter
            | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * bool * t * tree * coq_R_inter * tree * coq_R_inter
            
            type coq_R_diff =
            | R_diff_0 of tree * tree
            | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * bool * t * tree * coq_R_diff * tree * coq_R_diff
            | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * bool * t * tree * coq_R_diff * tree * coq_R_diff
            
            type coq_R_union =
            | R_union_0 of tree * tree
            | R_union_1 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
            | R_union_2 of tree * tree * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree
               * Int.Z_as_Int.t * tree
               * (((string_t, string_t * string_t) sum, (string_t * t',
                 (unit, string_t) sum) sum) sum * string_t list) * tree * 
               t * bool * t * tree * coq_R_union * tree * coq_R_union
           end
          
          module E : 
           sig 
            type t =
              ((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list
            
            val compare :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> comparison
            
            val eq_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
           end
          
          type elt =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
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
        
        type elt =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
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
          val eqb :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
        
        val min_elt : t -> elt option
        
        val max_elt : t -> elt option
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        module E : 
         sig 
          type t =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          val compare :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> (((string_t, string_t * string_t)
            sum, (string_t * t', (unit, string_t) sum) sum) sum * string_t
            list) OrderedType.coq_Compare
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
       end
      
      module X' : 
       sig 
        type t =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
        val eq_dec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
        
        val compare :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> comparison
       end
      
      module MSet : 
       sig 
        module Raw : 
         sig 
          type elt =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          type tree = PowLoc.SS.S.MSet.Raw.tree =
          | Leaf
          | Node of Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          
          val empty : tree
          
          val is_empty : tree -> bool
          
          val mem :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> bool
          
          val min_elt : tree -> elt option
          
          val max_elt : tree -> elt option
          
          val choose : tree -> elt option
          
          val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
          
          val elements_aux :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) list -> tree ->
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) list
          
          val elements :
            tree -> (((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) list
          
          val rev_elements_aux :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) list -> tree ->
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) list
          
          val rev_elements :
            tree -> (((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) list
          
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
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (enumeration ->
            comparison) -> enumeration -> comparison
          
          val compare_cont :
            tree -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_end : enumeration -> comparison
          
          val compare : tree -> tree -> comparison
          
          val equal : tree -> tree -> bool
          
          val subsetl :
            (tree -> bool) -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> tree -> bool
          
          val subsetr :
            (tree -> bool) -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> tree -> bool
          
          val subset : tree -> tree -> bool
          
          type t = tree
          
          val height : t -> Int.Z_as_Int.t
          
          val singleton :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree
          
          val create :
            t -> (((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
          
          val assert_false :
            t -> (((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
          
          val bal :
            t -> (((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) -> t -> tree
          
          val add :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> tree
          
          val join : tree -> elt -> t -> t
          
          val remove_min : tree -> elt -> t -> t * elt
          
          val merge : tree -> tree -> tree
          
          val remove :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> tree
          
          val concat : tree -> tree -> tree
          
          type triple = { t_left : t; t_in : bool; t_right : t }
          
          val t_left : triple -> t
          
          val t_in : triple -> bool
          
          val t_right : triple -> t
          
          val split :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> triple
          
          val inter : tree -> tree -> tree
          
          val diff : tree -> tree -> tree
          
          val union : tree -> tree -> tree
          
          val filter : (elt -> bool) -> tree -> tree
          
          val partition : (elt -> bool) -> t -> t * t
          
          val ltb_tree :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> bool
          
          val gtb_tree :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> tree -> bool
          
          val isok : tree -> bool
          
          module MX : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t =
                  ((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list
                
                val compare :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  comparison
                
                val eq_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
               end
              
              module TO : 
               sig 
                type t =
                  ((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list
                
                val compare :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  comparison
                
                val eq_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
               end
             end
            
            val eq_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val lt_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val eqb :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
           end
          
          type coq_R_min_elt =
          | R_min_elt_0 of tree
          | R_min_elt_1 of tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_min_elt_2 of tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * elt option
             * coq_R_min_elt
          
          type coq_R_max_elt =
          | R_max_elt_0 of tree
          | R_max_elt_1 of tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_max_elt_2 of tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * elt option
             * coq_R_max_elt
          
          module L : 
           sig 
            module MO : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                  
                  val compare :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    comparison
                  
                  val eq_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                 end
                
                module TO : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                  
                  val compare :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    comparison
                  
                  val eq_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                 end
               end
              
              val eq_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val lt_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val eqb :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
             end
           end
          
          val flatten_e : enumeration -> elt list
          
          type coq_R_bal =
          | R_bal_0 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t
          | R_bal_1 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_2 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_3 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_4 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t
          | R_bal_5 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_6 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_7 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t * Int.Z_as_Int.t
             * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_bal_8 of t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * t
          
          type coq_R_remove_min =
          | R_remove_min_0 of tree * elt * t
          | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * (t * elt)
             * coq_R_remove_min * t * elt
          
          type coq_R_merge =
          | R_merge_0 of tree * tree
          | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             elt
          
          type coq_R_concat =
          | R_concat_0 of tree * tree
          | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             elt
          
          type coq_R_inter =
          | R_inter_0 of tree * tree
          | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             bool * t * tree * coq_R_inter * tree * coq_R_inter
          | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             bool * t * tree * coq_R_inter * tree * coq_R_inter
          
          type coq_R_diff =
          | R_diff_0 of tree * tree
          | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             bool * t * tree * coq_R_diff * tree * coq_R_diff
          | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             bool * t * tree * coq_R_diff * tree * coq_R_diff
          
          type coq_R_union =
          | R_union_0 of tree * tree
          | R_union_1 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
          | R_union_2 of tree * tree * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree
             * Int.Z_as_Int.t * tree
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * tree * t * 
             bool * t * tree * coq_R_union * tree * coq_R_union
         end
        
        module E : 
         sig 
          type t =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          val compare :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> comparison
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
        
        type elt =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
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
      
      type elt =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
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
        val eqb :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
       end
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      module E : 
       sig 
        type t =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
        val compare :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> (((string_t, string_t * string_t) sum,
          (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
          OrderedType.coq_Compare
        
        val eq_dec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
       end
      
      module SF : 
       sig 
        val eqb :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
       end
      
      val choose_only : t -> elt option
      
      val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
      
      val cond_eq_rect :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
      
      val cond_eq_rec :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
     end
    
    type elt =
      ((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list
    
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
    
    val add :
      (((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list) -> t -> t
    
    val singleton :
      (((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list) -> t
    
    val mem : SS.elt -> SS.t -> bool
    
    val remove :
      (((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list) -> t -> t
    
    val union : SS.t -> SS.t -> SS.t
    
    val union_small_big : SS.t -> SS.t -> SS.t
    
    val intersect : SS.t -> SS.t -> SS.t
    
    val diff : SS.t -> SS.t -> SS.t
    
    val subset : SS.t -> SS.t -> bool
    
    val filter :
      ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
      string_t) sum) sum) sum * string_t list) -> bool) -> t -> t
    
    val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1
    
    val iter :
      ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
      string_t) sum) sum) sum * string_t list) -> unit) -> t -> unit
    
    val elements : SS.t -> SS.elt list
    
    val cardinal : SS.t -> int
    
    val choose : SS.t -> SS.elt option
    
    val choose_only : SS.t -> SS.elt option
    
    val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool
    
    val coq_ILat : t coq_TCLat
    
    val coq_ISet :
      (((string_t, string_t * string_t) sum, (string_t * t', (unit, string_t)
      sum) sum) sum * string_t list, t) coq_TCSet
   end
  
  module LocMap : 
   sig 
    module E : 
     sig 
      type t =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
      val compare :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> (((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
        OrderedType.coq_Compare
      
      val eq_dec :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
     end
    
    module Raw : 
     sig 
      type key =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
      type 'elt tree = 'elt LocMap.Raw.tree =
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
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> 'a1 tree -> bool
      
      val find :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1 option
      
      val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
      
      val remove_min :
        'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
      
      val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
      
      val remove :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1 tree
      
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
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1 triple
      
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
        ('a1 -> 'a1 -> bool) -> (((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum * string_t list) ->
        'a1 -> ('a1 enumeration -> bool) -> 'a1 enumeration -> bool
      
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
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree ->
        'a3 tree
      
      module Proofs : 
       sig 
        module MX : 
         sig 
          module TO : 
           sig 
            type t =
              ((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
          
          val lt_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
          
          val eqb :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
        
        module PX : 
         sig 
          module MO : 
           sig 
            module TO : 
             sig 
              type t =
                ((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val lt_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val eqb :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
           end
         end
        
        module L : 
         sig 
          module MX : 
           sig 
            module TO : 
             sig 
              type t =
                ((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list
             end
            
            module IsTO : 
             sig 
              
             end
            
            module OrderTac : 
             sig 
              
             end
            
            val eq_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val lt_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val eqb :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
           end
          
          module PX : 
           sig 
            module MO : 
             sig 
              module TO : 
               sig 
                type t =
                  ((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list
               end
              
              module IsTO : 
               sig 
                
               end
              
              module OrderTac : 
               sig 
                
               end
              
              val eq_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val lt_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val eqb :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
             end
           end
          
          type key =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          type 'elt t =
            ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) * 'elt) list
          
          val empty : 'a1 t
          
          val is_empty : 'a1 t -> bool
          
          val mem : key -> 'a1 t -> bool
          
          type 'elt coq_R_mem =
          | R_mem_0 of 'elt t
          | R_mem_1 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_mem_2 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_mem_3 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list * 
             bool * 'elt coq_R_mem
          
          val coq_R_mem_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> bool
            -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem
            -> 'a2
          
          val coq_R_mem_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> bool
            -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem
            -> 'a2
          
          val mem_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val mem_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
          
          val find : key -> 'a1 t -> 'a1 option
          
          type 'elt coq_R_find =
          | R_find_0 of 'elt t
          | R_find_1 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_find_2 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_find_3 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
             * 'elt option * 'elt coq_R_find
          
          val coq_R_find_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1
            option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option ->
            'a1 coq_R_find -> 'a2
          
          val coq_R_find_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1
            option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option ->
            'a1 coq_R_find -> 'a2
          
          val find_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val find_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val coq_R_find_correct :
            key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
          
          val add : key -> 'a1 -> 'a1 t -> 'a1 t
          
          type 'elt coq_R_add =
          | R_add_0 of 'elt t
          | R_add_1 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_add_2 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_add_3 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list * 
             'elt t * 'elt coq_R_add
          
          val coq_R_add_rect :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1 t
            -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
            coq_R_add -> 'a2
          
          val coq_R_add_rec :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1 t
            -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
            coq_R_add -> 'a2
          
          val add_rect :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val add_rec :
            key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val coq_R_add_correct :
            key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
          
          val remove : key -> 'a1 t -> 'a1 t
          
          type 'elt coq_R_remove =
          | R_remove_0 of 'elt t
          | R_remove_1 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_remove_2 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
          | R_remove_3 of 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list * 
             'elt t * 'elt coq_R_remove
          
          val coq_R_remove_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1 t
            -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
            coq_R_remove -> 'a2
          
          val coq_R_remove_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a1 t
            -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
            coq_R_remove -> 'a2
          
          val remove_rect :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val remove_rec :
            key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2)
            -> ('a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> __ -> __ -> 'a2) -> ('a1 t -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> 'a1 t -> 'a2
          
          val coq_R_remove_correct :
            key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
          
          val elements : 'a1 t -> 'a1 t
          
          val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
          
          type ('elt, 'a) coq_R_fold =
          | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
          | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list * 
             'a * ('elt, 'a) coq_R_fold
          
          val coq_R_fold_rect :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> ('a1, __)
            coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
            -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
          
          val coq_R_fold_rec :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> ('a1, __)
            coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
            -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
          
          val fold_rect :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> 'a2 -> 'a2) ->
            (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
          
          val fold_rec :
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
            (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> 'a2 -> 'a2) ->
            (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
          
          val coq_R_fold_correct :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
            coq_R_fold
          
          val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
          
          type 'elt coq_R_equal =
          | R_equal_0 of 'elt t * 'elt t
          | R_equal_1 of 'elt t * 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list * 
             bool * 'elt coq_R_equal
          | R_equal_2 of 'elt t * 'elt t
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt
             * ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list) * 'elt) list
             * (((string_t, string_t * string_t) sum, (string_t * t', (unit,
               string_t) sum) sum) sum * string_t list)
               OrderedType.coq_Compare
          | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
          
          val coq_R_equal_rect :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> bool
            -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
            'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
            'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
          
          val coq_R_equal_rec :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> bool
            -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
            'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
            'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
          
          val equal_rect :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
            'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
            'a2) -> 'a1 t -> 'a1 t -> 'a2
          
          val equal_rec :
            ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
            ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> __ -> __ -> 'a2 ->
            'a2) -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ((((string_t, string_t * string_t) sum, (string_t * t',
            (unit, string_t) sum) sum) sum * string_t list) * 'a1) list -> __
            -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) * 'a1) list -> __ -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
            'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
            'a2) -> 'a1 t -> 'a1 t -> 'a2
          
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
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3
            t
          
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
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
          'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
          coq_R_mem -> 'a2
        
        val coq_R_mem_rec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
          'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
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
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
          option -> 'a1 coq_R_find -> 'a2
        
        val coq_R_find_rec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
          option -> 'a1 coq_R_find -> 'a2
        
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
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
        
        val coq_R_bal_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2
        
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
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2
        
        val coq_R_add_rec :
          key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
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
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2
        
        val coq_R_remove_min_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
          key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2
        
        type 'elt coq_R_merge =
        | R_merge_0 of 'elt tree * 'elt tree
        | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt
        
        val coq_R_merge_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2
        
        val coq_R_merge_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2
        
        type 'elt coq_R_remove =
        | R_remove_0 of 'elt tree
        | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        
        val coq_R_remove_rect :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
          'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree
          -> 'a1 coq_R_remove -> 'a2
        
        val coq_R_remove_rec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
          'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree
          -> 'a1 coq_R_remove -> 'a2
        
        type 'elt coq_R_concat =
        | R_concat_0 of 'elt tree * 'elt tree
        | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
        
        val coq_R_concat_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
        
        val coq_R_concat_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2
        
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
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
          -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
          tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
          tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
          coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
          'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2
        
        val coq_R_split_rec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
          -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
          tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
          tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
          coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
          'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2
        
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
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3
        
        val coq_R_map_option_rec :
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3
        
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
           * 'elt' tree * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
           * 'elt'' tree * ('elt, 'elt', 'elt'') coq_R_map2_opt
        
        val coq_R_map2_opt_rect :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4
        
        val coq_R_map2_opt_rec :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4
        
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
        type t = E.t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
      
      val lt_dec :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
      
      val eqb :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
     end
    
    module O : 
     sig 
      module MO : 
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
        
        val eq_dec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
        
        val lt_dec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
        
        val eqb :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
       end
     end
    
    module P : 
     sig 
      module F : 
       sig 
        val eqb :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
        
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
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t -> 'a2
        -> __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ ->
        __ -> 'a3 -> 'a3) -> 'a3
      
      val fold_rec_nodep :
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1 ->
        'a2 -> __ -> 'a3 -> 'a3) -> 'a3
      
      val fold_rec_weak :
        (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __ ->
        'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3 ->
        'a3) -> 'a1 t -> 'a3
      
      val fold_rel :
        (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2 ->
        'a3 -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4 -> 'a4)
        -> 'a4
      
      val map_induction :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 -> __ ->
        __ -> 'a2) -> 'a1 t -> 'a2
      
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
      ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
      string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
      sum * string_t list) -> 'a1 -> __ -> __ -> 'a2) -> 'a1 t -> 'a2
    
    val map_induction_min :
      ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
      string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
      sum * string_t list) -> 'a1 -> __ -> __ -> 'a2) -> 'a1 t -> 'a2
    
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
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
        -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2
        -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool
        -> 'a1 coq_R_for_all -> 'a2
      
      val coq_R_for_all_rec :
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
        -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all -> 'a2
        -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree -> bool
        -> 'a1 coq_R_for_all -> 'a2
      
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
        -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
        Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1
        Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
        -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) ->
        ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key ->
        'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
        coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool -> 'a1
        coq_R_strong_le -> 'a2
      
      val coq_R_strong_le_rec :
        ('a1 -> 'a1 -> bool) -> ('a1 Raw.tree -> 'a1 Raw.tree -> __ -> 'a2)
        -> ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
        Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) -> ('a1
        Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
        Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_strong_le
        -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le -> 'a2 -> 'a2) ->
        ('a1 Raw.tree -> 'a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 ->
        'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> Raw.key ->
        'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1
        coq_R_strong_le -> 'a2 -> __ -> __ -> bool -> 'a1 coq_R_strong_le ->
        'a2 -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool -> 'a1
        coq_R_strong_le -> 'a2
      
      type 'elt coq_R_filter =
      | R_filter_0 of 'elt Raw.tree
      | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      
      val coq_R_filter_rect :
        (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t
        -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
        Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ ->
        'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
        coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2
      
      val coq_R_filter_rec :
        (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1 Raw.tree
        -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t
        -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1 Raw.tree -> 'a1
        Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree -> Int.Z_as_Int.t -> __ ->
        'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> 'a1 Raw.tree -> 'a1
        coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1 Raw.tree -> 'a1 Raw.tree ->
        'a1 coq_R_filter -> 'a2
     end
    
    val for_all :
      (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool
    
    val strong_le : ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool
    
    val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t
   end
  
  module ExtProcPos : 
   sig 
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
        type t = pos_t
       end
      
      module IsTO : 
       sig 
        
       end
      
      module OrderTac : 
       sig 
        
       end
      
      val eq_dec : pos_t -> pos_t -> bool
      
      val lt_dec : pos_t -> pos_t -> bool
      
      val eqb : pos_t -> pos_t -> bool
     end
    
    type t = string_t * pos_t
    
    val compare : t -> t -> (string_t * pos_t) OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
    
    val fst : ('a1 * 'a2) -> 'a1
    
    val snd : ('a1 * 'a2) -> 'a2
   end
  
  module PowExtProcPos : 
   sig 
    module A : 
     sig 
      type t = string_t * pos_t
      
      val compare :
        (string_t * pos_t) -> (string_t * pos_t) -> (string_t * pos_t)
        OrderedType.coq_Compare
      
      val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
     end
    
    module SS : 
     sig 
      module S : 
       sig 
        module X' : 
         sig 
          type t = string_t * pos_t
          
          val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
          
          val compare :
            (string_t * pos_t) -> (string_t * pos_t) -> comparison
         end
        
        module MSet : 
         sig 
          module Raw : 
           sig 
            type elt = string_t * pos_t
            
            type tree = PowExtProcPos.SS.S.MSet.Raw.tree =
            | Leaf
            | Node of Int.Z_as_Int.t * tree * (string_t * pos_t) * tree
            
            val empty : tree
            
            val is_empty : tree -> bool
            
            val mem : (string_t * pos_t) -> tree -> bool
            
            val min_elt : tree -> elt option
            
            val max_elt : tree -> elt option
            
            val choose : tree -> elt option
            
            val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
            
            val elements_aux :
              (string_t * pos_t) list -> tree -> (string_t * pos_t) list
            
            val elements : tree -> (string_t * pos_t) list
            
            val rev_elements_aux :
              (string_t * pos_t) list -> tree -> (string_t * pos_t) list
            
            val rev_elements : tree -> (string_t * pos_t) list
            
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
              (string_t * pos_t) -> (enumeration -> comparison) ->
              enumeration -> comparison
            
            val compare_cont :
              tree -> (enumeration -> comparison) -> enumeration ->
              comparison
            
            val compare_end : enumeration -> comparison
            
            val compare : tree -> tree -> comparison
            
            val equal : tree -> tree -> bool
            
            val subsetl :
              (tree -> bool) -> (string_t * pos_t) -> tree -> bool
            
            val subsetr :
              (tree -> bool) -> (string_t * pos_t) -> tree -> bool
            
            val subset : tree -> tree -> bool
            
            type t = tree
            
            val height : t -> Int.Z_as_Int.t
            
            val singleton : (string_t * pos_t) -> tree
            
            val create : t -> (string_t * pos_t) -> t -> tree
            
            val assert_false : t -> (string_t * pos_t) -> t -> tree
            
            val bal : t -> (string_t * pos_t) -> t -> tree
            
            val add : (string_t * pos_t) -> tree -> tree
            
            val join : tree -> elt -> t -> t
            
            val remove_min : tree -> elt -> t -> t * elt
            
            val merge : tree -> tree -> tree
            
            val remove : (string_t * pos_t) -> tree -> tree
            
            val concat : tree -> tree -> tree
            
            type triple = { t_left : t; t_in : bool; t_right : t }
            
            val t_left : triple -> t
            
            val t_in : triple -> bool
            
            val t_right : triple -> t
            
            val split : (string_t * pos_t) -> tree -> triple
            
            val inter : tree -> tree -> tree
            
            val diff : tree -> tree -> tree
            
            val union : tree -> tree -> tree
            
            val filter : (elt -> bool) -> tree -> tree
            
            val partition : (elt -> bool) -> t -> t * t
            
            val ltb_tree : (string_t * pos_t) -> tree -> bool
            
            val gtb_tree : (string_t * pos_t) -> tree -> bool
            
            val isok : tree -> bool
            
            module MX : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = string_t * pos_t
                  
                  val compare :
                    (string_t * pos_t) -> (string_t * pos_t) -> comparison
                  
                  val eq_dec :
                    (string_t * pos_t) -> (string_t * pos_t) -> bool
                 end
                
                module TO : 
                 sig 
                  type t = string_t * pos_t
                  
                  val compare :
                    (string_t * pos_t) -> (string_t * pos_t) -> comparison
                  
                  val eq_dec :
                    (string_t * pos_t) -> (string_t * pos_t) -> bool
                 end
               end
              
              val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
              
              val lt_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
              
              val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
             end
            
            type coq_R_min_elt =
            | R_min_elt_0 of tree
            | R_min_elt_1 of tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_min_elt_2 of tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * elt option * coq_R_min_elt
            
            type coq_R_max_elt =
            | R_max_elt_0 of tree
            | R_max_elt_1 of tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_max_elt_2 of tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * elt option * coq_R_max_elt
            
            module L : 
             sig 
              module MO : 
               sig 
                module OrderTac : 
                 sig 
                  module OTF : 
                   sig 
                    type t = string_t * pos_t
                    
                    val compare :
                      (string_t * pos_t) -> (string_t * pos_t) -> comparison
                    
                    val eq_dec :
                      (string_t * pos_t) -> (string_t * pos_t) -> bool
                   end
                  
                  module TO : 
                   sig 
                    type t = string_t * pos_t
                    
                    val compare :
                      (string_t * pos_t) -> (string_t * pos_t) -> comparison
                    
                    val eq_dec :
                      (string_t * pos_t) -> (string_t * pos_t) -> bool
                   end
                 end
                
                val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
                
                val lt_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
                
                val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
               end
             end
            
            val flatten_e : enumeration -> elt list
            
            type coq_R_bal =
            | R_bal_0 of t * (string_t * pos_t) * t
            | R_bal_1 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree
            | R_bal_2 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree
            | R_bal_3 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_bal_4 of t * (string_t * pos_t) * t
            | R_bal_5 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree
            | R_bal_6 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree
            | R_bal_7 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
               tree * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_bal_8 of t * (string_t * pos_t) * t
            
            type coq_R_remove_min =
            | R_remove_min_0 of tree * elt * t
            | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * (t * elt) * coq_R_remove_min * 
               t * elt
            
            type coq_R_merge =
            | R_merge_0 of tree * tree
            | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * elt
            
            type coq_R_concat =
            | R_concat_0 of tree * tree
            | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * elt
            
            type coq_R_inter =
            | R_inter_0 of tree * tree
            | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * bool * t * tree
               * coq_R_inter * tree * coq_R_inter
            | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * bool * t * tree
               * coq_R_inter * tree * coq_R_inter
            
            type coq_R_diff =
            | R_diff_0 of tree * tree
            | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_diff
               * tree * coq_R_diff
            | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_diff
               * tree * coq_R_diff
            
            type coq_R_union =
            | R_union_0 of tree * tree
            | R_union_1 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree
            | R_union_2 of tree * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
               * (string_t * pos_t) * tree * t * bool * t * tree
               * coq_R_union * tree * coq_R_union
           end
          
          module E : 
           sig 
            type t = string_t * pos_t
            
            val compare :
              (string_t * pos_t) -> (string_t * pos_t) -> comparison
            
            val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
           end
          
          type elt = string_t * pos_t
          
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
        
        type elt = string_t * pos_t
        
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
          val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
         end
        
        val min_elt : t -> elt option
        
        val max_elt : t -> elt option
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        module E : 
         sig 
          type t = string_t * pos_t
          
          val compare :
            (string_t * pos_t) -> (string_t * pos_t) -> (string_t * pos_t)
            OrderedType.coq_Compare
          
          val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
         end
       end
      
      module X' : 
       sig 
        type t = string_t * pos_t
        
        val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
        
        val compare : (string_t * pos_t) -> (string_t * pos_t) -> comparison
       end
      
      module MSet : 
       sig 
        module Raw : 
         sig 
          type elt = string_t * pos_t
          
          type tree = PowExtProcPos.SS.S.MSet.Raw.tree =
          | Leaf
          | Node of Int.Z_as_Int.t * tree * (string_t * pos_t) * tree
          
          val empty : tree
          
          val is_empty : tree -> bool
          
          val mem : (string_t * pos_t) -> tree -> bool
          
          val min_elt : tree -> elt option
          
          val max_elt : tree -> elt option
          
          val choose : tree -> elt option
          
          val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
          
          val elements_aux :
            (string_t * pos_t) list -> tree -> (string_t * pos_t) list
          
          val elements : tree -> (string_t * pos_t) list
          
          val rev_elements_aux :
            (string_t * pos_t) list -> tree -> (string_t * pos_t) list
          
          val rev_elements : tree -> (string_t * pos_t) list
          
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
            (string_t * pos_t) -> (enumeration -> comparison) -> enumeration
            -> comparison
          
          val compare_cont :
            tree -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_end : enumeration -> comparison
          
          val compare : tree -> tree -> comparison
          
          val equal : tree -> tree -> bool
          
          val subsetl : (tree -> bool) -> (string_t * pos_t) -> tree -> bool
          
          val subsetr : (tree -> bool) -> (string_t * pos_t) -> tree -> bool
          
          val subset : tree -> tree -> bool
          
          type t = tree
          
          val height : t -> Int.Z_as_Int.t
          
          val singleton : (string_t * pos_t) -> tree
          
          val create : t -> (string_t * pos_t) -> t -> tree
          
          val assert_false : t -> (string_t * pos_t) -> t -> tree
          
          val bal : t -> (string_t * pos_t) -> t -> tree
          
          val add : (string_t * pos_t) -> tree -> tree
          
          val join : tree -> elt -> t -> t
          
          val remove_min : tree -> elt -> t -> t * elt
          
          val merge : tree -> tree -> tree
          
          val remove : (string_t * pos_t) -> tree -> tree
          
          val concat : tree -> tree -> tree
          
          type triple = { t_left : t; t_in : bool; t_right : t }
          
          val t_left : triple -> t
          
          val t_in : triple -> bool
          
          val t_right : triple -> t
          
          val split : (string_t * pos_t) -> tree -> triple
          
          val inter : tree -> tree -> tree
          
          val diff : tree -> tree -> tree
          
          val union : tree -> tree -> tree
          
          val filter : (elt -> bool) -> tree -> tree
          
          val partition : (elt -> bool) -> t -> t * t
          
          val ltb_tree : (string_t * pos_t) -> tree -> bool
          
          val gtb_tree : (string_t * pos_t) -> tree -> bool
          
          val isok : tree -> bool
          
          module MX : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t = string_t * pos_t
                
                val compare :
                  (string_t * pos_t) -> (string_t * pos_t) -> comparison
                
                val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
               end
              
              module TO : 
               sig 
                type t = string_t * pos_t
                
                val compare :
                  (string_t * pos_t) -> (string_t * pos_t) -> comparison
                
                val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
               end
             end
            
            val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
            
            val lt_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
            
            val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
           end
          
          type coq_R_min_elt =
          | R_min_elt_0 of tree
          | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * (string_t * pos_t)
             * tree
          | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * (string_t * pos_t)
             * tree * Int.Z_as_Int.t * tree * (string_t * pos_t) * tree
             * elt option * coq_R_min_elt
          
          type coq_R_max_elt =
          | R_max_elt_0 of tree
          | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * (string_t * pos_t)
             * tree
          | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * (string_t * pos_t)
             * tree * Int.Z_as_Int.t * tree * (string_t * pos_t) * tree
             * elt option * coq_R_max_elt
          
          module L : 
           sig 
            module MO : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = string_t * pos_t
                  
                  val compare :
                    (string_t * pos_t) -> (string_t * pos_t) -> comparison
                  
                  val eq_dec :
                    (string_t * pos_t) -> (string_t * pos_t) -> bool
                 end
                
                module TO : 
                 sig 
                  type t = string_t * pos_t
                  
                  val compare :
                    (string_t * pos_t) -> (string_t * pos_t) -> comparison
                  
                  val eq_dec :
                    (string_t * pos_t) -> (string_t * pos_t) -> bool
                 end
               end
              
              val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
              
              val lt_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
              
              val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
             end
           end
          
          val flatten_e : enumeration -> elt list
          
          type coq_R_bal =
          | R_bal_0 of t * (string_t * pos_t) * t
          | R_bal_1 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree
          | R_bal_2 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree
          | R_bal_3 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_bal_4 of t * (string_t * pos_t) * t
          | R_bal_5 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree
          | R_bal_6 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree
          | R_bal_7 of t * (string_t * pos_t) * t * Int.Z_as_Int.t * 
             tree * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_bal_8 of t * (string_t * pos_t) * t
          
          type coq_R_remove_min =
          | R_remove_min_0 of tree * elt * t
          | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * (t * elt) * coq_R_remove_min * 
             t * elt
          
          type coq_R_merge =
          | R_merge_0 of tree * tree
          | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * elt
          
          type coq_R_concat =
          | R_concat_0 of tree * tree
          | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * elt
          
          type coq_R_inter =
          | R_inter_0 of tree * tree
          | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_inter
             * tree * coq_R_inter
          | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_inter
             * tree * coq_R_inter
          
          type coq_R_diff =
          | R_diff_0 of tree * tree
          | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_diff
             * tree * coq_R_diff
          | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_diff
             * tree * coq_R_diff
          
          type coq_R_union =
          | R_union_0 of tree * tree
          | R_union_1 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree
          | R_union_2 of tree * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * Int.Z_as_Int.t * tree
             * (string_t * pos_t) * tree * t * bool * t * tree * coq_R_union
             * tree * coq_R_union
         end
        
        module E : 
         sig 
          type t = string_t * pos_t
          
          val compare :
            (string_t * pos_t) -> (string_t * pos_t) -> comparison
          
          val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
         end
        
        type elt = string_t * pos_t
        
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
      
      type elt = string_t * pos_t
      
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
        val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
       end
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      module E : 
       sig 
        type t = string_t * pos_t
        
        val compare :
          (string_t * pos_t) -> (string_t * pos_t) -> (string_t * pos_t)
          OrderedType.coq_Compare
        
        val eq_dec : (string_t * pos_t) -> (string_t * pos_t) -> bool
       end
      
      module SF : 
       sig 
        val eqb : (string_t * pos_t) -> (string_t * pos_t) -> bool
       end
      
      val choose_only : t -> elt option
      
      val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
      
      val cond_eq_rect :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
      
      val cond_eq_rec :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
     end
    
    type elt = string_t * pos_t
    
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
    
    val add : (string_t * pos_t) -> t -> t
    
    val singleton : (string_t * pos_t) -> t
    
    val mem : SS.elt -> SS.t -> bool
    
    val remove : (string_t * pos_t) -> t -> t
    
    val union : SS.t -> SS.t -> SS.t
    
    val union_small_big : SS.t -> SS.t -> SS.t
    
    val intersect : SS.t -> SS.t -> SS.t
    
    val diff : SS.t -> SS.t -> SS.t
    
    val subset : SS.t -> SS.t -> bool
    
    val filter : ((string_t * pos_t) -> bool) -> t -> t
    
    val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1
    
    val iter : ((string_t * pos_t) -> unit) -> t -> unit
    
    val elements : SS.t -> SS.elt list
    
    val cardinal : SS.t -> int
    
    val choose : SS.t -> SS.elt option
    
    val choose_only : SS.t -> SS.elt option
    
    val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool
    
    val coq_ILat : t coq_TCLat
    
    val coq_ISet : (string_t * pos_t, t) coq_TCSet
   end
  
  module Dump : 
   sig 
    module A : 
     sig 
      type t = string_t
      
      val compare : string_t -> string_t -> string_t OrderedType.coq_Compare
      
      val eq_dec : string_t -> string_t -> bool
     end
    
    module B : 
     sig 
      type t = PowLoc.t
      
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
        type t = string_t
        
        val compare :
          string_t -> string_t -> string_t OrderedType.coq_Compare
        
        val eq_dec : string_t -> string_t -> bool
       end
      
      module Raw : 
       sig 
        type key = string_t
        
        type 'elt tree = 'elt Dump.M.Raw.tree =
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
        
        val mem : string_t -> 'a1 tree -> bool
        
        val find : string_t -> 'a1 tree -> 'a1 option
        
        val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
        
        val remove_min :
          'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
        
        val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
        
        val remove : string_t -> 'a1 tree -> 'a1 tree
        
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
        
        val split : string_t -> 'a1 tree -> 'a1 triple
        
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
          ('a1 -> 'a1 -> bool) -> string_t -> 'a1 -> ('a1 enumeration ->
          bool) -> 'a1 enumeration -> bool
        
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
          
          module PX : 
           sig 
            module MO : 
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
           end
          
          module L : 
           sig 
            module MX : 
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
            
            module PX : 
             sig 
              module MO : 
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
             end
            
            type key = string_t
            
            type 'elt t = (string_t * 'elt) list
            
            val empty : 'a1 t
            
            val is_empty : 'a1 t -> bool
            
            val mem : key -> 'a1 t -> bool
            
            type 'elt coq_R_mem =
            | R_mem_0 of 'elt t
            | R_mem_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_mem_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_mem_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
               * bool * 'elt coq_R_mem
            
            val coq_R_mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t
              -> bool -> 'a1 coq_R_mem -> 'a2
            
            val coq_R_mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t
              -> bool -> 'a1 coq_R_mem -> 'a2
            
            val mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
            
            val find : key -> 'a1 t -> 'a1 option
            
            type 'elt coq_R_find =
            | R_find_0 of 'elt t
            | R_find_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_find_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_find_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
               * 'elt option * 'elt coq_R_find
            
            val coq_R_find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2)
              -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
            
            val coq_R_find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2)
              -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
            
            val find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val coq_R_find_correct :
              key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
            
            val add : key -> 'a1 -> 'a1 t -> 'a1 t
            
            type 'elt coq_R_add =
            | R_add_0 of 'elt t
            | R_add_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_add_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_add_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
               * 'elt t * 'elt coq_R_add
            
            val coq_R_add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
              -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1
              t -> 'a1 t -> 'a1 coq_R_add -> 'a2
            
            val coq_R_add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
              -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1
              t -> 'a1 t -> 'a1 coq_R_add -> 'a2
            
            val add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
              -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1
              -> (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val coq_R_add_correct :
              key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
            
            val remove : key -> 'a1 t -> 'a1 t
            
            type 'elt coq_R_remove =
            | R_remove_0 of 'elt t
            | R_remove_1 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_remove_2 of 'elt t * string_t * 'elt * (string_t * 'elt) list
            | R_remove_3 of 'elt t * string_t * 'elt * (string_t * 'elt) list
               * 'elt t * 'elt coq_R_remove
            
            val coq_R_remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) ->
              'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
            
            val coq_R_remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) ->
              'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
            
            val remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ -> __ ->
              'a2) -> ('a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
            
            val coq_R_remove_correct :
              key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
            
            val elements : 'a1 t -> 'a1 t
            
            val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
            
            type ('elt, 'a) coq_R_fold =
            | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
            | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * string_t
               * 'elt * (string_t * 'elt) list * 'a * ('elt, 'a) coq_R_fold
            
            val coq_R_fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t ->
              'a1 -> (string_t * 'a1) list -> __ -> __ -> ('a1, __)
              coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1
              t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
            
            val coq_R_fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t ->
              'a1 -> (string_t * 'a1) list -> __ -> __ -> ('a1, __)
              coq_R_fold -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1
              t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold -> 'a2
            
            val fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t ->
              'a1 -> (string_t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key ->
              'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
            
            val fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> string_t ->
              'a1 -> (string_t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key ->
              'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a2
            
            val coq_R_fold_correct :
              (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
              coq_R_fold
            
            val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
            
            type 'elt coq_R_equal =
            | R_equal_0 of 'elt t * 'elt t
            | R_equal_1 of 'elt t * 'elt t * string_t * 'elt
               * (string_t * 'elt) list * string_t * 'elt
               * (string_t * 'elt) list * bool * 'elt coq_R_equal
            | R_equal_2 of 'elt t * 'elt t * string_t * 'elt
               * (string_t * 'elt) list * string_t * 'elt
               * (string_t * 'elt) list * string_t OrderedType.coq_Compare
            | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
            
            val coq_R_equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ ->
              __ -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
              -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> string_t
              -> 'a1 -> (string_t * 'a1) list -> __ -> string_t
              OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t
              -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t
              -> bool -> 'a1 coq_R_equal -> 'a2
            
            val coq_R_equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ ->
              __ -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
              -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> string_t
              -> 'a1 -> (string_t * 'a1) list -> __ -> string_t
              OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t
              -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t
              -> bool -> 'a1 coq_R_equal -> 'a2
            
            val equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ ->
              __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> string_t OrderedType.coq_Compare
              -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
              -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
            
            val equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> string_t -> 'a1 -> (string_t * 'a1) list ->
              __ -> string_t -> 'a1 -> (string_t * 'a1) list -> __ -> __ ->
              __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> string_t -> 'a1 ->
              (string_t * 'a1) list -> __ -> string_t OrderedType.coq_Compare
              -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
              -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2
            
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
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
            -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2
          
          val coq_R_mem_rec :
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
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
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
            -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2
          
          val coq_R_find_rec :
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
            -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2
          
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
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
            -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
          
          val coq_R_remove_rec :
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree
            -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2
          
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
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
            tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
            coq_R_split -> 'a2
          
          val coq_R_split_rec :
            string_t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1
            tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
            coq_R_split -> 'a2
          
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
          type t = E.t
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
      
      module O : 
       sig 
        module MO : 
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
          
          val eq_dec : string_t -> string_t -> bool
          
          val lt_dec : string_t -> string_t -> bool
          
          val eqb : string_t -> string_t -> bool
         end
       end
      
      module P : 
       sig 
        module F : 
         sig 
          val eqb : string_t -> string_t -> bool
          
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
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> string_t -> 'a1 ->
        __ -> __ -> 'a2) -> 'a1 t -> 'a2
      
      val map_induction_min :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> string_t -> 'a1 ->
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
    
    type t = PowLoc.t M.t
    
    val empty : t
    
    val is_empty : t -> bool
    
    val find : string_t -> t -> PowLoc.t
    
    val add : string_t -> PowLoc.t -> t -> t
    
    val weak_add : string_t -> PowLoc.t -> t -> t
    
    val fast_weak_add : string_t -> PowLoc.t -> t -> t
    
    val remove : string_t -> t -> t
    
    val map : (PowLoc.t -> PowLoc.t) -> t -> t
    
    val mapi : (string_t -> PowLoc.t -> PowLoc.t) -> t -> t
    
    val fold : (PowLoc.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val foldi : (string_t -> PowLoc.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val filteri : (string_t -> PowLoc.t -> bool) -> t -> t
    
    val elements : t -> (string_t * PowLoc.t) list
    
    val cardinal : t -> int
    
    val le_than : t -> string_t -> PowLoc.t -> bool
    
    val for_all :
      (string_t -> PowLoc.t -> unit) -> (string_t -> PowLoc.t -> bool) -> t
      -> bool
    
    val unstables :
      t -> t -> (PowLoc.t -> PowLoc.t -> bool) -> string_t list ->
      ((string_t * PowLoc.t) * PowLoc.t) list
    
    val meet_big_small : t -> t -> t
    
    val le_dec : t -> t -> bool
    
    val strong_le : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join' : PowLoc.t option -> PowLoc.t option -> PowLoc.t option
    
    val join : t -> t -> t
    
    val meet' : PowLoc.t option -> PowLoc.t option -> PowLoc.t option
    
    val meet : t -> t -> t
    
    val widen' : PowLoc.t option -> PowLoc.t option -> PowLoc.t option
    
    val widen : t -> t -> t
    
    val narrow' : PowLoc.t option -> PowLoc.t option -> PowLoc.t option
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
    
    val coq_IMap : (string_t, PowLoc.t, t) coq_TCMap
    
    val remove_node : InterNode.t -> t -> t
    
    val powloc_remove_function : pid_t -> PowLoc.t -> PowLoc.t
    
    val remove_function : pid_t -> t -> t
   end
  
  val var_of_gvar : GVar.t -> Var.t
  
  val var_of_lvar : LVar.t -> Var.t
  
  val classify_loc : Loc.t -> loc_type
  
  val loc_of_var : Var.t -> Loc.t
  
  val loc_of_allocsite : Allocsite.t -> Loc.t
  
  val proc_of_allocsite : Allocsite.t -> Proc.t option
  
  val append_field : Loc.t -> Field.t -> Loc.t
  
  val pow_loc_append_field : PowLoc.t -> Field.t -> PowLoc.t
  
  val allocsite_of_node : InterNode.t -> Allocsite.t
  
  val allocsite_of_ext : Proc.t option -> Allocsite.t
  
  module Val : 
   sig 
    module E2 : 
     sig 
      type t = DomBasic.PowExtProcPos.t * DomBasic.PowLoc.t
      
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
      type t = E2.t * ArrayBlk.t
      
      val le_dec : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join : t -> t -> t
      
      val meet : t -> t -> t
      
      val widen : t -> t -> t
      
      val narrow : t -> t -> t
      
      val coq_ILat : t coq_TCLat
     end
    
    type t = E3.t * DomBasic.PowProc.t
    
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
  
  val pow_proc_pos_of_val : Val.t -> DomBasic.PowExtProcPos.t
  
  val pow_loc_of_val : Val.t -> DomBasic.PowLoc.t
  
  val array_of_val : Val.t -> ArrayBlk.t
  
  val pow_proc_of_val : Val.t -> DomBasic.PowProc.t
  
  val val_of_pow_proc_pos : DomBasic.PowExtProcPos.t -> Val.t
  
  val val_of_pow_loc : DomBasic.PowLoc.t -> Val.t
  
  val val_of_array : ArrayBlk.t -> Val.t
  
  val val_of_pow_proc : DomBasic.PowProc.t -> Val.t
  
  val modify_pow_proc_pos : Val.t -> DomBasic.PowExtProcPos.t -> Val.t
  
  val modify_pow_loc : Val.t -> DomBasic.PowLoc.t -> Val.t
  
  val modify_array : Val.t -> ArrayBlk.t -> Val.t
  
  val modify_pow_proc : Val.t -> DomBasic.PowProc.t -> Val.t
  
  val coq_Var_g : DomCon.Var.t -> DomBasic.Var.t
  
  val coq_VarAlloc_g : VarRegion.t -> DomBasic.VarAllocsite.t
  
  val coq_Loc_g : DomCon.Loc.t -> DomBasic.Loc.t
  
  val coq_Val_g'_rect : val_t -> DomAbs.Val.t -> 'a1
  
  val coq_Val_g'_rec : val_t -> DomAbs.Val.t -> 'a1
  
  module Acc : 
   sig 
    type t = DomBasic.PowLoc.t * DomBasic.PowLoc.t
    
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
    
    val defof : t -> DomBasic.PowLoc.t
    
    val useof : t -> DomBasic.PowLoc.t
    
    val accessof : t -> DomBasic.PowLoc.t
    
    val empty : t
    
    val add : access_mode -> DomBasic.PowLoc.elt -> t -> t
    
    val add_set : access_mode -> DomBasic.PowLoc.t -> t -> t
    
    val singleton : access_mode -> DomBasic.PowLoc.elt -> t
    
    val from_set : access_mode -> DomBasic.PowLoc.t -> t
    
    val mem : DomBasic.PowLoc.elt -> t -> bool
    
    val remove : DomBasic.PowLoc.elt -> t -> t
    
    val remove_set : DomBasic.PowLoc.t -> t -> t
    
    val add_list : access_mode -> DomBasic.PowLoc.elt list -> t -> t
    
    val union : t -> t -> t
    
    val restrict : DomBasic.PowLoc.t -> t -> t
    
    val filter_out : DomBasic.PowLoc.t -> t -> t
   end
  
  module Mem : 
   sig 
    module A : 
     sig 
      type t =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
      val compare :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> (((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
        OrderedType.coq_Compare
      
      val eq_dec :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) -> bool
     end
    
    module B : 
     sig 
      type t = DomAbs.Val.t
      
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
        type t =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
        val compare :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> (((string_t, string_t * string_t) sum,
          (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
          OrderedType.coq_Compare
        
        val eq_dec :
          (((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list) -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> bool
       end
      
      module B : 
       sig 
        type t = DomAbs.Val.t
        
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
          type t =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          val compare :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> (((string_t, string_t * string_t)
            sum, (string_t * t', (unit, string_t) sum) sum) sum * string_t
            list) OrderedType.coq_Compare
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
        
        module Raw : 
         sig 
          type key =
            ((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list
          
          type 'elt tree = 'elt Mem.Mem.M.Raw.tree =
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
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 tree -> bool
          
          val find :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1
            option
          
          val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val add : key -> 'a1 -> 'a1 tree -> 'a1 tree
          
          val remove_min :
            'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)
          
          val merge : 'a1 tree -> 'a1 tree -> 'a1 tree
          
          val remove :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1 tree
          
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
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> 'a1 tree -> 'a1
            triple
          
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
            ('a1 -> 'a1 -> bool) -> (((string_t, string_t * string_t) sum,
            (string_t * t', (unit, string_t) sum) sum) sum * string_t list)
            -> 'a1 -> ('a1 enumeration -> bool) -> 'a1 enumeration -> bool
          
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
                type t =
                  ((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list
               end
              
              module IsTO : 
               sig 
                
               end
              
              module OrderTac : 
               sig 
                
               end
              
              val eq_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val lt_dec :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
              
              val eqb :
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> bool
             end
            
            module PX : 
             sig 
              module MO : 
               sig 
                module TO : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                 end
                
                module IsTO : 
                 sig 
                  
                 end
                
                module OrderTac : 
                 sig 
                  
                 end
                
                val eq_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val lt_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val eqb :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
               end
             end
            
            module L : 
             sig 
              module MX : 
               sig 
                module TO : 
                 sig 
                  type t =
                    ((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list
                 end
                
                module IsTO : 
                 sig 
                  
                 end
                
                module OrderTac : 
                 sig 
                  
                 end
                
                val eq_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val lt_dec :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
                
                val eqb :
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) ->
                  (((string_t, string_t * string_t) sum, (string_t * t',
                  (unit, string_t) sum) sum) sum * string_t list) -> bool
               end
              
              module PX : 
               sig 
                module MO : 
                 sig 
                  module TO : 
                   sig 
                    type t =
                      ((string_t, string_t * string_t) sum, (string_t * t',
                      (unit, string_t) sum) sum) sum * string_t list
                   end
                  
                  module IsTO : 
                   sig 
                    
                   end
                  
                  module OrderTac : 
                   sig 
                    
                   end
                  
                  val eq_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                  
                  val lt_dec :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                  
                  val eqb :
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) ->
                    (((string_t, string_t * string_t) sum, (string_t * t',
                    (unit, string_t) sum) sum) sum * string_t list) -> bool
                 end
               end
              
              type key =
                ((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list
              
              type 'elt t =
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'elt) list
              
              val empty : 'a1 t
              
              val is_empty : 'a1 t -> bool
              
              val mem : key -> 'a1 t -> bool
              
              type 'elt coq_R_mem =
              | R_mem_0 of 'elt t
              | R_mem_1 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_mem_2 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_mem_3 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * bool * 'elt coq_R_mem
              
              val coq_R_mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
                'a1 t -> bool -> 'a1 coq_R_mem -> 'a2
              
              val coq_R_mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
                'a1 t -> bool -> 'a1 coq_R_mem -> 'a2
              
              val mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem
              
              val find : key -> 'a1 t -> 'a1 option
              
              type 'elt coq_R_find =
              | R_find_0 of 'elt t
              | R_find_1 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_find_2 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_find_3 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * 'elt option * 'elt coq_R_find
              
              val coq_R_find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
                'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
              
              val coq_R_find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
                'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2
              
              val find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_find_correct :
                key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find
              
              val add : key -> 'a1 -> 'a1 t -> 'a1 t
              
              type 'elt coq_R_add =
              | R_add_0 of 'elt t
              | R_add_1 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_add_2 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_add_3 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * 'elt t * 'elt coq_R_add
              
              val coq_R_add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) ->
                'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2
              
              val coq_R_add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) ->
                'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2
              
              val add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_add_correct :
                key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
              
              val remove : key -> 'a1 t -> 'a1 t
              
              type 'elt coq_R_remove =
              | R_remove_0 of 'elt t
              | R_remove_1 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_remove_2 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
              | R_remove_3 of 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * 'elt t * 'elt coq_R_remove
              
              val coq_R_remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2)
                -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
              
              val coq_R_remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2)
                -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2
              
              val remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2
              
              val coq_R_remove_correct :
                key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove
              
              val elements : 'a1 t -> 'a1 t
              
              val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
              
              type ('elt, 'a) coq_R_fold =
              | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
              | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * 'a * ('elt, 'a) coq_R_fold
              
              val coq_R_fold_rect :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key ->
                'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3)
                coq_R_fold -> 'a2
              
              val coq_R_fold_rec :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key ->
                'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3)
                coq_R_fold -> 'a2
              
              val fold_rect :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
                -> 'a3 -> 'a2
              
              val fold_rec :
                (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2)
                -> (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
                -> 'a3 -> 'a2
              
              val coq_R_fold_correct :
                (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1,
                'a2) coq_R_fold
              
              val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
              
              type 'elt coq_R_equal =
              | R_equal_0 of 'elt t * 'elt t
              | R_equal_1 of 'elt t * 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list * bool * 'elt coq_R_equal
              | R_equal_2 of 'elt t * 'elt t
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 
                 'elt
                 * ((((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list) * 'elt)
                   list
                 * (((string_t, string_t * string_t) sum, (string_t * t',
                   (unit, string_t) sum) sum) sum * string_t list)
                   OrderedType.coq_Compare
              | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
              
              val coq_R_equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
                -> (((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
                'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __
                -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
              
              val coq_R_equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
                -> (((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) OrderedType.coq_Compare -> __ -> __ ->
                'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __
                -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2
              
              val equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t
                -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t
                -> 'a1 t -> 'a2
              
              val equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) -> 'a1 -> ((((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) * 'a1) list -> __ -> (((string_t, string_t * string_t)
                sum, (string_t * t', (unit, string_t) sum) sum)
                sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> (((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) -> 'a1 -> ((((string_t,
                string_t * string_t) sum, (string_t * t', (unit, string_t)
                sum) sum) sum * string_t list) * 'a1) list -> __ ->
                (((string_t, string_t * string_t) sum, (string_t * t', (unit,
                string_t) sum) sum) sum * string_t list) -> 'a1 ->
                ((((string_t, string_t * string_t) sum, (string_t * t',
                (unit, string_t) sum) sum) sum * string_t list) * 'a1) list
                -> __ -> (((string_t, string_t * string_t) sum,
                (string_t * t', (unit, string_t) sum) sum) sum * string_t
                list) OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t
                -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t
                -> 'a1 t -> 'a2
              
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
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem ->
              'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool
              -> 'a1 coq_R_mem -> 'a2
            
            val coq_R_mem_rec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem ->
              'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool
              -> 'a1 coq_R_mem -> 'a2
            
            type 'elt coq_R_find =
            | R_find_0 of 'elt tree
            | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
            | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t
            | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
            
            val coq_R_find_rect :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
              coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
              coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1
              coq_R_find -> 'a2
            
            val coq_R_find_rec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
              coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
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
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2
            
            val coq_R_remove_rec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
              coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
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
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
              option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple ->
              'a1 coq_R_split -> 'a2
            
            val coq_R_split_rec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> ('a1 tree -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
              __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
              option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple ->
              'a1 coq_R_split -> 'a2
            
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
        
        type key =
          ((string_t, string_t * string_t) sum, (string_t * t', (unit,
          string_t) sum) sum) sum * string_t list
        
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
            type t = E.t
           end
          
          module IsTO : 
           sig 
            
           end
          
          module OrderTac : 
           sig 
            
           end
          
          val eq_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
          
          val lt_dec :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
          
          val eqb :
            (((string_t, string_t * string_t) sum, (string_t * t', (unit,
            string_t) sum) sum) sum * string_t list) -> (((string_t,
            string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
            sum) sum * string_t list) -> bool
         end
        
        module O : 
         sig 
          module MO : 
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
            
            val eq_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val lt_dec :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
            val eqb :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
           end
         end
        
        module P : 
         sig 
          module F : 
           sig 
            val eqb :
              (((string_t, string_t * string_t) sum, (string_t * t', (unit,
              string_t) sum) sum) sum * string_t list) -> (((string_t,
              string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
              sum) sum * string_t list) -> bool
            
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
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> 'a1 -> __ -> __ -> 'a2) -> 'a1 t ->
          'a2
        
        val map_induction_min :
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> (((string_t,
          string_t * string_t) sum, (string_t * t', (unit, string_t) sum)
          sum) sum * string_t list) -> 'a1 -> __ -> __ -> 'a2) -> 'a1 t ->
          'a2
        
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
      
      type t = DomAbs.Val.t M.t
      
      val empty : t
      
      val is_empty : t -> bool
      
      val find :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> t -> DomAbs.Val.t
      
      val add :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> t -> t
      
      val weak_add :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> t -> t
      
      val fast_weak_add :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> t -> t
      
      val remove :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> t -> t
      
      val map : (DomAbs.Val.t -> DomAbs.Val.t) -> t -> t
      
      val mapi :
        ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t ->
        DomAbs.Val.t) -> t -> t
      
      val fold : (DomAbs.Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
      
      val foldi :
        ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> 'a1 ->
        'a1) -> t -> 'a1 -> 'a1
      
      val filteri :
        ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> bool) ->
        t -> t
      
      val elements :
        t -> ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) * DomAbs.Val.t) list
      
      val cardinal : t -> int
      
      val le_than :
        t -> (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> bool
      
      val for_all :
        ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> unit) ->
        ((((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> bool) ->
        t -> bool
      
      val unstables :
        t -> t -> (DomAbs.Val.t -> DomAbs.Val.t -> bool) -> (((string_t,
        string_t * string_t) sum, (string_t * t', (unit, string_t) sum) sum)
        sum * string_t list) list -> (((((string_t, string_t * string_t) sum,
        (string_t * t', (unit, string_t) sum) sum) sum * string_t
        list) * DomAbs.Val.t) * DomAbs.Val.t) list
      
      val meet_big_small : t -> t -> t
      
      val le_dec : t -> t -> bool
      
      val strong_le : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join' :
        DomAbs.Val.t option -> DomAbs.Val.t option -> DomAbs.Val.t option
      
      val join : t -> t -> t
      
      val meet' :
        DomAbs.Val.t option -> DomAbs.Val.t option -> DomAbs.Val.t option
      
      val meet : t -> t -> t
      
      val widen' :
        DomAbs.Val.t option -> DomAbs.Val.t option -> DomAbs.Val.t option
      
      val widen : t -> t -> t
      
      val narrow' :
        DomAbs.Val.t option -> DomAbs.Val.t option -> DomAbs.Val.t option
      
      val narrow : t -> t -> t
      
      val coq_ILat : t coq_TCLat
      
      val coq_IMap :
        (((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list, DomAbs.Val.t, t) coq_TCMap
     end
    
    module PowA : 
     sig 
      type t = DomBasic.PowLoc.t
      
      val le_dec : t -> t -> bool
      
      val eq_dec : t -> t -> bool
      
      val bot : t
      
      val join : t -> t -> t
      
      val meet : t -> t -> t
      
      val widen : t -> t -> t
      
      val narrow : t -> t -> t
      
      type elt =
        ((string_t, string_t * string_t) sum, (string_t * t', (unit,
        string_t) sum) sum) sum * string_t list
      
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
    
    type t' = DomMem.Mem.t' = { base : Mem.t; spec : Mem.t }
    
    val t'_rect : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1
    
    val t'_rec : (Mem.t -> Mem.t -> 'a1) -> t' -> 'a1
    
    val base : t' -> Mem.t
    
    val spec : t' -> Mem.t
    
    type t = t'
    
    val all : t -> Mem.t
    
    val empty : t
    
    val is_empty : t -> bool
    
    val find :
      (((string_t, string_t * string_t) sum, (string_t * IntraNode.t', (unit,
      string_t) sum) sum) sum * string_t list) -> t -> DomAbs.Val.t
    
    val add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val pre_add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val main_add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val weak_add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val pre_weak_add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val main_weak_add :
      DomBasic.PowLoc.t -> (((string_t, string_t * string_t) sum,
      (string_t * IntraNode.t', (unit, string_t) sum) sum) sum * string_t
      list) -> DomAbs.Val.t -> t -> t
    
    val remove :
      (((string_t, string_t * string_t) sum, (string_t * IntraNode.t', (unit,
      string_t) sum) sum) sum * string_t list) -> t -> t
    
    val map : (DomAbs.Val.t -> DomAbs.Val.t) -> t -> t
    
    val mapi :
      ((((string_t, string_t * string_t) sum, (string_t * IntraNode.t',
      (unit, string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t ->
      DomAbs.Val.t) -> t -> t
    
    val filteri :
      ((((string_t, string_t * string_t) sum, (string_t * IntraNode.t',
      (unit, string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t ->
      bool) -> t -> t
    
    val fold : (DomAbs.Val.t -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
    
    val foldi :
      ((((string_t, string_t * string_t) sum, (string_t * IntraNode.t',
      (unit, string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> 'a1
      -> 'a1) -> t -> 'a1 -> 'a1
    
    val cardinal : t -> int
    
    val unstables :
      t -> t -> (DomAbs.Val.t -> DomAbs.Val.t -> bool) -> (((string_t,
      string_t * string_t) sum, (string_t * IntraNode.t', (unit, string_t)
      sum) sum) sum * string_t list) list -> (((((string_t,
      string_t * string_t) sum, (string_t * IntraNode.t', (unit, string_t)
      sum) sum) sum * string_t list) * DomAbs.Val.t) * DomAbs.Val.t) list
    
    val meet_big_small : t -> t -> t
    
    val le_than :
      t -> (((string_t, string_t * string_t) sum, (string_t * IntraNode.t',
      (unit, string_t) sum) sum) sum * string_t list) -> DomAbs.Val.t -> bool
    
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
    type t = G.t = { icfg : InterCfg.t; callgraph : Global.Callgraph.t;
                     dump : DomBasic.Dump.t }
    
    val t_rect :
      (InterCfg.t -> Global.Callgraph.t -> DomBasic.Dump.t -> 'a1) -> t ->
      'a1
    
    val t_rec :
      (InterCfg.t -> Global.Callgraph.t -> DomBasic.Dump.t -> 'a1) -> t ->
      'a1
    
    val icfg : t -> InterCfg.t
    
    val callgraph : t -> Global.Callgraph.t
    
    val dump : t -> DomBasic.Dump.t
    
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
        
        type 'elt tree = 'elt Table.M.Raw.tree =
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
          type t = E.t
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
            type t = E.t
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
    (__ coq_AccPair, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t)
    coq_MemBasic
  
  val coq_IdMem :
    (__, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic
  
  val can_strong_update_lv : (pid_t -> bool) -> DomBasic.Loc.t -> bool
  
  val can_strong_update : param -> G.t -> Mem.PowA.t -> bool
  
  val mem_lookup :
    ('a1, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic ->
    'a1 coq_Monad -> Mem.PowA.t -> Mem.t -> 'a1
  
  val add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1
  
  val weak_add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1
  
  val mem_update :
    ('a1, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic ->
    'a1 coq_Monad -> param -> G.t -> Mem.PowA.t -> DomAbs.Val.t -> Mem.t ->
    'a1
  
  val str_realloc : string_t
  
  val str_strlen : string_t
  
  val str_scanf : string_t
  
  val str_printf : string_t
  
  val str_vprintf : string_t
  
  val str_fprintf : string_t
  
  val str_sprintf : string_t
  
  val str_vfprintf : string_t
  
  val str_vsprintf : string_t
  
  val str_vasprintf : string_t
  
  val str___asprintf : string_t
  
  val str_asprintf : string_t
  
  val str_vdprintf : string_t
  
  val str_dprintf : string_t
  
  val str_obstack_printf : string_t
  
  val str_obstack_vprintf : string_t
  
  val str_easprintf : string_t
  
  val str_evasprintf : string_t
  
  val str_snprintf : string_t
  
  val str_vsnprintf : string_t
  
  val str_argv : string_t
  
  val ext_api : unit -> t M.t
  
  val is_printf1 : DStr.DStr.t -> bool
  
  val is_printf2 : DStr.DStr.t -> bool
  
  val is_printf3 : DStr.DStr.t -> bool
  
  val is_ppointer_printf : DStr.DStr.t -> bool
  
  val is_no_assign_printf : DStr.DStr.t -> bool
  
  val is_printf : exp -> pid_t option
  
  val update_dump :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> InterNode.t -> DomBasic.PowProc.t ->
    lval option -> DomMem.Mem.t -> DomMem.G.t -> 'a1
  
  val locals_of_mem : InterNode.t -> DomMem.Mem.t -> DomBasic.PowLoc.t
  
  val remove_local_variables :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> DomMem.Mem.t -> 'a1
  
  val external_value : DomBasic.Allocsite.t -> DPos.DPos.t -> DomAbs.Val.t
  
  val is_undef : DomMem.G.t -> exp -> pid_t option
  
  val run_realloc :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> lval -> DomAbs.Val.t list -> DomMem.Mem.t -> 'a1
  
  val run_strlen :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> lval -> DomMem.Mem.t -> 'a1
  
  val run_scanf :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> DomMem.G.t ->
    DomAbs.Val.t list -> DomMem.Mem.t -> pid_t -> DPos.DPos.t -> 'a1
  
  val run_printf1 : 'a1 coq_Monad -> DomMem.Mem.t -> 'a1
  
  val val_joins : DomAbs.Val.t list -> DomAbs.Val.t
  
  val va_src_val_joins :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.Mem.t -> DomAbs.Val.t list -> 'a1
  
  val va_arg_joins :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> InterNode.t -> DomMem.Mem.t -> exp list
    -> 'a1
  
  val run_printf_pointer :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> exp -> exp list -> DomMem.Mem.t -> 'a1
  
  val run_printf_ppointer :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> exp -> exp list -> DomMem.Mem.t -> 'a1
  
  val run_printf2 :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> pid_t -> exp list -> DomMem.Mem.t -> 'a1
  
  val run_printf3 :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> exp list -> DomMem.Mem.t -> 'a1
  
  val run_printf :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> pid_t -> exp list -> DomMem.Mem.t -> 'a1
  
  val set_ext_allocsite :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> lval -> DomBasic.Allocsite.t -> DomMem.Mem.t -> DPos.DPos.t
    -> 'a1
  
  val find_string_api_name : string_t -> t option
  
  val process_dsts :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> DPos.DPos.t -> string_t ->
    ((DomAbs.Val.t * alloc_t) * src_t) list -> DomAbs.Val.t -> DomMem.Mem.t
    -> 'a1
  
  val process_ret :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> DPos.DPos.t -> string_t -> t -> lval option -> DomAbs.Val.t
    -> ((DomAbs.Val.t * alloc_t) * src_t) list -> DomMem.Mem.t -> 'a1
  
  val run_api :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> DomMem.Mem.t -> lval option -> DomAbs.Val.t list ->
    DPos.DPos.t -> string_t -> t -> 'a1
  
  val run_api_with_name :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> DomMem.Mem.t -> lval option -> DomAbs.Val.t list ->
    DPos.DPos.t -> string_t -> 'a1
  
  val run_undef_funcs :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
    DomMem.G.t -> lval option -> pid_t -> DomAbs.Val.t list -> DomMem.Mem.t
    -> DPos.DPos.t -> 'a1
  
  val run :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t -> cmd ->
    (DomMem.Mem.t * DomMem.G.t) -> 'a1
  
  val run_access :
    DomMem.param -> InterNode.t -> cmd -> (DomMem.Mem.t * DomMem.G.t) ->
    (DomMem.Mem.t * DomMem.G.t) DomMem.coq_AccPair
  
  val run_only :
    DomMem.param -> InterNode.t -> cmd -> (DomMem.Mem.t * DomMem.G.t) ->
    DomMem.Mem.t * DomMem.G.t
  
  type query = exp
  
  type status' = Query.status' =
  | Tainted of DomBasic.PowExtProcPos.t
  | Clean
  
  val status'_rect :
    (DomBasic.PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1
  
  val status'_rec :
    (DomBasic.PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1
  
  type status = status'
  
  val fmt_query : exp -> (query * DPos.DPos.t) list
  
  val collect_query : cmd -> (query * DPos.DPos.t) list
  
  val check_query :
    ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
    coq_MemBasic -> 'a1 coq_Monad -> pid_t -> DomMem.Mem.t -> query -> 'a1
 end

