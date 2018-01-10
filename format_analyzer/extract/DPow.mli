open DFSetAVL
open DLat
open Datatypes
open VocabA

type __ = Obj.t

type ('e, 's) coq_TCSet = { set_empty : 's; set_is_empty : ('s -> bool);
                            set_add : ('e -> 's -> 's);
                            set_singleton : ('e -> 's);
                            set_mem : ('e -> 's -> bool);
                            set_remove : ('e -> 's -> 's);
                            set_union : ('s -> 's -> 's);
                            set_union_small_big : ('s -> 's -> 's);
                            set_intersect : ('s -> 's -> 's);
                            set_diff : ('s -> 's -> 's);
                            set_subset : ('s -> 's -> bool);
                            set_filter : (('e -> bool) -> 's -> 's);
                            set_fold : (__ -> ('e -> __ -> __) -> 's -> __ ->
                                       __);
                            set_iter : (('e -> unit) -> 's -> unit);
                            set_elements : ('s -> 'e list);
                            set_cardinal : ('s -> int);
                            set_choose : ('s -> 'e option);
                            set_choose_only : ('s -> 'e option);
                            set_for_all : (('e -> unit) -> ('e -> bool) -> 's
                                          -> bool) }

val coq_TCSet_rect :
  ('a2 -> ('a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a1 -> 'a2) -> ('a1 -> 'a2
  -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 ->
  'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> bool)
  -> (('a1 -> bool) -> 'a2 -> 'a2) -> (__ -> ('a1 -> __ -> __) -> 'a2 -> __
  -> __) -> (('a1 -> unit) -> 'a2 -> unit) -> ('a2 -> 'a1 list) -> ('a2 ->
  int) -> ('a2 -> 'a1 option) -> ('a2 -> 'a1 option) -> (('a1 -> unit) ->
  ('a1 -> bool) -> 'a2 -> bool) -> 'a3) -> ('a1, 'a2) coq_TCSet -> 'a3

val coq_TCSet_rec :
  ('a2 -> ('a2 -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a1 -> 'a2) -> ('a1 -> 'a2
  -> bool) -> ('a1 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 ->
  'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> 'a2) -> ('a2 -> 'a2 -> bool)
  -> (('a1 -> bool) -> 'a2 -> 'a2) -> (__ -> ('a1 -> __ -> __) -> 'a2 -> __
  -> __) -> (('a1 -> unit) -> 'a2 -> unit) -> ('a2 -> 'a1 list) -> ('a2 ->
  int) -> ('a2 -> 'a1 option) -> ('a2 -> 'a1 option) -> (('a1 -> unit) ->
  ('a1 -> bool) -> 'a2 -> bool) -> 'a3) -> ('a1, 'a2) coq_TCSet -> 'a3

val set_empty : ('a1, 'a2) coq_TCSet -> 'a2

val set_is_empty : ('a1, 'a2) coq_TCSet -> 'a2 -> bool

val set_add : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> 'a2

val set_singleton : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2

val set_mem : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> bool

val set_remove : ('a1, 'a2) coq_TCSet -> 'a1 -> 'a2 -> 'a2

val set_union : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2

val set_union_small_big : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2

val set_intersect : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2

val set_diff : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> 'a2

val set_subset : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a2 -> bool

val set_filter : ('a1, 'a2) coq_TCSet -> ('a1 -> bool) -> 'a2 -> 'a2

val set_fold :
  ('a1, 'a2) coq_TCSet -> ('a1 -> 'a3 -> 'a3) -> 'a2 -> 'a3 -> 'a3

val set_iter : ('a1, 'a2) coq_TCSet -> ('a1 -> unit) -> 'a2 -> unit

val set_elements : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 list

val set_cardinal : ('a1, 'a2) coq_TCSet -> 'a2 -> int

val set_choose : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 option

val set_choose_only : ('a1, 'a2) coq_TCSet -> 'a2 -> 'a1 option

val set_for_all :
  ('a1, 'a2) coq_TCSet -> ('a1 -> unit) -> ('a1 -> bool) -> 'a2 -> bool

module type POW = 
 sig 
  type t 
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  type elt 
  
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

module Pow : 
 functor (A__1:KEY) ->
 sig 
  module A : 
   sig 
    type t = A__1.t
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    val eq_dec : t -> t -> bool
   end
  
  module SS : 
   sig 
    module S : 
     sig 
      module X' : 
       sig 
        type t = A__1.t
        
        val eq_dec : t -> t -> bool
        
        val compare : A__1.t -> A__1.t -> comparison
       end
      
      module MSet : 
       sig 
        module Raw : 
         sig 
          type elt = A__1.t
          
          type tree =
          | Leaf
          | Node of Int.Z_as_Int.t * tree * A__1.t * tree
          
          val empty : tree
          
          val is_empty : tree -> bool
          
          val mem : A__1.t -> tree -> bool
          
          val min_elt : tree -> elt option
          
          val max_elt : tree -> elt option
          
          val choose : tree -> elt option
          
          val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
          
          val elements_aux : A__1.t list -> tree -> A__1.t list
          
          val elements : tree -> A__1.t list
          
          val rev_elements_aux : A__1.t list -> tree -> A__1.t list
          
          val rev_elements : tree -> A__1.t list
          
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
            A__1.t -> (enumeration -> comparison) -> enumeration ->
            comparison
          
          val compare_cont :
            tree -> (enumeration -> comparison) -> enumeration -> comparison
          
          val compare_end : enumeration -> comparison
          
          val compare : tree -> tree -> comparison
          
          val equal : tree -> tree -> bool
          
          val subsetl : (tree -> bool) -> A__1.t -> tree -> bool
          
          val subsetr : (tree -> bool) -> A__1.t -> tree -> bool
          
          val subset : tree -> tree -> bool
          
          type t = tree
          
          val height : t -> Int.Z_as_Int.t
          
          val singleton : A__1.t -> tree
          
          val create : t -> A__1.t -> t -> tree
          
          val assert_false : t -> A__1.t -> t -> tree
          
          val bal : t -> A__1.t -> t -> tree
          
          val add : A__1.t -> tree -> tree
          
          val join : tree -> elt -> t -> t
          
          val remove_min : tree -> elt -> t -> t * elt
          
          val merge : tree -> tree -> tree
          
          val remove : A__1.t -> tree -> tree
          
          val concat : tree -> tree -> tree
          
          type triple = { t_left : t; t_in : bool; t_right : t }
          
          val t_left : triple -> t
          
          val t_in : triple -> bool
          
          val t_right : triple -> t
          
          val split : A__1.t -> tree -> triple
          
          val inter : tree -> tree -> tree
          
          val diff : tree -> tree -> tree
          
          val union : tree -> tree -> tree
          
          val filter : (elt -> bool) -> tree -> tree
          
          val partition : (elt -> bool) -> t -> t * t
          
          val ltb_tree : A__1.t -> tree -> bool
          
          val gtb_tree : A__1.t -> tree -> bool
          
          val isok : tree -> bool
          
          module MX : 
           sig 
            module OrderTac : 
             sig 
              module OTF : 
               sig 
                type t = A__1.t
                
                val compare : A__1.t -> A__1.t -> comparison
                
                val eq_dec : A__1.t -> A__1.t -> bool
               end
              
              module TO : 
               sig 
                type t = A__1.t
                
                val compare : A__1.t -> A__1.t -> comparison
                
                val eq_dec : A__1.t -> A__1.t -> bool
               end
             end
            
            val eq_dec : A__1.t -> A__1.t -> bool
            
            val lt_dec : A__1.t -> A__1.t -> bool
            
            val eqb : A__1.t -> A__1.t -> bool
           end
          
          type coq_R_min_elt =
          | R_min_elt_0 of tree
          | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
             * Int.Z_as_Int.t * tree * A__1.t * tree * elt option
             * coq_R_min_elt
          
          type coq_R_max_elt =
          | R_max_elt_0 of tree
          | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
             * Int.Z_as_Int.t * tree * A__1.t * tree * elt option
             * coq_R_max_elt
          
          module L : 
           sig 
            module MO : 
             sig 
              module OrderTac : 
               sig 
                module OTF : 
                 sig 
                  type t = A__1.t
                  
                  val compare : A__1.t -> A__1.t -> comparison
                  
                  val eq_dec : A__1.t -> A__1.t -> bool
                 end
                
                module TO : 
                 sig 
                  type t = A__1.t
                  
                  val compare : A__1.t -> A__1.t -> comparison
                  
                  val eq_dec : A__1.t -> A__1.t -> bool
                 end
               end
              
              val eq_dec : A__1.t -> A__1.t -> bool
              
              val lt_dec : A__1.t -> A__1.t -> bool
              
              val eqb : A__1.t -> A__1.t -> bool
             end
           end
          
          val flatten_e : enumeration -> elt list
          
          type coq_R_bal =
          | R_bal_0 of t * A__1.t * t
          | R_bal_1 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_2 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_3 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_4 of t * A__1.t * t
          | R_bal_5 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_6 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_7 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_bal_8 of t * A__1.t * t
          
          type coq_R_remove_min =
          | R_remove_min_0 of tree * elt * t
          | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree * 
             A__1.t * tree * (t * elt) * coq_R_remove_min * t * elt
          
          type coq_R_merge =
          | R_merge_0 of tree * tree
          | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * elt
          
          type coq_R_concat =
          | R_concat_0 of tree * tree
          | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * elt
          
          type coq_R_inter =
          | R_inter_0 of tree * tree
          | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
             t * tree * coq_R_inter * tree * coq_R_inter
          
          type coq_R_diff =
          | R_diff_0 of tree * tree
          | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
             t * tree * coq_R_diff * tree * coq_R_diff
          
          type coq_R_union =
          | R_union_0 of tree * tree
          | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
          | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
             tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
             t * tree * coq_R_union * tree * coq_R_union
         end
        
        module E : 
         sig 
          type t = A__1.t
          
          val compare : A__1.t -> A__1.t -> comparison
          
          val eq_dec : A__1.t -> A__1.t -> bool
         end
        
        type elt = A__1.t
        
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
      
      type elt = A__1.t
      
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
        val eqb : A__1.t -> A__1.t -> bool
       end
      
      val min_elt : t -> elt option
      
      val max_elt : t -> elt option
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      module E : 
       sig 
        type t = A__1.t
        
        val compare : t -> t -> t OrderedType.coq_Compare
        
        val eq_dec : t -> t -> bool
       end
     end
    
    module X' : 
     sig 
      type t = A__1.t
      
      val eq_dec : t -> t -> bool
      
      val compare : A__1.t -> A__1.t -> comparison
     end
    
    module MSet : 
     sig 
      module Raw : 
       sig 
        type elt = X'.t
        
        type tree = S.MSet.Raw.tree =
        | Leaf
        | Node of Int.Z_as_Int.t * tree * A__1.t * tree
        
        val empty : tree
        
        val is_empty : tree -> bool
        
        val mem : A__1.t -> tree -> bool
        
        val min_elt : tree -> elt option
        
        val max_elt : tree -> elt option
        
        val choose : tree -> elt option
        
        val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1
        
        val elements_aux : A__1.t list -> tree -> A__1.t list
        
        val elements : tree -> A__1.t list
        
        val rev_elements_aux : A__1.t list -> tree -> A__1.t list
        
        val rev_elements : tree -> A__1.t list
        
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
          A__1.t -> (enumeration -> comparison) -> enumeration -> comparison
        
        val compare_cont :
          tree -> (enumeration -> comparison) -> enumeration -> comparison
        
        val compare_end : enumeration -> comparison
        
        val compare : tree -> tree -> comparison
        
        val equal : tree -> tree -> bool
        
        val subsetl : (tree -> bool) -> A__1.t -> tree -> bool
        
        val subsetr : (tree -> bool) -> A__1.t -> tree -> bool
        
        val subset : tree -> tree -> bool
        
        type t = tree
        
        val height : t -> Int.Z_as_Int.t
        
        val singleton : A__1.t -> tree
        
        val create : t -> A__1.t -> t -> tree
        
        val assert_false : t -> A__1.t -> t -> tree
        
        val bal : t -> A__1.t -> t -> tree
        
        val add : A__1.t -> tree -> tree
        
        val join : tree -> elt -> t -> t
        
        val remove_min : tree -> elt -> t -> t * elt
        
        val merge : tree -> tree -> tree
        
        val remove : A__1.t -> tree -> tree
        
        val concat : tree -> tree -> tree
        
        type triple = { t_left : t; t_in : bool; t_right : t }
        
        val t_left : triple -> t
        
        val t_in : triple -> bool
        
        val t_right : triple -> t
        
        val split : A__1.t -> tree -> triple
        
        val inter : tree -> tree -> tree
        
        val diff : tree -> tree -> tree
        
        val union : tree -> tree -> tree
        
        val filter : (elt -> bool) -> tree -> tree
        
        val partition : (elt -> bool) -> t -> t * t
        
        val ltb_tree : A__1.t -> tree -> bool
        
        val gtb_tree : A__1.t -> tree -> bool
        
        val isok : tree -> bool
        
        module MX : 
         sig 
          module OrderTac : 
           sig 
            module OTF : 
             sig 
              type t = X'.t
              
              val compare : A__1.t -> A__1.t -> comparison
              
              val eq_dec : A__1.t -> A__1.t -> bool
             end
            
            module TO : 
             sig 
              type t = A__1.t
              
              val compare : A__1.t -> A__1.t -> comparison
              
              val eq_dec : A__1.t -> A__1.t -> bool
             end
           end
          
          val eq_dec : A__1.t -> A__1.t -> bool
          
          val lt_dec : A__1.t -> A__1.t -> bool
          
          val eqb : A__1.t -> A__1.t -> bool
         end
        
        type coq_R_min_elt =
        | R_min_elt_0 of tree
        | R_min_elt_1 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_min_elt_2 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
           * Int.Z_as_Int.t * tree * A__1.t * tree * elt option
           * coq_R_min_elt
        
        type coq_R_max_elt =
        | R_max_elt_0 of tree
        | R_max_elt_1 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_max_elt_2 of tree * Int.Z_as_Int.t * tree * A__1.t * tree
           * Int.Z_as_Int.t * tree * A__1.t * tree * elt option
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
                
                val compare : A__1.t -> A__1.t -> comparison
                
                val eq_dec : A__1.t -> A__1.t -> bool
               end
              
              module TO : 
               sig 
                type t = A__1.t
                
                val compare : A__1.t -> A__1.t -> comparison
                
                val eq_dec : A__1.t -> A__1.t -> bool
               end
             end
            
            val eq_dec : A__1.t -> A__1.t -> bool
            
            val lt_dec : A__1.t -> A__1.t -> bool
            
            val eqb : A__1.t -> A__1.t -> bool
           end
         end
        
        val flatten_e : enumeration -> elt list
        
        type coq_R_bal =
        | R_bal_0 of t * A__1.t * t
        | R_bal_1 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_2 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_3 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_4 of t * A__1.t * t
        | R_bal_5 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_6 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_7 of t * A__1.t * t * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_bal_8 of t * A__1.t * t
        
        type coq_R_remove_min =
        | R_remove_min_0 of tree * elt * t
        | R_remove_min_1 of tree * elt * t * Int.Z_as_Int.t * tree * 
           A__1.t * tree * (t * elt) * coq_R_remove_min * t * elt
        
        type coq_R_merge =
        | R_merge_0 of tree * tree
        | R_merge_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_merge_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * elt
        
        type coq_R_concat =
        | R_concat_0 of tree * tree
        | R_concat_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_concat_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * elt
        
        type coq_R_inter =
        | R_inter_0 of tree * tree
        | R_inter_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_inter_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
           t * tree * coq_R_inter * tree * coq_R_inter
        | R_inter_3 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
           t * tree * coq_R_inter * tree * coq_R_inter
        
        type coq_R_diff =
        | R_diff_0 of tree * tree
        | R_diff_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_diff_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
           t * tree * coq_R_diff * tree * coq_R_diff
        | R_diff_3 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
           t * tree * coq_R_diff * tree * coq_R_diff
        
        type coq_R_union =
        | R_union_0 of tree * tree
        | R_union_1 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * tree
        | R_union_2 of tree * tree * Int.Z_as_Int.t * tree * A__1.t * 
           tree * Int.Z_as_Int.t * tree * A__1.t * tree * t * bool * 
           t * tree * coq_R_union * tree * coq_R_union
       end
      
      module E : 
       sig 
        type t = A__1.t
        
        val compare : A__1.t -> A__1.t -> comparison
        
        val eq_dec : A__1.t -> A__1.t -> bool
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
    
    type elt = A__1.t
    
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
      val eqb : A__1.t -> A__1.t -> bool
     end
    
    val min_elt : t -> elt option
    
    val max_elt : t -> elt option
    
    val compare : t -> t -> t OrderedType.coq_Compare
    
    module E : 
     sig 
      type t = A__1.t
      
      val compare : t -> t -> t OrderedType.coq_Compare
      
      val eq_dec : t -> t -> bool
     end
    
    module SF : 
     sig 
      val eqb : A__1.t -> A__1.t -> bool
     end
    
    val choose_only : t -> elt option
    
    val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool
    
    val cond_eq_rect :
      (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
    
    val cond_eq_rec :
      (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1
   end
  
  type elt = A__1.t
  
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
  
  val add : A__1.t -> t -> t
  
  val singleton : A__1.t -> t
  
  val mem : SS.elt -> SS.t -> bool
  
  val remove : A__1.t -> t -> t
  
  val union : SS.t -> SS.t -> SS.t
  
  val union_small_big : SS.t -> SS.t -> SS.t
  
  val intersect : SS.t -> SS.t -> SS.t
  
  val diff : SS.t -> SS.t -> SS.t
  
  val subset : SS.t -> SS.t -> bool
  
  val filter : (A__1.t -> bool) -> t -> t
  
  val fold : (SS.elt -> 'a1 -> 'a1) -> SS.t -> 'a1 -> 'a1
  
  val iter : (A__1.t -> unit) -> t -> unit
  
  val elements : SS.t -> SS.elt list
  
  val cardinal : SS.t -> int
  
  val choose : SS.t -> SS.elt option
  
  val choose_only : SS.t -> SS.elt option
  
  val for_all : (SS.elt -> unit) -> (SS.elt -> bool) -> SS.t -> bool
  
  val coq_ILat : t coq_TCLat
  
  val coq_ISet : (A__1.t, t) coq_TCSet
 end

