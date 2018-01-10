open Datatypes
open FSetAVL
open FSetFacts
open Peano_dec
open VocabA

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module FSetAVL' = 
 struct 
  module Make = 
   functor (X:OrderedType.OrderedType) ->
   struct 
    module S = Make(X)
    
    module X' = S.X'
    
    module MSet = S.MSet
    
    type elt = X.t
    
    type t = MSet.t
    
    (** val empty : t **)
    
    let empty =
      MSet.empty
    
    (** val is_empty : t -> bool **)
    
    let is_empty =
      MSet.is_empty
    
    (** val mem : elt -> t -> bool **)
    
    let mem =
      MSet.mem
    
    (** val add : elt -> t -> t **)
    
    let add =
      MSet.add
    
    (** val singleton : elt -> t **)
    
    let singleton =
      MSet.singleton
    
    (** val remove : elt -> t -> t **)
    
    let remove =
      MSet.remove
    
    (** val union : t -> t -> t **)
    
    let union =
      MSet.union
    
    (** val inter : t -> t -> t **)
    
    let inter =
      MSet.inter
    
    (** val diff : t -> t -> t **)
    
    let diff =
      MSet.diff
    
    (** val eq_dec : t -> t -> bool **)
    
    let eq_dec =
      MSet.eq_dec
    
    (** val equal : t -> t -> bool **)
    
    let equal =
      MSet.equal
    
    (** val subset : t -> t -> bool **)
    
    let subset =
      MSet.subset
    
    (** val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
    
    let fold x x0 x1 =
      MSet.fold x x0 x1
    
    (** val for_all : (elt -> bool) -> t -> bool **)
    
    let for_all =
      MSet.for_all
    
    (** val exists_ : (elt -> bool) -> t -> bool **)
    
    let exists_ =
      MSet.exists_
    
    (** val filter : (elt -> bool) -> t -> t **)
    
    let filter =
      MSet.filter
    
    (** val partition : (elt -> bool) -> t -> t * t **)
    
    let partition =
      MSet.partition
    
    (** val cardinal : t -> int **)
    
    let cardinal =
      MSet.cardinal
    
    (** val elements : t -> elt list **)
    
    let elements =
      MSet.elements
    
    (** val choose : t -> elt option **)
    
    let choose =
      MSet.choose
    
    module MF = S.MF
    
    (** val min_elt : t -> elt option **)
    
    let min_elt =
      MSet.min_elt
    
    (** val max_elt : t -> elt option **)
    
    let max_elt =
      MSet.max_elt
    
    (** val compare : t -> t -> t OrderedType.coq_Compare **)
    
    let compare s s' =
      let c = coq_CompSpec2Type s s' (MSet.compare s s') in
      (match c with
       | CompEqT -> OrderedType.EQ
       | CompLtT -> OrderedType.LT
       | CompGtT -> OrderedType.GT)
    
    module E = 
     struct 
      type t = X.t
      
      (** val compare : t -> t -> t OrderedType.coq_Compare **)
      
      let compare =
        X.compare
      
      (** val eq_dec : t -> t -> bool **)
      
      let eq_dec =
        X.eq_dec
     end
    
    module SF = Facts(S)
    
    (** val choose_only : t -> elt option **)
    
    let choose_only s =
      if eq_nat_dec (cardinal s) (Pervasives.succ 0) then choose s else None
    
    (** val for_all' : (elt -> unit) -> (elt -> bool) -> t -> bool **)
    
    let for_all' print cond m =
      let add_cond = fun v acc -> (&&) acc (print_when_false print v cond v)
      in
      fold add_cond m true
    
    (** val cond_eq_rect :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1 **)
    
    let cond_eq_rect f g f0 =
      f0 __ __ __
    
    (** val cond_eq_rec :
        (elt -> bool) -> (elt -> bool) -> (__ -> __ -> __ -> 'a1) -> 'a1 **)
    
    let cond_eq_rec f g f0 =
      f0 __ __ __
   end
 end

