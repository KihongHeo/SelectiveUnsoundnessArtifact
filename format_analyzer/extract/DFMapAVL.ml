open Datatypes
open List0
open OrdersTac
open Peano
open VocabA

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module FMapAVL' = 
 struct 
  module Make = 
   functor (X:OrderedType.OrderedType) ->
   struct 
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
    
    module Raw = 
     struct 
      type key = X.t
      
      type 'elt tree =
      | Leaf
      | Node of 'elt tree * key * 'elt * 'elt tree * Int.Z_as_Int.t
      
      (** val tree_rect :
          'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
          Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2 **)
      
      let rec tree_rect f f0 = function
      | Leaf -> f
      | Node (t1, k, e, t2, t3) ->
        f0 t1 (tree_rect f f0 t1) k e t2 (tree_rect f f0 t2) t3
      
      (** val tree_rec :
          'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
          Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2 **)
      
      let rec tree_rec f f0 = function
      | Leaf -> f
      | Node (t1, k, e, t2, t3) ->
        f0 t1 (tree_rec f f0 t1) k e t2 (tree_rec f f0 t2) t3
      
      (** val height : 'a1 tree -> Int.Z_as_Int.t **)
      
      let height = function
      | Leaf -> Int.Z_as_Int._0
      | Node (t0, k, e, t1, h) -> h
      
      (** val cardinal : 'a1 tree -> int **)
      
      let rec cardinal = function
      | Leaf -> 0
      | Node (l, k, e, r, t0) ->
        Pervasives.succ (plus (cardinal l) (cardinal r))
      
      (** val empty : 'a1 tree **)
      
      let empty =
        Leaf
      
      (** val is_empty : 'a1 tree -> bool **)
      
      let is_empty = function
      | Leaf -> true
      | Node (t0, k, e, t1, t2) -> false
      
      (** val mem : X.t -> 'a1 tree -> bool **)
      
      let rec mem x = function
      | Leaf -> false
      | Node (l, y, e, r, t0) ->
        (match X.compare x y with
         | OrderedType.LT -> mem x l
         | OrderedType.EQ -> true
         | OrderedType.GT -> mem x r)
      
      (** val find : X.t -> 'a1 tree -> 'a1 option **)
      
      let rec find x = function
      | Leaf -> None
      | Node (l, y, d, r, t0) ->
        (match X.compare x y with
         | OrderedType.LT -> find x l
         | OrderedType.EQ -> Some d
         | OrderedType.GT -> find x r)
      
      (** val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
      
      let create l x e r =
        Node (l, x, e, r,
          (Int.Z_as_Int.plus (Int.Z_as_Int.max (height l) (height r))
            Int.Z_as_Int._1))
      
      (** val assert_false :
          'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
      
      let assert_false =
        create
      
      (** val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
      
      let bal l x d r =
        let hl = height l in
        let hr = height r in
        if Int.Z_as_Int.gt_le_dec hl (Int.Z_as_Int.plus hr Int.Z_as_Int._2)
        then (match l with
              | Leaf -> assert_false l x d r
              | Node (ll, lx, ld, lr, t0) ->
                if Int.Z_as_Int.ge_lt_dec (height ll) (height lr)
                then create ll lx ld (create lr x d r)
                else (match lr with
                      | Leaf -> assert_false l x d r
                      | Node (lrl, lrx, lrd, lrr, t1) ->
                        create (create ll lx ld lrl) lrx lrd
                          (create lrr x d r)))
        else if Int.Z_as_Int.gt_le_dec hr
                  (Int.Z_as_Int.plus hl Int.Z_as_Int._2)
             then (match r with
                   | Leaf -> assert_false l x d r
                   | Node (rl, rx, rd, rr, t0) ->
                     if Int.Z_as_Int.ge_lt_dec (height rr) (height rl)
                     then create (create l x d rl) rx rd rr
                     else (match rl with
                           | Leaf -> assert_false l x d r
                           | Node (rll, rlx, rld, rlr, t1) ->
                             create (create l x d rll) rlx rld
                               (create rlr rx rd rr)))
             else create l x d r
      
      (** val add : key -> 'a1 -> 'a1 tree -> 'a1 tree **)
      
      let rec add x d = function
      | Leaf -> Node (Leaf, x, d, Leaf, Int.Z_as_Int._1)
      | Node (l, y, d', r, h) ->
        (match X.compare x y with
         | OrderedType.LT -> bal (add x d l) y d' r
         | OrderedType.EQ -> Node (l, y, d, r, h)
         | OrderedType.GT -> bal l y d' (add x d r))
      
      (** val remove_min :
          'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1) **)
      
      let rec remove_min l x d r =
        match l with
        | Leaf -> (r, (x, d))
        | Node (ll, lx, ld, lr, lh) ->
          let (l', m) = remove_min ll lx ld lr in ((bal l' x d r), m)
      
      (** val merge : 'a1 tree -> 'a1 tree -> 'a1 tree **)
      
      let merge s1 s2 =
        match s1 with
        | Leaf -> s2
        | Node (t0, k, e, t1, t2) ->
          (match s2 with
           | Leaf -> s1
           | Node (l2, x2, d2, r2, h2) ->
             let (s2', p) = remove_min l2 x2 d2 r2 in
             let (x, d) = p in bal s1 x d s2')
      
      (** val remove : X.t -> 'a1 tree -> 'a1 tree **)
      
      let rec remove x = function
      | Leaf -> Leaf
      | Node (l, y, d, r, h) ->
        (match X.compare x y with
         | OrderedType.LT -> bal (remove x l) y d r
         | OrderedType.EQ -> merge l r
         | OrderedType.GT -> bal l y d (remove x r))
      
      (** val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
      
      let rec join l = match l with
      | Leaf -> add
      | Node (ll, lx, ld, lr, lh) ->
        (fun x d ->
          let rec join_aux r = match r with
          | Leaf -> add x d l
          | Node (rl, rx, rd, rr, rh) ->
            if Int.Z_as_Int.gt_le_dec lh
                 (Int.Z_as_Int.plus rh Int.Z_as_Int._2)
            then bal ll lx ld (join lr x d r)
            else if Int.Z_as_Int.gt_le_dec rh
                      (Int.Z_as_Int.plus lh Int.Z_as_Int._2)
                 then bal (join_aux rl) rx rd rr
                 else create l x d r
          in join_aux)
      
      type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                           t_right : 'elt tree }
      
      (** val triple_rect :
          ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2 **)
      
      let triple_rect f t0 =
        let { t_left = x; t_opt = x0; t_right = x1 } = t0 in f x x0 x1
      
      (** val triple_rec :
          ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2 **)
      
      let triple_rec f t0 =
        let { t_left = x; t_opt = x0; t_right = x1 } = t0 in f x x0 x1
      
      (** val t_left : 'a1 triple -> 'a1 tree **)
      
      let t_left t0 =
        t0.t_left
      
      (** val t_opt : 'a1 triple -> 'a1 option **)
      
      let t_opt t0 =
        t0.t_opt
      
      (** val t_right : 'a1 triple -> 'a1 tree **)
      
      let t_right t0 =
        t0.t_right
      
      (** val split : X.t -> 'a1 tree -> 'a1 triple **)
      
      let rec split x = function
      | Leaf -> { t_left = Leaf; t_opt = None; t_right = Leaf }
      | Node (l, y, d, r, h) ->
        (match X.compare x y with
         | OrderedType.LT ->
           let { t_left = ll; t_opt = o; t_right = rl } = split x l in
           { t_left = ll; t_opt = o; t_right = (join rl y d r) }
         | OrderedType.EQ -> { t_left = l; t_opt = (Some d); t_right = r }
         | OrderedType.GT ->
           let { t_left = rl; t_opt = o; t_right = rr } = split x r in
           { t_left = (join l y d rl); t_opt = o; t_right = rr })
      
      (** val concat : 'a1 tree -> 'a1 tree -> 'a1 tree **)
      
      let concat m1 m2 =
        match m1 with
        | Leaf -> m2
        | Node (t0, k, e, t1, t2) ->
          (match m2 with
           | Leaf -> m1
           | Node (l2, x2, d2, r2, t3) ->
             let (m2', xd) = remove_min l2 x2 d2 r2 in
             join m1 (fst xd) (snd xd) m2')
      
      (** val elements_aux :
          (key * 'a1) list -> 'a1 tree -> (key * 'a1) list **)
      
      let rec elements_aux acc = function
      | Leaf -> acc
      | Node (l, x, d, r, t0) ->
        elements_aux ((x, d) :: (elements_aux acc r)) l
      
      (** val elements : 'a1 tree -> (key * 'a1) list **)
      
      let elements m =
        elements_aux [] m
      
      (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)
      
      let rec fold f m a =
        match m with
        | Leaf -> a
        | Node (l, x, d, r, t0) -> fold f r (f x d (fold f l a))
      
      type 'elt enumeration =
      | End
      | More of key * 'elt * 'elt tree * 'elt enumeration
      
      (** val enumeration_rect :
          'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
          'a1 enumeration -> 'a2 **)
      
      let rec enumeration_rect f f0 = function
      | End -> f
      | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rect f f0 e1)
      
      (** val enumeration_rec :
          'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
          'a1 enumeration -> 'a2 **)
      
      let rec enumeration_rec f f0 = function
      | End -> f
      | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rec f f0 e1)
      
      (** val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration **)
      
      let rec cons m e =
        match m with
        | Leaf -> e
        | Node (l, x, d, r, h) -> cons l (More (x, d, r, e))
      
      (** val equal_more :
          ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) ->
          'a1 enumeration -> bool **)
      
      let equal_more cmp x1 d1 cont = function
      | End -> false
      | More (x2, d2, r2, e3) ->
        (match X.compare x1 x2 with
         | OrderedType.EQ -> if cmp d1 d2 then cont (cons r2 e3) else false
         | _ -> false)
      
      (** val equal_cont :
          ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) ->
          'a1 enumeration -> bool **)
      
      let rec equal_cont cmp m1 cont e2 =
        match m1 with
        | Leaf -> cont e2
        | Node (l1, x1, d1, r1, t0) ->
          equal_cont cmp l1 (equal_more cmp x1 d1 (equal_cont cmp r1 cont))
            e2
      
      (** val equal_end : 'a1 enumeration -> bool **)
      
      let equal_end = function
      | End -> true
      | More (k, e, t0, e0) -> false
      
      (** val equal :
          ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool **)
      
      let equal cmp m1 m2 =
        equal_cont cmp m1 equal_end (cons m2 End)
      
      (** val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)
      
      let rec map f = function
      | Leaf -> Leaf
      | Node (l, x, d, r, h) -> Node ((map f l), x, (f d), (map f r), h)
      
      (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)
      
      let rec mapi f = function
      | Leaf -> Leaf
      | Node (l, x, d, r, h) -> Node ((mapi f l), x, (f x d), (mapi f r), h)
      
      (** val map_option :
          (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree **)
      
      let rec map_option f = function
      | Leaf -> Leaf
      | Node (l, x, d, r, h) ->
        (match f x d with
         | Some d' -> join (map_option f l) x d' (map_option f r)
         | None -> concat (map_option f l) (map_option f r))
      
      (** val map2_opt :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree **)
      
      let rec map2_opt f mapl mapr m1 m2 =
        match m1 with
        | Leaf -> mapr m2
        | Node (l1, x1, d1, r1, h1) ->
          (match m2 with
           | Leaf -> mapl m1
           | Node (t0, k, y, t1, t2) ->
             let { t_left = l2'; t_opt = o2; t_right = r2' } = split x1 m2 in
             (match f x1 d1 o2 with
              | Some e ->
                join (map2_opt f mapl mapr l1 l2') x1 e
                  (map2_opt f mapl mapr r1 r2')
              | None ->
                concat (map2_opt f mapl mapr l1 l2')
                  (map2_opt f mapl mapr r1 r2')))
      
      (** val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree ->
          'a3 tree **)
      
      let map2 f =
        map2_opt (fun x d o -> f (Some d) o)
          (map_option (fun x d -> f (Some d) None))
          (map_option (fun x d' -> f None (Some d')))
      
      module Proofs = 
       struct 
        module MX = 
         struct 
          module TO = 
           struct 
            type t = X.t
           end
          
          module IsTO = 
           struct 
            
           end
          
          module OrderTac = MakeOrderTac(TO)(IsTO)
          
          (** val eq_dec : X.t -> X.t -> bool **)
          
          let eq_dec =
            X.eq_dec
          
          (** val lt_dec : X.t -> X.t -> bool **)
          
          let lt_dec x y =
            match X.compare x y with
            | OrderedType.LT -> true
            | _ -> false
          
          (** val eqb : X.t -> X.t -> bool **)
          
          let eqb x y =
            if eq_dec x y then true else false
         end
        
        module PX = 
         struct 
          module MO = 
           struct 
            module TO = 
             struct 
              type t = X.t
             end
            
            module IsTO = 
             struct 
              
             end
            
            module OrderTac = MakeOrderTac(TO)(IsTO)
            
            (** val eq_dec : X.t -> X.t -> bool **)
            
            let eq_dec =
              X.eq_dec
            
            (** val lt_dec : X.t -> X.t -> bool **)
            
            let lt_dec x y =
              match X.compare x y with
              | OrderedType.LT -> true
              | _ -> false
            
            (** val eqb : X.t -> X.t -> bool **)
            
            let eqb x y =
              if eq_dec x y then true else false
           end
         end
        
        module L = 
         struct 
          module MX = 
           struct 
            module TO = 
             struct 
              type t = X.t
             end
            
            module IsTO = 
             struct 
              
             end
            
            module OrderTac = MakeOrderTac(TO)(IsTO)
            
            (** val eq_dec : X.t -> X.t -> bool **)
            
            let eq_dec =
              X.eq_dec
            
            (** val lt_dec : X.t -> X.t -> bool **)
            
            let lt_dec x y =
              match X.compare x y with
              | OrderedType.LT -> true
              | _ -> false
            
            (** val eqb : X.t -> X.t -> bool **)
            
            let eqb x y =
              if eq_dec x y then true else false
           end
          
          module PX = 
           struct 
            module MO = 
             struct 
              module TO = 
               struct 
                type t = X.t
               end
              
              module IsTO = 
               struct 
                
               end
              
              module OrderTac = MakeOrderTac(TO)(IsTO)
              
              (** val eq_dec : X.t -> X.t -> bool **)
              
              let eq_dec =
                X.eq_dec
              
              (** val lt_dec : X.t -> X.t -> bool **)
              
              let lt_dec x y =
                match X.compare x y with
                | OrderedType.LT -> true
                | _ -> false
              
              (** val eqb : X.t -> X.t -> bool **)
              
              let eqb x y =
                if eq_dec x y then true else false
             end
           end
          
          type key = X.t
          
          type 'elt t = (X.t * 'elt) list
          
          (** val empty : 'a1 t **)
          
          let empty =
            []
          
          (** val is_empty : 'a1 t -> bool **)
          
          let is_empty = function
          | [] -> true
          | x :: x0 -> false
          
          (** val mem : key -> 'a1 t -> bool **)
          
          let rec mem k = function
          | [] -> false
          | p :: l ->
            let (k', e) = p in
            (match X.compare k k' with
             | OrderedType.LT -> false
             | OrderedType.EQ -> true
             | OrderedType.GT -> mem k l)
          
          type 'elt coq_R_mem =
          | R_mem_0 of 'elt t
          | R_mem_1 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_mem_2 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_mem_3 of 'elt t * X.t * 'elt * (X.t * 'elt) list * bool
             * 'elt coq_R_mem
          
          (** val coq_R_mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem ->
              'a2 **)
          
          let rec coq_R_mem_rect k f f0 f1 f2 s b = function
          | R_mem_0 s0 -> f s0 __
          | R_mem_1 (s0, k', _x, l) -> f0 s0 k' _x l __ __ __
          | R_mem_2 (s0, k', _x, l) -> f1 s0 k' _x l __ __ __
          | R_mem_3 (s0, k', _x, l, res, r0) ->
            f2 s0 k' _x l __ __ __ res r0
              (coq_R_mem_rect k f f0 f1 f2 l res r0)
          
          (** val coq_R_mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem ->
              'a2 **)
          
          let rec coq_R_mem_rec k f f0 f1 f2 s b = function
          | R_mem_0 s0 -> f s0 __
          | R_mem_1 (s0, k', _x, l) -> f0 s0 k' _x l __ __ __
          | R_mem_2 (s0, k', _x, l) -> f1 s0 k' _x l __ __ __
          | R_mem_3 (s0, k', _x, l, res, r0) ->
            f2 s0 k' _x l __ __ __ res r0
              (coq_R_mem_rec k f f0 f1 f2 l res r0)
          
          (** val mem_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let rec mem_rect k f2 f1 f0 f s =
            let f3 = f2 s in
            let f4 = f1 s in
            let f5 = f0 s in
            let f6 = f s in
            (match s with
             | [] -> f3 __
             | p :: l ->
               let (t0, e) = p in
               let f7 = f6 t0 e l __ in
               let f8 = fun _ _ ->
                 let hrec = mem_rect k f2 f1 f0 f l in f7 __ __ hrec
               in
               let f9 = f5 t0 e l __ in
               let f10 = f4 t0 e l __ in
               (match X.compare k t0 with
                | OrderedType.LT -> f10 __ __
                | OrderedType.EQ -> f9 __ __
                | OrderedType.GT -> f8 __ __))
          
          (** val mem_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let mem_rec k =
            mem_rect k
          
          (** val coq_R_mem_correct :
              key -> 'a1 t -> bool -> 'a1 coq_R_mem **)
          
          let coq_R_mem_correct x x0 res =
            let princ = fun x1 -> mem_rect x1 in
            Obj.magic princ x (fun y _ z _ -> R_mem_0 y)
              (fun y y0 y1 y2 _ _ _ z _ -> R_mem_1 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ z _ -> R_mem_2 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ y6 z _ -> R_mem_3 (y, y0, y1, y2,
              (mem x y2), (y6 (mem x y2) __))) x0 res __
          
          (** val find : key -> 'a1 t -> 'a1 option **)
          
          let rec find k = function
          | [] -> None
          | p :: s' ->
            let (k', x) = p in
            (match X.compare k k' with
             | OrderedType.LT -> None
             | OrderedType.EQ -> Some x
             | OrderedType.GT -> find k s')
          
          type 'elt coq_R_find =
          | R_find_0 of 'elt t
          | R_find_1 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_find_2 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_find_3 of 'elt t * X.t * 'elt * (X.t * 'elt) list * 'elt option
             * 'elt coq_R_find
          
          (** val coq_R_find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 option
              -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option -> 'a1
              coq_R_find -> 'a2 **)
          
          let rec coq_R_find_rect k f f0 f1 f2 s o = function
          | R_find_0 s0 -> f s0 __
          | R_find_1 (s0, k', x, s') -> f0 s0 k' x s' __ __ __
          | R_find_2 (s0, k', x, s') -> f1 s0 k' x s' __ __ __
          | R_find_3 (s0, k', x, s', res, r0) ->
            f2 s0 k' x s' __ __ __ res r0
              (coq_R_find_rect k f f0 f1 f2 s' res r0)
          
          (** val coq_R_find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 option
              -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t -> 'a1 option -> 'a1
              coq_R_find -> 'a2 **)
          
          let rec coq_R_find_rec k f f0 f1 f2 s o = function
          | R_find_0 s0 -> f s0 __
          | R_find_1 (s0, k', x, s') -> f0 s0 k' x s' __ __ __
          | R_find_2 (s0, k', x, s') -> f1 s0 k' x s' __ __ __
          | R_find_3 (s0, k', x, s', res, r0) ->
            f2 s0 k' x s' __ __ __ res r0
              (coq_R_find_rec k f f0 f1 f2 s' res r0)
          
          (** val find_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let rec find_rect k f2 f1 f0 f s =
            let f3 = f2 s in
            let f4 = f1 s in
            let f5 = f0 s in
            let f6 = f s in
            (match s with
             | [] -> f3 __
             | p :: l ->
               let (t0, e) = p in
               let f7 = f6 t0 e l __ in
               let f8 = fun _ _ ->
                 let hrec = find_rect k f2 f1 f0 f l in f7 __ __ hrec
               in
               let f9 = f5 t0 e l __ in
               let f10 = f4 t0 e l __ in
               (match X.compare k t0 with
                | OrderedType.LT -> f10 __ __
                | OrderedType.EQ -> f9 __ __
                | OrderedType.GT -> f8 __ __))
          
          (** val find_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let find_rec k =
            find_rect k
          
          (** val coq_R_find_correct :
              key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find **)
          
          let coq_R_find_correct x x0 res =
            let princ = fun x1 -> find_rect x1 in
            Obj.magic princ x (fun y _ z _ -> R_find_0 y)
              (fun y y0 y1 y2 _ _ _ z _ -> R_find_1 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ z _ -> R_find_2 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ y6 z _ -> R_find_3 (y, y0, y1, y2,
              (find x y2), (y6 (find x y2) __))) x0 res __
          
          (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)
          
          let rec add k x s = match s with
          | [] -> (k, x) :: []
          | p :: l ->
            let (k', y) = p in
            (match X.compare k k' with
             | OrderedType.LT -> (k, x) :: s
             | OrderedType.EQ -> (k, x) :: l
             | OrderedType.GT -> (k', y) :: (add k x l))
          
          type 'elt coq_R_add =
          | R_add_0 of 'elt t
          | R_add_1 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_add_2 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_add_3 of 'elt t * X.t * 'elt * (X.t * 'elt) list * 'elt t
             * 'elt coq_R_add
          
          (** val coq_R_add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 t ->
              'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
              -> 'a2 **)
          
          let rec coq_R_add_rect k x f f0 f1 f2 s t0 = function
          | R_add_0 s0 -> f s0 __
          | R_add_1 (s0, k', y, l) -> f0 s0 k' y l __ __ __
          | R_add_2 (s0, k', y, l) -> f1 s0 k' y l __ __ __
          | R_add_3 (s0, k', y, l, res, r0) ->
            f2 s0 k' y l __ __ __ res r0
              (coq_R_add_rect k x f f0 f1 f2 l res r0)
          
          (** val coq_R_add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 t ->
              'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add
              -> 'a2 **)
          
          let rec coq_R_add_rec k x f f0 f1 f2 s t0 = function
          | R_add_0 s0 -> f s0 __
          | R_add_1 (s0, k', y, l) -> f0 s0 k' y l __ __ __
          | R_add_2 (s0, k', y, l) -> f1 s0 k' y l __ __ __
          | R_add_3 (s0, k', y, l, res, r0) ->
            f2 s0 k' y l __ __ __ res r0
              (coq_R_add_rec k x f f0 f1 f2 l res r0)
          
          (** val add_rect :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let rec add_rect k x f2 f1 f0 f s =
            let f3 = f2 s in
            let f4 = f1 s in
            let f5 = f0 s in
            let f6 = f s in
            (match s with
             | [] -> f3 __
             | p :: l ->
               let (t0, e) = p in
               let f7 = f6 t0 e l __ in
               let f8 = fun _ _ ->
                 let hrec = add_rect k x f2 f1 f0 f l in f7 __ __ hrec
               in
               let f9 = f5 t0 e l __ in
               let f10 = f4 t0 e l __ in
               (match X.compare k t0 with
                | OrderedType.LT -> f10 __ __
                | OrderedType.EQ -> f9 __ __
                | OrderedType.GT -> f8 __ __))
          
          (** val add_rec :
              key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let add_rec k x =
            add_rect k x
          
          (** val coq_R_add_correct :
              key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add **)
          
          let coq_R_add_correct x x0 x1 res =
            add_rect x x0 (fun y _ z _ -> R_add_0 y)
              (fun y y0 y1 y2 _ _ _ z _ -> R_add_1 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ z _ -> R_add_2 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ y6 z _ -> R_add_3 (y, y0, y1, y2,
              (add x x0 y2), (y6 (add x x0 y2) __))) x1 res __
          
          (** val remove : key -> 'a1 t -> 'a1 t **)
          
          let rec remove k s = match s with
          | [] -> []
          | p :: l ->
            let (k', x) = p in
            (match X.compare k k' with
             | OrderedType.LT -> s
             | OrderedType.EQ -> l
             | OrderedType.GT -> (k', x) :: (remove k l))
          
          type 'elt coq_R_remove =
          | R_remove_0 of 'elt t
          | R_remove_1 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_remove_2 of 'elt t * X.t * 'elt * (X.t * 'elt) list
          | R_remove_3 of 'elt t * X.t * 'elt * (X.t * 'elt) list * 'elt t
             * 'elt coq_R_remove
          
          (** val coq_R_remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 t ->
              'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
              coq_R_remove -> 'a2 **)
          
          let rec coq_R_remove_rect k f f0 f1 f2 s t0 = function
          | R_remove_0 s0 -> f s0 __
          | R_remove_1 (s0, k', x, l) -> f0 s0 k' x l __ __ __
          | R_remove_2 (s0, k', x, l) -> f1 s0 k' x l __ __ __
          | R_remove_3 (s0, k', x, l, res, r0) ->
            f2 s0 k' x l __ __ __ res r0
              (coq_R_remove_rect k f f0 f1 f2 l res r0)
          
          (** val coq_R_remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a1 t ->
              'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1
              coq_R_remove -> 'a2 **)
          
          let rec coq_R_remove_rec k f f0 f1 f2 s t0 = function
          | R_remove_0 s0 -> f s0 __
          | R_remove_1 (s0, k', x, l) -> f0 s0 k' x l __ __ __
          | R_remove_2 (s0, k', x, l) -> f1 s0 k' x l __ __ __
          | R_remove_3 (s0, k', x, l, res, r0) ->
            f2 s0 k' x l __ __ __ res r0
              (coq_R_remove_rec k f f0 f1 f2 l res r0)
          
          (** val remove_rect :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let rec remove_rect k f2 f1 f0 f s =
            let f3 = f2 s in
            let f4 = f1 s in
            let f5 = f0 s in
            let f6 = f s in
            (match s with
             | [] -> f3 __
             | p :: l ->
               let (t0, e) = p in
               let f7 = f6 t0 e l __ in
               let f8 = fun _ _ ->
                 let hrec = remove_rect k f2 f1 f0 f l in f7 __ __ hrec
               in
               let f9 = f5 t0 e l __ in
               let f10 = f4 t0 e l __ in
               (match X.compare k t0 with
                | OrderedType.LT -> f10 __ __
                | OrderedType.EQ -> f9 __ __
                | OrderedType.GT -> f8 __ __))
          
          (** val remove_rec :
              key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t ->
              'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2) -> ('a1 t ->
              X.t -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2)
              -> 'a1 t -> 'a2 **)
          
          let remove_rec k =
            remove_rect k
          
          (** val coq_R_remove_correct :
              key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove **)
          
          let coq_R_remove_correct x x0 res =
            let princ = fun x1 -> remove_rect x1 in
            Obj.magic princ x (fun y _ z _ -> R_remove_0 y)
              (fun y y0 y1 y2 _ _ _ z _ -> R_remove_1 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ z _ -> R_remove_2 (y, y0, y1, y2))
              (fun y y0 y1 y2 _ _ _ y6 z _ -> R_remove_3 (y, y0, y1, y2,
              (remove x y2), (y6 (remove x y2) __))) x0 res __
          
          (** val elements : 'a1 t -> 'a1 t **)
          
          let elements m =
            m
          
          (** val fold :
              (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)
          
          let rec fold f m acc =
            match m with
            | [] -> acc
            | p :: m' -> let (k, e) = p in fold f m' (f k e acc)
          
          type ('elt, 'a) coq_R_fold =
          | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
          | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * X.t * 
             'elt * (X.t * 'elt) list * 'a * ('elt, 'a) coq_R_fold
          
          (** val coq_R_fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 ->
              'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 ->
              ('a1, 'a3) coq_R_fold -> 'a2 **)
          
          let rec coq_R_fold_rect f f0 f1 m acc a = function
          | R_fold_0 (f2, m0, acc0) -> Obj.magic f __ f2 m0 acc0 __
          | R_fold_1 (f2, m0, acc0, k, e, m', res, r0) ->
            Obj.magic f0 __ f2 m0 acc0 k e m' __ res r0
              (coq_R_fold_rect f f0 f2 m' (f2 k e acc0) res r0)
          
          (** val coq_R_fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 ->
              'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 ->
              ('a1, 'a3) coq_R_fold -> 'a2 **)
          
          let rec coq_R_fold_rec f f0 f1 m acc a = function
          | R_fold_0 (f2, m0, acc0) -> Obj.magic f __ f2 m0 acc0 __
          | R_fold_1 (f2, m0, acc0, k, e, m', res, r0) ->
            Obj.magic f0 __ f2 m0 acc0 k e m' __ res r0
              (coq_R_fold_rec f f0 f2 m' (f2 k e acc0) res r0)
          
          (** val fold_rect :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 ->
              'a3) -> 'a1 t -> 'a3 -> 'a2 **)
          
          let rec fold_rect f0 f f1 m acc =
            let f2 = Obj.magic f0 __ f1 m acc in
            let f3 = Obj.magic f __ f1 m acc in
            (match m with
             | [] -> f2 __
             | p :: l ->
               let (t0, e) = p in
               let f4 = f3 t0 e l __ in
               let hrec = fold_rect f0 f f1 l (f1 t0 e acc) in f4 hrec)
          
          (** val fold_rec :
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) ->
              (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 ->
              'a3) -> 'a1 t -> 'a3 -> 'a2 **)
          
          let fold_rec f f0 f1 m acc =
            fold_rect f f0 f1 m acc
          
          (** val coq_R_fold_correct :
              (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
              coq_R_fold **)
          
          let coq_R_fold_correct x0 x1 x2 res =
            let princ = fun x x3 -> fold_rect x x3 in
            Obj.magic princ (fun _ y0 y1 y2 _ z _ -> R_fold_0 (y0, y1, y2))
              (fun _ y0 y1 y2 y3 y4 y5 _ y7 z _ -> R_fold_1 (y0, y1, y2, y3,
              y4, y5, (fold y0 y5 (y0 y3 y4 y2)),
              (y7 (fold y0 y5 (y0 y3 y4 y2)) __))) x0 x1 x2 res __
          
          (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)
          
          let rec equal cmp m m' =
            match m with
            | [] ->
              (match m' with
               | [] -> true
               | p :: l -> false)
            | p :: l ->
              let (x, e) = p in
              (match m' with
               | [] -> false
               | p0 :: l' ->
                 let (x', e') = p0 in
                 (match X.compare x x' with
                  | OrderedType.EQ -> (&&) (cmp e e') (equal cmp l l')
                  | _ -> false))
          
          type 'elt coq_R_equal =
          | R_equal_0 of 'elt t * 'elt t
          | R_equal_1 of 'elt t * 'elt t * X.t * 'elt * (X.t * 'elt) list
             * X.t * 'elt * (X.t * 'elt) list * bool * 'elt coq_R_equal
          | R_equal_2 of 'elt t * 'elt t * X.t * 'elt * (X.t * 'elt) list
             * X.t * 'elt * (X.t * 'elt) list * X.t OrderedType.coq_Compare
          | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
          
          (** val coq_R_equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> X.t -> 'a1 -> (X.t * 'a1) list -> __
              -> X.t OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t ->
              'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t ->
              'a1 t -> bool -> 'a1 coq_R_equal -> 'a2 **)
          
          let rec coq_R_equal_rect cmp f f0 f1 f2 m m' b = function
          | R_equal_0 (m0, m'0) -> f m0 m'0 __ __
          | R_equal_1 (m0, m'0, x, e, l, x', e', l', res, r0) ->
            f0 m0 m'0 x e l __ x' e' l' __ __ __ res r0
              (coq_R_equal_rect cmp f f0 f1 f2 l l' res r0)
          | R_equal_2 (m0, m'0, x, e, l, x', e', l', _x) ->
            f1 m0 m'0 x e l __ x' e' l' __ _x __ __
          | R_equal_3 (m0, m'0, _x, _x0) -> f2 m0 m'0 _x __ _x0 __ __
          
          (** val coq_R_equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> bool -> 'a1
              coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t -> X.t -> 'a1 ->
              (X.t * 'a1) list -> __ -> X.t -> 'a1 -> (X.t * 'a1) list -> __
              -> X.t OrderedType.coq_Compare -> __ -> __ -> 'a2) -> ('a1 t ->
              'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t ->
              'a1 t -> bool -> 'a1 coq_R_equal -> 'a2 **)
          
          let rec coq_R_equal_rec cmp f f0 f1 f2 m m' b = function
          | R_equal_0 (m0, m'0) -> f m0 m'0 __ __
          | R_equal_1 (m0, m'0, x, e, l, x', e', l', res, r0) ->
            f0 m0 m'0 x e l __ x' e' l' __ __ __ res r0
              (coq_R_equal_rec cmp f f0 f1 f2 l l' res r0)
          | R_equal_2 (m0, m'0, x, e, l, x', e', l', _x) ->
            f1 m0 m'0 x e l __ x' e' l' __ _x __ __
          | R_equal_3 (m0, m'0, _x, _x0) -> f2 m0 m'0 _x __ _x0 __ __
          
          (** val equal_rect :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> X.t OrderedType.coq_Compare
              -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
              -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2 **)
          
          let rec equal_rect cmp f2 f1 f0 f m m' =
            let f3 = f2 m m' in
            let f4 = f1 m m' in
            let f5 = f0 m m' in
            let f6 = f m m' in
            let f7 = f6 m __ in
            let f8 = f7 m' __ in
            (match m with
             | [] ->
               let f9 = f3 __ in
               (match m' with
                | [] -> f9 __
                | p :: l -> f8 __)
             | p :: l ->
               let (t0, e) = p in
               let f9 = f5 t0 e l __ in
               let f10 = f4 t0 e l __ in
               (match m' with
                | [] -> f8 __
                | p0 :: l0 ->
                  let (t1, e0) = p0 in
                  let f11 = f9 t1 e0 l0 __ in
                  let f12 = let _x = X.compare t0 t1 in f11 _x __ in
                  let f13 = f10 t1 e0 l0 __ in
                  let f14 = fun _ _ ->
                    let hrec = equal_rect cmp f2 f1 f0 f l l0 in
                    f13 __ __ hrec
                  in
                  (match X.compare t0 t1 with
                   | OrderedType.EQ -> f14 __ __
                   | _ -> f12 __)))
          
          (** val equal_rec :
              ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> __ -> __ -> 'a2 -> 'a2) ->
              ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t * 'a1) list -> __ -> X.t
              -> 'a1 -> (X.t * 'a1) list -> __ -> X.t OrderedType.coq_Compare
              -> __ -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t
              -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2 **)
          
          let equal_rec cmp =
            equal_rect cmp
          
          (** val coq_R_equal_correct :
              ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1
              coq_R_equal **)
          
          let coq_R_equal_correct x x0 x1 res =
            equal_rect x (fun y y0 _ _ z _ -> R_equal_0 (y, y0))
              (fun y y0 y1 y2 y3 _ y5 y6 y7 _ _ _ y11 z _ -> R_equal_1 (y,
              y0, y1, y2, y3, y5, y6, y7, (equal x y3 y7),
              (y11 (equal x y3 y7) __)))
              (fun y y0 y1 y2 y3 _ y5 y6 y7 _ y9 _ _ z _ -> R_equal_2 (y, y0,
              y1, y2, y3, y5, y6, y7, y9)) (fun y y0 y1 _ y3 _ _ z _ ->
              R_equal_3 (y, y0, y1, y3)) x0 x1 res __
          
          (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)
          
          let rec map f = function
          | [] -> []
          | p :: m' -> let (k, e) = p in (k, (f e)) :: (map f m')
          
          (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)
          
          let rec mapi f = function
          | [] -> []
          | p :: m' -> let (k, e) = p in (k, (f k e)) :: (mapi f m')
          
          (** val option_cons :
              key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list **)
          
          let option_cons k o l =
            match o with
            | Some e -> (k, e) :: l
            | None -> l
          
          (** val map2_l :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)
          
          let rec map2_l f = function
          | [] -> []
          | p :: l ->
            let (k, e) = p in option_cons k (f (Some e) None) (map2_l f l)
          
          (** val map2_r :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)
          
          let rec map2_r f = function
          | [] -> []
          | p :: l' ->
            let (k, e') = p in option_cons k (f None (Some e')) (map2_r f l')
          
          (** val map2 :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
              'a3 t **)
          
          let rec map2 f m = match m with
          | [] -> map2_r f
          | p :: l ->
            let (k, e) = p in
            let rec map2_aux m' = match m' with
            | [] -> map2_l f m
            | p0 :: l' ->
              let (k', e') = p0 in
              (match X.compare k k' with
               | OrderedType.LT ->
                 option_cons k (f (Some e) None) (map2 f l m')
               | OrderedType.EQ ->
                 option_cons k (f (Some e) (Some e')) (map2 f l l')
               | OrderedType.GT ->
                 option_cons k' (f None (Some e')) (map2_aux l'))
            in map2_aux
          
          (** val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t **)
          
          let rec combine m = match m with
          | [] -> map (fun e' -> (None, (Some e')))
          | p :: l ->
            let (k, e) = p in
            let rec combine_aux m' = match m' with
            | [] -> map (fun e0 -> ((Some e0), None)) m
            | p0 :: l' ->
              let (k', e') = p0 in
              (match X.compare k k' with
               | OrderedType.LT -> (k, ((Some e), None)) :: (combine l m')
               | OrderedType.EQ ->
                 (k, ((Some e), (Some e'))) :: (combine l l')
               | OrderedType.GT ->
                 (k', (None, (Some e'))) :: (combine_aux l'))
            in combine_aux
          
          (** val fold_right_pair :
              ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3 **)
          
          let fold_right_pair f l i =
            fold_right (fun p -> f (fst p) (snd p)) i l
          
          (** val map2_alt :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
              (key * 'a3) list **)
          
          let map2_alt f m m' =
            let m0 = combine m m' in
            let m1 = map (fun p -> f (fst p) (snd p)) m0 in
            fold_right_pair option_cons m1 []
          
          (** val at_least_one :
              'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option **)
          
          let at_least_one o o' =
            match o with
            | Some e -> Some (o, o')
            | None ->
              (match o' with
               | Some e -> Some (o, o')
               | None -> None)
          
          (** val at_least_one_then_f :
              ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
              option -> 'a3 option **)
          
          let at_least_one_then_f f o o' =
            match o with
            | Some e -> f o o'
            | None ->
              (match o' with
               | Some e -> f o o'
               | None -> None)
         end
        
        type 'elt coq_R_mem =
        | R_mem_0 of 'elt tree
        | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * bool * 'elt coq_R_mem
        | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * bool * 'elt coq_R_mem
        
        (** val coq_R_mem_rect :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool ->
            'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1
            tree -> bool -> 'a1 coq_R_mem -> 'a2 **)
        
        let rec coq_R_mem_rect x f f0 f1 f2 m b = function
        | R_mem_0 m0 -> f m0 __
        | R_mem_1 (m0, l, y, _x, r0, _x0, res, r1) ->
          f0 m0 l y _x r0 _x0 __ __ __ res r1
            (coq_R_mem_rect x f f0 f1 f2 l res r1)
        | R_mem_2 (m0, l, y, _x, r0, _x0) -> f1 m0 l y _x r0 _x0 __ __ __
        | R_mem_3 (m0, l, y, _x, r0, _x0, res, r1) ->
          f2 m0 l y _x r0 _x0 __ __ __ res r1
            (coq_R_mem_rect x f f0 f1 f2 r0 res r1)
        
        (** val coq_R_mem_rec :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> bool ->
            'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1
            tree -> bool -> 'a1 coq_R_mem -> 'a2 **)
        
        let rec coq_R_mem_rec x f f0 f1 f2 m b = function
        | R_mem_0 m0 -> f m0 __
        | R_mem_1 (m0, l, y, _x, r0, _x0, res, r1) ->
          f0 m0 l y _x r0 _x0 __ __ __ res r1
            (coq_R_mem_rec x f f0 f1 f2 l res r1)
        | R_mem_2 (m0, l, y, _x, r0, _x0) -> f1 m0 l y _x r0 _x0 __ __ __
        | R_mem_3 (m0, l, y, _x, r0, _x0, res, r1) ->
          f2 m0 l y _x r0 _x0 __ __ __ res r1
            (coq_R_mem_rec x f f0 f1 f2 r0 res r1)
        
        type 'elt coq_R_find =
        | R_find_0 of 'elt tree
        | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
        | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
        
        (** val coq_R_find_rect :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option
            -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key
            -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2)
            -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
        
        let rec coq_R_find_rect x f f0 f1 f2 m o = function
        | R_find_0 m0 -> f m0 __
        | R_find_1 (m0, l, y, d, r0, _x, res, r1) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_find_rect x f f0 f1 f2 l res r1)
        | R_find_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_find_3 (m0, l, y, d, r0, _x, res, r1) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_find_rect x f f0 f1 f2 r0 res r1)
        
        (** val coq_R_find_rec :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option
            -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key
            -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
            ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t
            -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2)
            -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
        
        let rec coq_R_find_rec x f f0 f1 f2 m o = function
        | R_find_0 m0 -> f m0 __
        | R_find_1 (m0, l, y, d, r0, _x, res, r1) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_find_rec x f f0 f1 f2 l res r1)
        | R_find_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_find_3 (m0, l, y, d, r0, _x, res, r1) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_find_rec x f f0 f1 f2 r0 res r1)
        
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
        
        (** val coq_R_bal_rect :
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
            'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)
        
        let coq_R_bal_rect f f0 f1 f2 f3 f4 f5 f6 f7 l x d r t0 = function
        | R_bal_0 (x0, x1, x2, x3) -> f x0 x1 x2 x3 __ __ __
        | R_bal_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f0 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __
        | R_bal_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f1 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ __
        | R_bal_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
          f2 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13 __
        | R_bal_4 (x0, x1, x2, x3) -> f3 x0 x1 x2 x3 __ __ __ __ __
        | R_bal_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f4 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __
        | R_bal_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f5 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ __
        | R_bal_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
          f6 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12
            x13 __
        | R_bal_8 (x0, x1, x2, x3) -> f7 x0 x1 x2 x3 __ __ __ __
        
        (** val coq_R_bal_rec :
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
            'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)
        
        let coq_R_bal_rec f f0 f1 f2 f3 f4 f5 f6 f7 l x d r t0 = function
        | R_bal_0 (x0, x1, x2, x3) -> f x0 x1 x2 x3 __ __ __
        | R_bal_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f0 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __
        | R_bal_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f1 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ __
        | R_bal_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
          f2 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13 __
        | R_bal_4 (x0, x1, x2, x3) -> f3 x0 x1 x2 x3 __ __ __ __ __
        | R_bal_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f4 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __
        | R_bal_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
          f5 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ __
        | R_bal_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
          f6 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12
            x13 __
        | R_bal_8 (x0, x1, x2, x3) -> f7 x0 x1 x2 x3 __ __ __ __
        
        type 'elt coq_R_add =
        | R_add_0 of 'elt tree
        | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
        | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
        
        (** val coq_R_add_rect :
            key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
            'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2 **)
        
        let rec coq_R_add_rect x d f f0 f1 f2 m t0 = function
        | R_add_0 m0 -> f m0 __
        | R_add_1 (m0, l, y, d', r0, h, res, r1) ->
          f0 m0 l y d' r0 h __ __ __ res r1
            (coq_R_add_rect x d f f0 f1 f2 l res r1)
        | R_add_2 (m0, l, y, d', r0, h) -> f1 m0 l y d' r0 h __ __ __
        | R_add_3 (m0, l, y, d', r0, h, res, r1) ->
          f2 m0 l y d' r0 h __ __ __ res r1
            (coq_R_add_rect x d f f0 f1 f2 r0 res r1)
        
        (** val coq_R_add_rec :
            key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
            tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
            'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2 **)
        
        let rec coq_R_add_rec x d f f0 f1 f2 m t0 = function
        | R_add_0 m0 -> f m0 __
        | R_add_1 (m0, l, y, d', r0, h, res, r1) ->
          f0 m0 l y d' r0 h __ __ __ res r1
            (coq_R_add_rec x d f f0 f1 f2 l res r1)
        | R_add_2 (m0, l, y, d', r0, h) -> f1 m0 l y d' r0 h __ __ __
        | R_add_3 (m0, l, y, d', r0, h, res, r1) ->
          f2 m0 l y d' r0 h __ __ __ res r1
            (coq_R_add_rec x d f f0 f1 f2 r0 res r1)
        
        type 'elt coq_R_remove_min =
        | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
        | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
           * key * 'elt * 'elt tree * Int.Z_as_Int.t
           * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min * 'elt tree
           * (key * 'elt)
        
        (** val coq_R_remove_min_rect :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
            key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
            coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2)
            -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1))
            -> 'a1 coq_R_remove_min -> 'a2 **)
        
        let rec coq_R_remove_min_rect f f0 l x d r p = function
        | R_remove_min_0 (l0, x0, d0, r1) -> f l0 x0 d0 r1 __
        | R_remove_min_1 (l0, x0, d0, r1, ll, lx, ld, lr, _x, res, r2, l', m) ->
          f0 l0 x0 d0 r1 ll lx ld lr _x __ res r2
            (coq_R_remove_min_rect f f0 ll lx ld lr res r2) l' m __
        
        (** val coq_R_remove_min_rec :
            ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
            key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
            coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2)
            -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1))
            -> 'a1 coq_R_remove_min -> 'a2 **)
        
        let rec coq_R_remove_min_rec f f0 l x d r p = function
        | R_remove_min_0 (l0, x0, d0, r1) -> f l0 x0 d0 r1 __
        | R_remove_min_1 (l0, x0, d0, r1, ll, lx, ld, lr, _x, res, r2, l', m) ->
          f0 l0 x0 d0 r1 ll lx ld lr _x __ res r2
            (coq_R_remove_min_rec f f0 ll lx ld lr res r2) l' m __
        
        type 'elt coq_R_merge =
        | R_merge_0 of 'elt tree * 'elt tree
        | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt
        
        (** val coq_R_merge_rect :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
            'a1 coq_R_merge -> 'a2 **)
        
        let coq_R_merge_rect f f0 f1 s1 s2 t0 = function
        | R_merge_0 (x, x0) -> f x x0 __
        | R_merge_1 (x, x0, x1, x2, x3, x4, x5) ->
          f0 x x0 x1 x2 x3 x4 x5 __ __
        | R_merge_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
                     x12, x13, x14) ->
          f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __ x13 x14 __
        
        (** val coq_R_merge_rec :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
            'a1 coq_R_merge -> 'a2 **)
        
        let coq_R_merge_rec f f0 f1 s1 s2 t0 = function
        | R_merge_0 (x, x0) -> f x x0 __
        | R_merge_1 (x, x0, x1, x2, x3, x4, x5) ->
          f0 x x0 x1 x2 x3 x4 x5 __ __
        | R_merge_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
                     x12, x13, x14) ->
          f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __ x13 x14 __
        
        type 'elt coq_R_remove =
        | R_remove_0 of 'elt tree
        | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        
        (** val coq_R_remove_rect :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree
            -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)
        
        let rec coq_R_remove_rect x f f0 f1 f2 m t0 = function
        | R_remove_0 m0 -> f m0 __
        | R_remove_1 (m0, l, y, d, r0, _x, res, r1) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_remove_rect x f f0 f1 f2 l res r1)
        | R_remove_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_remove_3 (m0, l, y, d, r0, _x, res, r1) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_remove_rect x f f0 f1 f2 r0 res r1)
        
        (** val coq_R_remove_rec :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree
            -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
            key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
            'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
            Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
            -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)
        
        let rec coq_R_remove_rec x f f0 f1 f2 m t0 = function
        | R_remove_0 m0 -> f m0 __
        | R_remove_1 (m0, l, y, d, r0, _x, res, r1) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_remove_rec x f f0 f1 f2 l res r1)
        | R_remove_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_remove_3 (m0, l, y, d, r0, _x, res, r1) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_remove_rec x f f0 f1 f2 r0 res r1)
        
        type 'elt coq_R_concat =
        | R_concat_0 of 'elt tree * 'elt tree
        | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt)
        
        (** val coq_R_concat_rect :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat ->
            'a2 **)
        
        let coq_R_concat_rect f f0 f1 m1 m2 t0 = function
        | R_concat_0 (x, x0) -> f x x0 __
        | R_concat_1 (x, x0, x1, x2, x3, x4, x5) ->
          f0 x x0 x1 x2 x3 x4 x5 __ __
        | R_concat_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
                      x12) ->
          f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __
        
        (** val coq_R_concat_rec :
            ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
            'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
            -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
            tree -> Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ ->
            'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat ->
            'a2 **)
        
        let coq_R_concat_rec f f0 f1 m1 m2 t0 = function
        | R_concat_0 (x, x0) -> f x x0 __
        | R_concat_1 (x, x0, x1, x2, x3, x4, x5) ->
          f0 x x0 x1 x2 x3 x4 x5 __ __
        | R_concat_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
                      x12) ->
          f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __
        
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
        
        (** val coq_R_split_rect :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple
            -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
            coq_R_split -> 'a2 **)
        
        let rec coq_R_split_rect x f f0 f1 f2 m t0 = function
        | R_split_0 m0 -> f m0 __
        | R_split_1 (m0, l, y, d, r0, _x, res, r1, ll, o, rl) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_split_rect x f f0 f1 f2 l res r1) ll o rl __
        | R_split_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_split_3 (m0, l, y, d, r0, _x, res, r1, rl, o, rr) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_split_rect x f f0 f1 f2 r0 res r1) rl o rr __
        
        (** val coq_R_split_rec :
            X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
            'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple
            -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree
            -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
            -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
            tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
            __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
            option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
            coq_R_split -> 'a2 **)
        
        let rec coq_R_split_rec x f f0 f1 f2 m t0 = function
        | R_split_0 m0 -> f m0 __
        | R_split_1 (m0, l, y, d, r0, _x, res, r1, ll, o, rl) ->
          f0 m0 l y d r0 _x __ __ __ res r1
            (coq_R_split_rec x f f0 f1 f2 l res r1) ll o rl __
        | R_split_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
        | R_split_3 (m0, l, y, d, r0, _x, res, r1, rl, o, rr) ->
          f2 m0 l y d r0 _x __ __ __ res r1
            (coq_R_split_rec x f f0 f1 f2 r0 res r1) rl o rr __
        
        type ('elt, 'elt') coq_R_map_option =
        | R_map_option_0 of 'elt tree
        | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt' * 'elt' tree
           * ('elt, 'elt') coq_R_map_option * 'elt' tree
           * ('elt, 'elt') coq_R_map_option
        | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt' tree * ('elt, 'elt') coq_R_map_option
           * 'elt' tree * ('elt, 'elt') coq_R_map_option
        
        (** val coq_R_map_option_rect :
            (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
            -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
            tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree ->
            'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 **)
        
        let rec coq_R_map_option_rect f f0 f1 f2 m t0 = function
        | R_map_option_0 m0 -> f0 m0 __
        | R_map_option_1 (m0, l, x, d, r0, _x, d', res0, r1, res, r2) ->
          f1 m0 l x d r0 _x __ d' __ res0 r1
            (coq_R_map_option_rect f f0 f1 f2 l res0 r1) res r2
            (coq_R_map_option_rect f f0 f1 f2 r0 res r2)
        | R_map_option_2 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
          f2 m0 l x d r0 _x __ __ res0 r1
            (coq_R_map_option_rect f f0 f1 f2 l res0 r1) res r2
            (coq_R_map_option_rect f f0 f1 f2 r0 res r2)
        
        (** val coq_R_map_option_rec :
            (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
            -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1
            tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t ->
            __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
            tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree ->
            'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 **)
        
        let rec coq_R_map_option_rec f f0 f1 f2 m t0 = function
        | R_map_option_0 m0 -> f0 m0 __
        | R_map_option_1 (m0, l, x, d, r0, _x, d', res0, r1, res, r2) ->
          f1 m0 l x d r0 _x __ d' __ res0 r1
            (coq_R_map_option_rec f f0 f1 f2 l res0 r1) res r2
            (coq_R_map_option_rec f f0 f1 f2 r0 res r2)
        | R_map_option_2 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
          f2 m0 l x d r0 _x __ __ res0 r1
            (coq_R_map_option_rec f f0 f1 f2 l res0 r1) res r2
            (coq_R_map_option_rec f f0 f1 f2 r0 res r2)
        
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
        
        (** val coq_R_map2_opt_rect :
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
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)
        
        let rec coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 m1 m2 t0 = function
        | R_map2_opt_0 (m3, m4) -> f0 m3 m4 __
        | R_map2_opt_1 (m3, m4, l1, x1, d1, r1, _x) ->
          f1 m3 m4 l1 x1 d1 r1 _x __ __
        | R_map2_opt_2 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4,
                        l2', o2, r2', e, res0, r0, res, r2) ->
          f2 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e
            __ res0 r0
            (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res
            r2 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
        | R_map2_opt_3 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4,
                        l2', o2, r2', res0, r0, res, r2) ->
          f3 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __
            res0 r0
            (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res
            r2 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
        
        (** val coq_R_map2_opt_rec :
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
            tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)
        
        let rec coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 m1 m2 t0 = function
        | R_map2_opt_0 (m3, m4) -> f0 m3 m4 __
        | R_map2_opt_1 (m3, m4, l1, x1, d1, r1, _x) ->
          f1 m3 m4 l1 x1 d1 r1 _x __ __
        | R_map2_opt_2 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4,
                        l2', o2, r2', e, res0, r0, res, r2) ->
          f2 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e
            __ res0 r0
            (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res
            r2 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
        | R_map2_opt_3 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4,
                        l2', o2, r2', res0, r0, res, r2) ->
          f3 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __
            res0 r0
            (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res
            r2 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
        
        (** val fold' :
            (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)
        
        let fold' f s =
          L.fold f (elements s)
        
        (** val flatten_e : 'a1 enumeration -> (key * 'a1) list **)
        
        let rec flatten_e = function
        | End -> []
        | More (x, e0, t0, r) -> (x, e0) :: (app (elements t0) (flatten_e r))
       end
     end
    
    type 'elt bst =
      'elt Raw.tree
      (* singleton inductive, whose constructor was Bst *)
    
    (** val bst_rect : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2 **)
    
    let bst_rect f b =
      f b __
    
    (** val bst_rec : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2 **)
    
    let bst_rec f b =
      f b __
    
    (** val this : 'a1 bst -> 'a1 Raw.tree **)
    
    let this b =
      b
    
    type 'elt t = 'elt bst
    
    type key = E.t
    
    (** val empty : 'a1 t **)
    
    let empty =
      Raw.empty
    
    (** val is_empty : 'a1 t -> bool **)
    
    let is_empty m =
      Raw.is_empty (this m)
    
    (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)
    
    let add x e m =
      Raw.add x e (this m)
    
    (** val remove : key -> 'a1 t -> 'a1 t **)
    
    let remove x m =
      Raw.remove x (this m)
    
    (** val mem : key -> 'a1 t -> bool **)
    
    let mem x m =
      Raw.mem x (this m)
    
    (** val find : key -> 'a1 t -> 'a1 option **)
    
    let find x m =
      Raw.find x (this m)
    
    (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)
    
    let map f m =
      Raw.map f (this m)
    
    (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)
    
    let mapi f m =
      Raw.mapi f (this m)
    
    (** val map2 :
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)
    
    let map2 f m m' =
      Raw.map2 f (this m) (this m')
    
    (** val elements : 'a1 t -> (key * 'a1) list **)
    
    let elements m =
      Raw.elements (this m)
    
    (** val cardinal : 'a1 t -> int **)
    
    let cardinal m =
      Raw.cardinal (this m)
    
    (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)
    
    let fold f m i =
      Raw.fold f (this m) i
    
    (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)
    
    let equal cmp m m' =
      Raw.equal cmp (this m) (this m')
    
    module ME = OrderedType.OrderedTypeFacts(E)
    
    module O = OrderedType.KeyOrderedType(E)
    
    module P = 
     struct 
      module F = 
       struct 
        (** val eqb : X.t -> X.t -> bool **)
        
        let eqb x y =
          if E.eq_dec x y then true else false
        
        (** val coq_In_dec : 'a1 t -> key -> bool **)
        
        let coq_In_dec m x =
          let b = mem x m in if b then true else false
        
        (** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)
        
        let option_map f = function
        | Some a -> Some (f a)
        | None -> None
       end
      
      (** val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3 **)
      
      let uncurry f p =
        f (fst p) (snd p)
      
      (** val of_list : (key * 'a1) list -> 'a1 t **)
      
      let of_list l =
        fold_right (uncurry add) empty l
      
      (** val to_list : 'a1 t -> (key * 'a1) list **)
      
      let to_list x =
        elements x
      
      (** val fold_rec :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> __ -> 'a3)
          -> (key -> 'a1 -> 'a2 -> 'a1 t -> 'a1 t -> __ -> __ -> __ -> 'a3 ->
          'a3) -> 'a3 **)
      
      let fold_rec f i m hempty hstep =
        let f0 = uncurry f in
        let l = rev (elements m) in
        let hstep' = fun k e a m' m'' x ->
          hstep (fst (k, e)) (snd (k, e)) a m' m'' __ __ __ x
        in
        let rec f1 l0 hstep'0 m0 =
          match l0 with
          | [] -> hempty m0 __
          | y :: l1 ->
            let (k, e) = y in
            hstep'0 k e (fold_right f0 i l1) (of_list l1) m0 __ __ __
              (f1 l1 (fun k0 e0 a m' m'' _ _ _ x ->
                hstep'0 k0 e0 a m' m'' __ __ __ x) (of_list l1))
        in f1 l (fun k e a m' m'' _ _ _ x -> hstep' k e a m' m'' x) m
      
      (** val fold_rec_bis :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> ('a1 t -> 'a1 t ->
          'a2 -> __ -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t ->
          __ -> __ -> 'a3 -> 'a3) -> 'a3 **)
      
      let fold_rec_bis f i m pmorphism pempty pstep =
        fold_rec f i m (fun m0 _ -> pmorphism empty m0 i __ pempty)
          (fun k e a m' m'' _ _ _ x ->
          pmorphism (add k e m') m'' (f k e a) __ (pstep k e a m' __ __ x))
      
      (** val fold_rec_nodep :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 t -> 'a3 -> (key -> 'a1 ->
          'a2 -> __ -> 'a3 -> 'a3) -> 'a3 **)
      
      let fold_rec_nodep f i m x x0 =
        fold_rec_bis f i m (fun m0 m' a _ x1 -> x1) x (fun k e a m' _ _ x1 ->
          x0 k e a __ x1)
      
      (** val fold_rec_weak :
          (key -> 'a1 -> 'a2 -> 'a2) -> 'a2 -> ('a1 t -> 'a1 t -> 'a2 -> __
          -> 'a3 -> 'a3) -> 'a3 -> (key -> 'a1 -> 'a2 -> 'a1 t -> __ -> 'a3
          -> 'a3) -> 'a1 t -> 'a3 **)
      
      let fold_rec_weak f i x x0 x1 m =
        fold_rec_bis f i m x x0 (fun k e a m' _ _ x2 -> x1 k e a m' __ x2)
      
      (** val fold_rel :
          (key -> 'a1 -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a2 ->
          'a3 -> 'a1 t -> 'a4 -> (key -> 'a1 -> 'a2 -> 'a3 -> __ -> 'a4 ->
          'a4) -> 'a4 **)
      
      let fold_rel f g i j m rempty rstep =
        let l = rev (elements m) in
        let rstep' = fun k e a b x -> rstep k e a b __ x in
        let rec f0 l0 rstep'0 =
          match l0 with
          | [] -> rempty
          | y :: l1 ->
            rstep'0 (fst y) (snd y) (fold_right (uncurry f) i l1)
              (fold_right (uncurry g) j l1) __
              (f0 l1 (fun k e a0 b _ x -> rstep'0 k e a0 b __ x))
        in f0 l (fun k e a b _ x -> rstep' k e a b x)
      
      (** val map_induction :
          ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> key -> 'a1 -> __
          -> __ -> 'a2) -> 'a1 t -> 'a2 **)
      
      let map_induction x x0 m =
        fold_rec (fun x1 x2 x3 -> ()) () m x (fun k e a m' m'' _ _ _ x1 ->
          x0 m' m'' x1 k e __ __)
      
      (** val map_induction_bis :
          ('a1 t -> 'a1 t -> __ -> 'a2 -> 'a2) -> 'a2 -> (key -> 'a1 -> 'a1 t
          -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
      
      let map_induction_bis x x0 x1 m =
        fold_rec_bis (fun x2 x3 x4 -> ()) () m (fun m0 m' a _ x2 ->
          x m0 m' __ x2) x0 (fun k e a m' _ _ x2 -> x1 k e m' __ x2)
      
      (** val cardinal_inv_2 : 'a1 t -> int -> (key * 'a1) **)
      
      let cardinal_inv_2 m n =
        let l = elements m in
        (match l with
         | [] -> assert false (* absurd case *)
         | p :: l0 -> p)
      
      (** val cardinal_inv_2b : 'a1 t -> (key * 'a1) **)
      
      let cardinal_inv_2b m =
        let n = fun _ -> cardinal in
        let x = fun x -> cardinal_inv_2 m x in
        ((fun fO fS n -> if n=0 then fO () else fS (n-1))
           (fun _ -> assert false
           (* absurd case *))
           (fun n0 ->
           x n0)
           (n __ m))
      
      (** val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t **)
      
      let filter f m =
        fold (fun k e m0 -> if f k e then add k e m0 else m0) m empty
      
      (** val for_all : (key -> 'a1 -> bool) -> 'a1 t -> bool **)
      
      let for_all f m =
        fold (fun k e b -> if f k e then b else false) m true
      
      (** val exists_ : (key -> 'a1 -> bool) -> 'a1 t -> bool **)
      
      let exists_ f m =
        fold (fun k e b -> if f k e then true else b) m false
      
      (** val partition : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t * 'a1 t **)
      
      let partition f m =
        ((filter f m), (filter (fun k e -> negb (f k e)) m))
      
      (** val update : 'a1 t -> 'a1 t -> 'a1 t **)
      
      let update m1 m2 =
        fold add m2 m1
      
      (** val restrict : 'a1 t -> 'a1 t -> 'a1 t **)
      
      let restrict m1 m2 =
        filter (fun k x -> mem k m2) m1
      
      (** val diff : 'a1 t -> 'a1 t -> 'a1 t **)
      
      let diff m1 m2 =
        filter (fun k x -> negb (mem k m2)) m1
      
      (** val coq_Partition_In : 'a1 t -> 'a1 t -> 'a1 t -> key -> bool **)
      
      let coq_Partition_In m m1 m2 k =
        F.coq_In_dec m1 k
      
      (** val update_dec : 'a1 t -> 'a1 t -> key -> 'a1 -> bool **)
      
      let update_dec m m' k e =
        F.coq_In_dec m' k
      
      (** val filter_dom : (key -> bool) -> 'a1 t -> 'a1 t **)
      
      let filter_dom f =
        filter (fun k x -> f k)
      
      (** val filter_range : ('a1 -> bool) -> 'a1 t -> 'a1 t **)
      
      let filter_range f =
        filter (fun x -> f)
      
      (** val for_all_dom : (key -> bool) -> 'a1 t -> bool **)
      
      let for_all_dom f =
        for_all (fun k x -> f k)
      
      (** val for_all_range : ('a1 -> bool) -> 'a1 t -> bool **)
      
      let for_all_range f =
        for_all (fun x -> f)
      
      (** val exists_dom : (key -> bool) -> 'a1 t -> bool **)
      
      let exists_dom f =
        exists_ (fun k x -> f k)
      
      (** val exists_range : ('a1 -> bool) -> 'a1 t -> bool **)
      
      let exists_range f =
        exists_ (fun x -> f)
      
      (** val partition_dom : (key -> bool) -> 'a1 t -> 'a1 t * 'a1 t **)
      
      let partition_dom f =
        partition (fun k x -> f k)
      
      (** val partition_range : ('a1 -> bool) -> 'a1 t -> 'a1 t * 'a1 t **)
      
      let partition_range f =
        partition (fun x -> f)
     end
    
    (** val gtb : (key * 'a1) -> (key * 'a1) -> bool **)
    
    let gtb p p' =
      match E.compare (fst p) (fst p') with
      | OrderedType.GT -> true
      | _ -> false
    
    (** val leb : (key * 'a1) -> (key * 'a1) -> bool **)
    
    let leb p p' =
      negb (gtb p p')
    
    (** val elements_lt : (key * 'a1) -> 'a1 t -> (key * 'a1) list **)
    
    let elements_lt p m =
      filter (gtb p) (elements m)
    
    (** val elements_ge : (key * 'a1) -> 'a1 t -> (key * 'a1) list **)
    
    let elements_ge p m =
      filter (leb p) (elements m)
    
    (** val max_elt_aux : (key * 'a1) list -> (key * 'a1) option **)
    
    let rec max_elt_aux = function
    | [] -> None
    | p :: l0 ->
      (match l0 with
       | [] -> Some p
       | p0 :: l1 -> max_elt_aux l0)
    
    (** val max_elt : 'a1 t -> (key * 'a1) option **)
    
    let max_elt m =
      max_elt_aux (elements m)
    
    (** val min_elt : 'a1 t -> (key * 'a1) option **)
    
    let min_elt m =
      match elements m with
      | [] -> None
      | p :: l -> Some p
    
    (** val map_induction_max :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> X.t -> 'a1 -> __ ->
        __ -> 'a2) -> 'a1 t -> 'a2 **)
    
    let map_induction_max x x0 m =
      let n = cardinal m in
      let rec f n0 m0 =
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ ->
          x m0 __)
          (fun n1 ->
          match max_elt m0 with
          | Some p ->
            let (k, e) = p in
            x0 (remove k m0) m0 (f n1 (remove k m0)) k e __ __
          | None -> x m0 __)
          n0
      in f n m
    
    (** val map_induction_min :
        ('a1 t -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a2 -> X.t -> 'a1 -> __ ->
        __ -> 'a2) -> 'a1 t -> 'a2 **)
    
    let map_induction_min x x0 m =
      let n = cardinal m in
      let rec f n0 m0 =
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ ->
          x m0 __)
          (fun n1 ->
          match min_elt m0 with
          | Some p ->
            let (k, e) = p in
            x0 (remove k m0) m0 (f n1 (remove k m0)) k e __ __
          | None -> x m0 __)
          n0
      in f n m
    
    module Raw2 = 
     struct 
      (** val for_all :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 Raw.tree ->
          bool **)
      
      let rec for_all print2 cond = function
      | Raw.Leaf -> true
      | Raw.Node (l, x, d, r, t0) ->
        (&&)
          ((&&) (for_all print2 cond l)
            (print_when_false (print2 x) d (cond x) d))
          (for_all print2 cond r)
      
      (** val strong_le :
          ('a1 -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree -> bool **)
      
      let rec strong_le elt_le m1 m2 =
        (||) (physical_eq m1 m2)
          (match m1 with
           | Raw.Leaf -> true
           | Raw.Node (l1, x1, d1, r1, t0) ->
             (match m2 with
              | Raw.Leaf -> false
              | Raw.Node (l2, x2, d2, r2, t1) ->
                (&&)
                  ((&&)
                    ((&&) (strong_le elt_le l1 l2)
                      (match X.compare x1 x2 with
                       | OrderedType.EQ -> true
                       | _ -> false)) (elt_le d1 d2))
                  (strong_le elt_le r1 r2)))
      
      (** val filter :
          (key -> 'a1 -> bool) -> 'a1 Raw.tree -> 'a1 Raw.tree **)
      
      let rec filter f = function
      | Raw.Leaf -> Raw.Leaf
      | Raw.Node (l, x, d, r, t0) ->
        let l' = filter f l in
        let r' = filter f r in
        if f x d then Raw.join l' x d r' else Raw.concat l' r'
      
      type 'elt coq_R_for_all =
      | R_for_all_0 of 'elt Raw.tree
      | R_for_all_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * bool * 'elt coq_R_for_all * 
         bool * 'elt coq_R_for_all
      
      (** val coq_R_for_all_rect :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
          -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all ->
          'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          bool -> 'a1 coq_R_for_all -> 'a2 **)
      
      let rec coq_R_for_all_rect print2 cond f f0 m b = function
      | R_for_all_0 m0 -> f m0 __
      | R_for_all_1 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
        f0 m0 l x d r0 _x __ res0 r1
          (coq_R_for_all_rect print2 cond f f0 l res0 r1) res r2
          (coq_R_for_all_rect print2 cond f f0 r0 res r2)
      
      (** val coq_R_for_all_rec :
          (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __
          -> 'a2) -> ('a1 Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1
          Raw.tree -> Int.Z_as_Int.t -> __ -> bool -> 'a1 coq_R_for_all ->
          'a2 -> bool -> 'a1 coq_R_for_all -> 'a2 -> 'a2) -> 'a1 Raw.tree ->
          bool -> 'a1 coq_R_for_all -> 'a2 **)
      
      let rec coq_R_for_all_rec print2 cond f f0 m b = function
      | R_for_all_0 m0 -> f m0 __
      | R_for_all_1 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
        f0 m0 l x d r0 _x __ res0 r1
          (coq_R_for_all_rec print2 cond f f0 l res0 r1) res r2
          (coq_R_for_all_rec print2 cond f f0 r0 res r2)
      
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
      
      (** val coq_R_strong_le_rect :
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
          'a1 Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2 **)
      
      let rec coq_R_strong_le_rect elt_le f f0 f1 f2 f3 m1 m2 b = function
      | R_strong_le_0 (m3, m4) -> f m3 m4 __
      | R_strong_le_1 (m3, m4, l1, x1, d1, r1, _x) ->
        f0 m3 m4 l1 x1 d1 r1 _x __ __
      | R_strong_le_2 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f1 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 r1 r2 res r3)
      | R_strong_le_3 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f2 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 r1 r2 res r3)
      | R_strong_le_4 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f3 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rect elt_le f f0 f1 f2 f3 r1 r2 res r3)
      
      (** val coq_R_strong_le_rec :
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
          'a1 Raw.tree -> bool -> 'a1 coq_R_strong_le -> 'a2 **)
      
      let rec coq_R_strong_le_rec elt_le f f0 f1 f2 f3 m1 m2 b = function
      | R_strong_le_0 (m3, m4) -> f m3 m4 __
      | R_strong_le_1 (m3, m4, l1, x1, d1, r1, _x) ->
        f0 m3 m4 l1 x1 d1 r1 _x __ __
      | R_strong_le_2 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f1 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 r1 r2 res r3)
      | R_strong_le_3 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f2 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 r1 r2 res r3)
      | R_strong_le_4 (m3, m4, l1, x1, d1, r1, _x, l2, x2, d2, r2, _x0, res0,
                       r0, res, r3) ->
        f3 m3 m4 l1 x1 d1 r1 _x __ l2 x2 d2 r2 _x0 __ res0 r0
          (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 l1 l2 res0 r0) __ __ res
          r3 (coq_R_strong_le_rec elt_le f f0 f1 f2 f3 r1 r2 res r3)
      
      type 'elt coq_R_filter =
      | R_filter_0 of 'elt Raw.tree
      | R_filter_1 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      | R_filter_2 of 'elt Raw.tree * 'elt Raw.tree * Raw.key * 'elt
         * 'elt Raw.tree * Int.Z_as_Int.t * 'elt Raw.tree * 'elt coq_R_filter
         * 'elt Raw.tree * 'elt coq_R_filter
      
      (** val coq_R_filter_rect :
          (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 **)
      
      let rec coq_R_filter_rect f f0 f1 f2 m t0 = function
      | R_filter_0 m0 -> f0 m0 __
      | R_filter_1 (m0, l, x, d, r0, _x, res, r1, x0, x1) ->
        f1 m0 l x d r0 _x __ res r1 (coq_R_filter_rect f f0 f1 f2 l res r1)
          x0 x1 (coq_R_filter_rect f f0 f1 f2 r0 x0 x1) __
      | R_filter_2 (m0, l, x, d, r0, _x, res, r1, x0, x1) ->
        f2 m0 l x d r0 _x __ res r1 (coq_R_filter_rect f f0 f1 f2 l res r1)
          x0 x1 (coq_R_filter_rect f f0 f1 f2 r0 x0 x1) __
      
      (** val coq_R_filter_rec :
          (key -> 'a1 -> bool) -> ('a1 Raw.tree -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> ('a1
          Raw.tree -> 'a1 Raw.tree -> Raw.key -> 'a1 -> 'a1 Raw.tree ->
          Int.Z_as_Int.t -> __ -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 ->
          'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 -> __ -> 'a2) -> 'a1
          Raw.tree -> 'a1 Raw.tree -> 'a1 coq_R_filter -> 'a2 **)
      
      let rec coq_R_filter_rec f f0 f1 f2 m t0 = function
      | R_filter_0 m0 -> f0 m0 __
      | R_filter_1 (m0, l, x, d, r0, _x, res, r1, x0, x1) ->
        f1 m0 l x d r0 _x __ res r1 (coq_R_filter_rec f f0 f1 f2 l res r1) x0
          x1 (coq_R_filter_rec f f0 f1 f2 r0 x0 x1) __
      | R_filter_2 (m0, l, x, d, r0, _x, res, r1, x0, x1) ->
        f2 m0 l x d r0 _x __ res r1 (coq_R_filter_rec f f0 f1 f2 l res r1) x0
          x1 (coq_R_filter_rec f f0 f1 f2 r0 x0 x1) __
     end
    
    (** val for_all :
        (key -> 'a1 -> unit) -> (key -> 'a1 -> bool) -> 'a1 bst -> bool **)
    
    let for_all print2 cond m =
      Raw2.for_all print2 cond (this m)
    
    (** val strong_le :
        ('a1 -> 'a1 -> bool) -> 'a1 bst -> 'a1 bst -> bool **)
    
    let strong_le elt_le m m' =
      Raw2.strong_le elt_le (this m) (this m')
    
    (** val filter : (key -> 'a1 -> bool) -> 'a1 t -> 'a1 t **)
    
    let filter f m =
      Raw2.filter f (this m)
   end
 end

