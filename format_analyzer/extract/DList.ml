open DLat
open VocabA

module ListKey = 
 functor (A:KEY) ->
 struct 
  module F = OrderedType.OrderedTypeFacts(A)
  
  type t = A.t list
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let rec compare x y =
    match x with
    | [] ->
      (match y with
       | [] -> OrderedType.EQ
       | hd :: tl -> OrderedType.LT)
    | hd1 :: tl1 ->
      (match y with
       | [] -> OrderedType.GT
       | hd2 :: tl2 ->
         (match A.compare hd1 hd2 with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> compare tl1 tl2
          | OrderedType.GT -> OrderedType.GT))
  
  (** val eq_dec : t -> t -> bool **)
  
  let rec eq_dec x y =
    if physical_eq x y
    then true
    else (match x with
          | [] ->
            (match y with
             | [] -> true
             | t0 :: l -> false)
          | hd1 :: tl1 ->
            (match y with
             | [] -> false
             | hd2 :: tl2 ->
               if A.eq_dec hd1 hd2 then eq_dec tl1 tl2 else false))
 end

