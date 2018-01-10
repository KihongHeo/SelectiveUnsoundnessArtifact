open DLat
open VocabA

(** val size : int **)

let size = try int_of_string (Sys.getenv "NLISTSIZE") with _ -> 6

module NListKey = 
 functor (A:KEY) ->
 struct 
  module F = OrderedType.OrderedTypeFacts(A)
  
  type t = A.t list
  
  (** val compare' : int -> t -> t -> t OrderedType.coq_Compare **)
  
  let rec compare' s x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      OrderedType.EQ)
      (fun s' ->
      match x with
      | [] ->
        (match y with
         | [] -> OrderedType.EQ
         | ty :: y' -> OrderedType.LT)
      | tx :: x' ->
        (match y with
         | [] -> OrderedType.GT
         | ty :: y' ->
           (match A.compare tx ty with
            | OrderedType.LT -> OrderedType.LT
            | OrderedType.EQ -> compare' s' x' y'
            | OrderedType.GT -> OrderedType.GT)))
      s
  
  (** val compare : t -> t -> t OrderedType.coq_Compare **)
  
  let compare x y =
    compare' size x y
  
  (** val eq_dec' : int -> t -> t -> bool **)
  
  let rec eq_dec' s x y =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      true)
      (fun s' ->
      match x with
      | [] ->
        (match y with
         | [] -> true
         | t0 :: l -> false)
      | hd1 :: tl1 ->
        (match y with
         | [] -> false
         | hd2 :: tl2 ->
           if A.eq_dec hd1 hd2 then eq_dec' s' tl1 tl2 else false))
      s
  
  (** val eq_dec : t -> t -> bool **)
  
  let eq_dec x y =
    if physical_eq x y then true else eq_dec' size x y
 end

