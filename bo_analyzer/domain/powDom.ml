(** Powset domain *)
open AbsDom
open Vocab

module type S = 
sig 
  include LAT
  type elt
  exception Error

  val filter : (elt -> bool) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
  val map : (elt -> 'a) -> t -> 'a BatSet.t
  val iter : (elt -> unit) -> t -> unit

  val singleton : elt -> t 
  val subset : elt BatSet.t -> t -> bool
  val cardinal : t -> int

  val mem : elt -> t -> bool

  val add : elt -> t -> t
  val diff : t -> t -> t

  val choose : t -> elt
  
  val remove : elt -> t -> t

  val is_empty : t -> bool

  val for_all : (elt -> bool) -> t -> bool 
  val exists : (elt -> bool) -> t -> bool
  val of_list : elt list -> t

end

module Make (A:SET) : S with type elt = A.t and type t = A.t BatSet.t =
struct
  type elt = A.t
  type t = elt BatSet.t
  
  exception Error
  let compare = BatSet.compare

  let to_string : t -> string = fun x ->
    if BatSet.is_empty x then "bot" else
      string_of_set A.to_string x

  let le : t -> t -> bool = fun x y ->
    if x == y then true else BatSet.subset x y

  let eq : t -> t -> bool = fun x y ->
    if x == y then true else BatSet.equal x y

  let bot : t = BatSet.empty

  let join : t -> t -> t = fun x y ->
    if le x y then y else
    if le y x then x else
      BatSet.union x y

  let meet : t -> t -> t = fun x y ->
    if le x y then x else
    if le y x then y else
      BatSet.intersect x y


  (* Since module A is finite,  widening is defined as union which is
     sufficient to guarantee analysis termination.  *)
  let widen : t -> t -> t = fun x y ->
    if x = y then x else
      BatSet.union x y

  let narrow : t -> t -> t = fun x y ->
    if x = y then x else
      BatSet.intersect x y


  let filter : (elt -> bool) -> t -> t = fun f s ->
    BatSet.filter f s

  let fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a = BatSet.fold
  let fold2 : (elt -> elt -> 'a -> 'a) -> t -> t -> 'a -> 'a 
  = fun f s1 s2 -> BatSet.fold (fun x -> BatSet.fold (f x) s2) s1

  let map = BatSet.map

  let iter : (elt -> unit) -> t -> unit = BatSet.iter

  let singleton : elt -> t = fun e ->
    BatSet.singleton e

  let subset : elt BatSet.t -> t -> bool = BatSet.subset

  let cardinal : t -> int = BatSet.cardinal

  let mem : elt -> t -> bool = BatSet.mem

  let add e s = BatSet.add e s
  let diff = BatSet.diff

  let choose = BatSet.choose
  
  let remove = BatSet.remove

  let is_empty = BatSet.is_empty

  let for_all = BatSet.for_all
  let exists = BatSet.exists
  let of_list = BatSet.of_list
end
