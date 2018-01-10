(** Powset domain *)
open AbsDom

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

module Make (A:SET) : S with type elt = A.t and type t = A.t BatSet.t
