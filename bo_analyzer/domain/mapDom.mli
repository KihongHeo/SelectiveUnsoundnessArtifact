(** Map domain *)
open AbsDom
open Vocab

module type S =
sig
  include LAT
  module A : SET
  module B : LAT

  val empty : t
  val is_empty : t -> bool
  val find : A.t -> t -> B.t
  val add : A.t -> B.t -> t -> t
  val weak_add : A.t -> B.t -> t -> t
  val remove : A.t -> t -> t
  val map : (B.t -> B.t) -> t -> t
  val mapi : (A.t -> B.t -> B.t) -> t -> t
  val fold : (B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val foldi : (A.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (A.t -> B.t -> unit) -> t -> unit
  val mem : A.t -> t -> bool
  val filter : (A.t -> B.t -> bool) -> t -> t
  val cardinal : t -> int
  val choose : t -> (A.t * B.t)
  val to_string : t -> string
  val for_all : (A.t -> B.t -> bool) -> t -> bool
  val keys : t -> A.t BatSet.t

  val unstables : t -> t -> (B.t -> B.t -> bool) -> A.t BatSet.t
    -> (A.t * B.t * B.t) list

  val join_pairs : (A.t * B.t) list -> t -> t
  val widen_pairs : (A.t * B.t) list -> t -> t
  val meet_big_small : t -> t -> t
end

module Make (A:SET)(B:LAT) : S with type A.t = A.t and type B.t = B.t 
