(** Product domain  *)
open AbsDom

module Make4 (A:LAT) (B:LAT) (C:LAT) (D:LAT) : sig
  include AbsDom.LAT
  val make  : (A.t * B.t * C.t * D.t) -> t
  val fst   : t -> A.t
  val snd   : t -> B.t
  val trd   : t -> C.t
  val frth  : t -> D.t
end with type t = A.t * B.t * C.t * D.t

module Make5 (A:LAT) (B:LAT) (C:LAT) (D:LAT) (E:LAT) : sig
  include AbsDom.LAT
  val make  : (A.t * B.t * C.t * D.t * E.t) -> t
  val fst   : t -> A.t
  val snd   : t -> B.t
  val trd   : t -> C.t
  val frth  : t -> D.t
  val fifth : t -> E.t
end with type t = A.t * B.t * C.t * D.t * E.t
