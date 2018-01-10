(** Signature for abstract domain *)

(** Signature of set *)
module type SET =
sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
end

module type HASHABLE_SET = 
sig
  include SET
  val equal     : t -> t -> bool
  val hash      : t -> int
end

(** Signature of lattice *)
module type LAT =
sig
  include SET

  val le : t -> t -> bool
  val eq : t -> t -> bool

  val bot : t

  val join : t -> t -> t
  val meet : t -> t -> t

  val widen : t -> t -> t
  val narrow : t -> t -> t
end
