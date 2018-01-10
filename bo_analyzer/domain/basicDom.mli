(** Basic abstract domains *)
open AbsDom

module Node = InterCfg.Node
module Proc : sig
  include AbsDom.HASHABLE_SET
  val equal : t -> t -> bool
  val hash : t -> int
end with type t = string

module PowProc : PowDom.S with type t = Proc.t BatSet.t and type elt = Proc.t

module Allocsite : sig
  include AbsDom.SET
  val allocsite_of_node : Node.t -> int -> t
  val is_string_allocsite : t -> bool
  val is_global_allocsite : t -> bool
  val is_ext_allocsite : t -> bool
  val is_cmd_arg : t -> bool
  val allocsite_of_ext : string option -> t 
  val allocsite_of_string : Node.t -> int -> t 
end
module Loc : sig
  type t = GVar of string | LVar of Proc.t * string | Allocsite of Allocsite.t 
  | Field of Allocsite.t * field
  and field = string
  val null : t
  val dummy : t
  val is_var : t -> bool
  val is_lvar : t -> bool
  val is_gvar : t -> bool
  val is_allocsite : t -> bool
  val is_field : t -> bool
  val of_gvar : string -> t
  val of_lvar : Proc.t * string -> t
  val append_field : t -> field -> t 
  val of_allocsite : Allocsite.t -> t
  val is_local_of : Proc.t -> t -> bool
  val get_proc : t -> Proc.t
  val to_string : t -> string
  val compare : t -> t -> int
end
module PowLoc : sig
  include PowDom.S
  val null : t
  val prune : Cil.binop -> t -> Cil.exp -> t 
  val append_field : t -> Loc.field -> t
end with type t = Loc.t BatSet.t and type elt = Loc.t 

module Dump : MapDom.S with type A.t = Proc.t and type B.t = PowLoc.t
