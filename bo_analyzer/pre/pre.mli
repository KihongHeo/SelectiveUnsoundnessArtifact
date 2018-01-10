(** Pre-analysis to compute accessibility *)

module type S = 
sig
  module Access : Access.ACCESS
  type t
  val empty : t
  val get_total_abslocs : t -> Access.Loc.t BatSet.t
  val get_access : t -> BasicDom.Node.t -> Access.t
  val get_access_proc : t -> BasicDom.Proc.t -> Access.t
  val get_access_reach : t -> BasicDom.Proc.t -> Access.t
  val get_access_local : t -> BasicDom.Proc.t -> Access.Loc.t BatSet.t
  val get_access_local_program : t -> Access.Loc.t BatSet.t
  val get_defs_of : t -> (Access.Loc.t, BasicDom.Node.t BatSet.t) BatMap.t
  val get_uses_of : t -> (Access.Loc.t, BasicDom.Node.t BatSet.t) BatMap.t
  val get_single_defs : (Access.Loc.t, BasicDom.Node.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  val restrict_access   : t -> Access.Loc.t BatSet.t -> t
  val do_preanalysis : ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t -> Global.t -> t * Global.t
end

module Make(AccessSem : AbsSem.S) : S with type Access.t = AccessSem.Access.t and type Access.Loc.t = AccessSem.Dom.A.t
