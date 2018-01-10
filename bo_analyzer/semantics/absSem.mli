(** Signature for abstract semantics *)

type update_mode =
  | Weak
  | Strong

module type S = 
sig
  module Dom : MapDom.S
  module Access : Access.ACCESS with type Loc.t = Dom.A.t
  val run : ?mode:update_mode -> ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t 
    -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t
  val accessof : ?mode:update_mode -> ?locset: Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t -> Global.t -> BasicDom.Node.t -> Dom.t -> Access.t
  val do_preanalysis : ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t -> Global.t -> Dom.t * Global.t 
end 
