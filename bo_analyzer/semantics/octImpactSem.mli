(** Abstract semantics of octagon impact analysis *)
module Dom = OctImpactDom.Mem
module Access : Access.ACCESS with type Loc.t = Dom.A.t

val run_cmd : ?position:int -> AbsSem.update_mode -> BasicDom.Node.t 
  -> IntraCfg.Cmd.t -> ItvDom.Mem.t -> Dom.t * Global.t -> Dom.t

val run: ?mode:AbsSem.update_mode -> ?locset: Access.Loc.t BatSet.t 
  -> ?ptrinfo:ItvDom.Table.t -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t

val accessof: ?mode:AbsSem.update_mode -> ?locset: Access.Loc.t BatSet.t 
  -> ?ptrinfo: ItvDom.Table.t -> Global.t -> BasicDom.Node.t -> Dom.t -> Access.t

val do_preanalysis: ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t 
  -> Global.t -> Dom.t * Global.t 
