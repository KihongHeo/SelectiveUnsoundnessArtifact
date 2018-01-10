(** Abstract semantics of octagon analysis *)
module Dom = OctDom.Mem
module Access : Access.ACCESS with type Loc.t = Dom.A.t

val simplify_exp : Cil.exp -> Cil.exp 

val run_cmd : ?position:int -> AbsSem.update_mode -> OctDom.PackConf.t -> BasicDom.Node.t 
  -> IntraCfg.Cmd.t -> ItvDom.Mem.t -> Dom.t * Global.t -> Dom.t

val run: ?mode:AbsSem.update_mode -> ?locset: Access.Loc.t BatSet.t 
  -> ?ptrinfo:ItvDom.Table.t -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t

val accessof: ?mode:AbsSem.update_mode -> ?locset: Access.Loc.t BatSet.t 
  -> ?ptrinfo: ItvDom.Table.t -> Global.t -> BasicDom.Node.t -> Dom.t -> Access.t

val do_preanalysis: ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t 
  -> Global.t -> Dom.t * Global.t 

val check_bo: BasicDom.Proc.t -> OctDom.PackConf.t -> BasicDom.Allocsite.t 
  -> Itv.t -> Cil.exp -> ItvDom.Mem.t -> Dom.t -> Itv.t
