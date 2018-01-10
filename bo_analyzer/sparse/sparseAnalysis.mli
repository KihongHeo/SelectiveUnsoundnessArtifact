(** Sparse analysis framework *)
module Make(Pre:Pre.S)
           (MainSem:AbsSem.S with type Dom.A.t = Pre.Access.Loc.t)
           (Table:MapDom.S with type A.t = BasicDom.Node.t and type B.t = MainSem.Dom.t)
           (DUGraph:Dug.S with type vertex = Table.A.t and type loc = Pre.Access.Loc.t) : 
sig
  val perform : ?locset:Pre.Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t 
    -> Global.t * DUGraph.t * Table.t -> Global.t * Table.t * Table.t
end

