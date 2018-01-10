(** Worklist *)
module Make(DUGraph : Dug.S) : 
sig
  type t
  val init : DUGraph.t -> t
  val pick : t -> (BasicDom.Node.t * t) option
  val push : BasicDom.Node.t -> BasicDom.Node.t -> t -> t
  val push_set : BasicDom.Node.t -> BasicDom.Node.t BatSet.t -> t -> t
  val is_loopheader : BasicDom.Node.t -> t -> bool
end
