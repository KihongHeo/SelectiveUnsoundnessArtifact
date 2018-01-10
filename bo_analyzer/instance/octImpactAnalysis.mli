(** Octagon impact analysis *)

val marshal_in : Global.t * ItvDom.Table.t -> Global.t * ItvDom.Table.t * OctImpactDom.Table.t * OctImpactDom.Table.t 
val marshal_out : Global.t * ItvDom.Table.t * OctImpactDom.Table.t * OctImpactDom.Table.t 
  -> Global.t * ItvDom.Table.t * OctImpactDom.Table.t * OctImpactDom.Table.t

val packing : Global.t * ItvDom.Table.t -> OctDom.PackConf.t

val do_analysis : Global.t * ItvDom.Table.t 
  -> Global.t * ItvDom.Table.t * OctImpactDom.Table.t * OctImpactDom.Table.t

