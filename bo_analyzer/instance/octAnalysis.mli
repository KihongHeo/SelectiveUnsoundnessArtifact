(** Octagon analysis *)

val marshal_in : Global.t * ItvDom.Table.t * ItvDom.Table.t 
  -> Global.t * OctDom.PackConf.t * ItvDom.Table.t * OctDom.Table.t * OctDom.Table.t 
val marshal_out : Global.t * OctDom.PackConf.t * ItvDom.Table.t * OctDom.Table.t * OctDom.Table.t 
  -> Global.t * OctDom.PackConf.t * ItvDom.Table.t * OctDom.Table.t * OctDom.Table.t


val do_analysis : Global.t * ItvDom.Table.t * ItvDom.Table.t 
  -> Global.t * OctDom.PackConf.t * ItvDom.Table.t * OctDom.Table.t * OctDom.Table.t

val inspect_alarm : Global.t * OctDom.PackConf.t * ItvDom.Table.t * OctDom.Table.t * OctDom.Table.t 
  -> Report.query list
