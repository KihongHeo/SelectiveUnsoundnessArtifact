(** Interval analysis *)

val marshal_in : Global.t -> Global.t * ItvDom.Table.t * ItvDom.Table.t 
val marshal_out : Global.t * ItvDom.Table.t * ItvDom.Table.t -> Global.t * ItvDom.Table.t * ItvDom.Table.t
val do_sparse_analysis : Global.t -> Global.t * ItvDom.Table.t * ItvDom.Table.t
val inspect_alarm : Global.t * ItvDom.Table.t * ItvDom.Table.t -> Report.query list
