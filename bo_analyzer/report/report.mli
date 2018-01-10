(** Alarm report of interval analysis *)
type target = BO | ND | DZ 
type status = Proven | UnProven | BotAlarm
type query = {
  node : InterCfg.node;
  exp : AlarmExp.t;
  loc : Cil.location;
  allocsite : BasicDom.Allocsite.t option; 
  status : status;
  desc : string
}
type part_unit = Cil.location 
val sort_partition : (part_unit * query list) list -> (part_unit * query list) list
val string_of_alarminfo : Itv.t -> Itv.t -> string 
val partition : query list -> (part_unit, query list) BatMap.t
val generate : Global.t * ItvDom.Table.t * target -> query list
val check_bo : ItvDom.Val.t -> ItvDom.Val.t option -> (status * BasicDom.Allocsite.t option * string) list
val print : bool -> query list -> unit
