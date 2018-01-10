(** Abstract semantics of interval analysis *)
module Dom = ItvDom.Mem
module Access : Access.ACCESS with type Loc.t = Dom.A.t

val rev_binop : Cil.binop -> Cil.binop
val make_cond_simple : Cil.exp -> Cil.exp option
val eval_lv : BasicDom.Proc.t -> Cil.lval -> ItvDom.Mem.t -> BasicDom.PowLoc.t
val eval : BasicDom.Proc.t -> Cil.exp -> ItvDom.Mem.t -> ItvDom.Val.t
val eval_alloc : BasicDom.Node.t -> int -> Cil.exp -> bool -> Dom.t -> ItvDom.Val.t
val eval_salloc : BasicDom.Node.t -> int -> string -> Dom.t -> ItvDom.Val.t
val run_cmd : ?position:int -> AbsSem.update_mode -> BasicDom.Node.t 
  -> IntraCfg.Cmd.t -> Dom.t * Global.t -> Dom.t * Global.t

val run : ?mode:AbsSem.update_mode -> ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t
  -> BasicDom.Node.t -> Dom.t * Global.t -> Dom.t * Global.t
val accessof : ?mode:AbsSem.update_mode -> ?locset:Access.Loc.t BatSet.t 
  -> ?ptrinfo:ItvDom.Table.t -> Global.t -> BasicDom.Node.t -> Dom.t -> Access.t 
val init_access : unit -> unit
val return_access : unit -> Access.t
val do_preanalysis : ?locset:Access.Loc.t BatSet.t -> ?ptrinfo:ItvDom.Table.t -> Global.t -> Dom.t * Global.t 
