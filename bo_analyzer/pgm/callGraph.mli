(** Call-graph *)
type t
val empty : t
val add_edge : InterCfg.pid -> InterCfg.pid -> t -> t
val callees : InterCfg.pid -> t -> InterCfg.pid BatSet.t
val remove_function : InterCfg.pid -> t -> t

val is_rec : t -> InterCfg.pid -> bool

val init_trans_calls : t -> t
val trans_callees : InterCfg.pid -> t -> InterCfg.pid BatSet.t

val to_json : t -> Yojson.Safe.json 
