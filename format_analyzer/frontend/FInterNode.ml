(* Inter Node *)

type pid = string

type t = pid * FIntraNode.t
let name = "Node"
let to_string (pid,node) = pid ^ "-" ^ FIntraNode.to_string node
let to_json x = `String (to_string x)
let make pid node = (pid,node)
let get_pid (pid,node) = pid
let get_cfgnode (pid,node) = node
let compare = compare
let hash = Hashtbl.hash
let equal (p1, n1) (p2, n2) = p1 = p2 && FIntraNode.equal n1 n2
