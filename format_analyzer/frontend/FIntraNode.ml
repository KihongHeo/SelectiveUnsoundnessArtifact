open Cil

(* Intra Node *)

type t = ENTRY | EXIT | Node of int

let compare = compare

let equal n1 n2 =
  match n1, n2 with
  | ENTRY, ENTRY -> true
  | EXIT, EXIT -> true
  | Node i1, Node i2 -> i1 = i2
  | _, _ -> false

let hash = Hashtbl.hash

let nid = ref 0

let fromCilStmt : stmt -> t
  =fun s -> 
    if !nid < s.sid then nid := s.sid;
    Node s.sid

let make () = nid := !nid + 1; Node !nid

let getid n = 
  match n with
  | ENTRY
  | EXIT -> -1
  | Node id -> id 

let to_string : t -> string 
  =fun n ->
    match n with
    | ENTRY -> "ENTRY"
    | EXIT -> "EXIT"
    | Node i -> string_of_int i
