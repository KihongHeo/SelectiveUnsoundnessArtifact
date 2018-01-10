open VocabA
open VocabB
open UserInputType
open UserInput.Input
       
module InterNodeG = Key2Ordered (InterNode)
    
module Label = struct
  type t = string
  let compare : t -> t -> int = String.compare
  let default = "C"
end

module CG =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (InterNodeG) (Label)

(* For context encoding *)
module Check = Graph.Path.Check (CG)

(* Data structures for edge-id conversions *)
type edge2id = (CG.E.t, int) Hashtbl.t
type id2edge = (int, CG.E.t) Hashtbl.t 

let empty_idedges () =
  let edge2id = Hashtbl.create 999 in 
  let id2edge = Hashtbl.create 999 in 
  (edge2id, id2edge)

let create_idedges cg start_id =
  let nb_edges = (CG.nb_edges cg) * 2 in 
  let edge2id = Hashtbl.create nb_edges in 
  let id2edge = Hashtbl.create nb_edges in
  let add_idedge e cnt =
    let _ = Hashtbl.add edge2id e cnt in
    let _ = Hashtbl.add id2edge cnt e in 
    cnt + 1
  in
  let _ = CG.fold_edges_e add_idedge cg start_id in 
  (edge2id, id2edge)   

let add_idedges (edge2id, id2edge) cnt e =
  let _ = Hashtbl.add edge2id e cnt in
  let _ = Hashtbl.add id2edge cnt e in
  ()   

let get_edge_id (edge2id, id2edge) edge =
  try
    Hashtbl.find edge2id edge
  with
  | Not_found ->
    failwith "get_edge_id failed"

let get_edge_from_id (edge2id, id2edge) id =
  try
    (Hashtbl.find id2edge (int_of_string id))
  with
  | Not_found ->
    failwith ("get_edge_from_id failed id = " ^ (id))

let print_idedges (edge2id, id2edge) =
  let print_iter id edge =
    let edge_src = (PPIL.string_of_inter_node (CG.E.src edge)) in
    let edge_dst = (PPIL.string_of_inter_node (CG.E.dst edge)) in
    Printf.printf "%d : %s -> %s\n" id edge_src edge_dst
  in
  Hashtbl.iter print_iter id2edge
