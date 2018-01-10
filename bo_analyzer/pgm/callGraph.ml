open Vocab
open BasicDom
open Yojson.Safe
open InterCfg

module G = 
struct 
  include Graph.Persistent.Digraph.Concrete (BasicDom.Proc)
  let succ g pid = try succ g pid with _ -> []
end
module SCC = Graph.Components.Make (G)
module Oper = Graph.Oper.P(G)

type t = { 
  graph : G.t ;
  trans_calls : G.t 
}

let empty = { 
  graph = G.empty ;
  trans_calls = G.empty 
}
 
let add_edge : BasicDom.Proc.t -> BasicDom.Proc.t -> t -> t
= fun src dst g -> 
  { g with graph = G.add_edge g.graph src dst }

let callees : BasicDom.Proc.t -> t -> PowProc.t
= fun pid g -> G.succ g.graph pid |> PowProc.of_list

let trans_callees : BasicDom.Proc.t -> t -> PowProc.t
= fun pid g -> G.succ g.trans_calls pid |> PowProc.of_list
 
let init_trans_calls : t -> t = fun callgraph ->
  let trans_calls = Oper.transitive_closure callgraph.graph in
  { callgraph with trans_calls = trans_calls }

  let is_rec : t -> InterCfg.pid -> bool = fun callgraph pid ->
    try 
      let trans = G.succ callgraph.trans_calls pid in
      List.mem pid trans
    with _ -> true (* conservative answer for exceptional cases (e.g., unreachable functions) *)

  let to_json : t -> json 
  = fun g ->
    let nodes = `List (G.fold_vertex (fun v nodes -> 
              (`String (Proc.to_string v))::nodes) g.graph [])
    in
    let edges = `List (G.fold_edges (fun v1 v2 edges ->
              (`List [`String (Proc.to_string v1); 
                      `String (Proc.to_string v2)
                     ])::edges) g.graph []) in
    `Assoc [("nodes", nodes); ("edges", edges)]

  let remove_function : InterCfg.pid -> t -> t = fun pid callgraph ->
  { 
     graph = G.remove_vertex callgraph.graph pid
    ; trans_calls = G.remove_vertex callgraph.trans_calls pid}

