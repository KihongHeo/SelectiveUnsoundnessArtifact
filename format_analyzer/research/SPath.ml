open CallGraph
    
module Weight = struct
  type edge = CG.E.t
  type t = int
  let weight e = 1
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end
module SPath = Graph.Path.Dijkstra (CG) (Weight)
