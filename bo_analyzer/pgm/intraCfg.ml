(** CFG for a Function. *)

open Vocab
open Cil
open CilHelper
open Printf 

module Node = struct
  type t = ENTRY | EXIT | Node of int
  let compare = compare
  let equal n1 n2 =
    match n1, n2 with
    | ENTRY, ENTRY -> true
    | EXIT, EXIT -> true
    | Node i1, Node i2 -> i1 = i2
    | _, _ -> false
  let hash = Hashtbl.hash

  let entry = ENTRY
  let exit = EXIT

  let nid = ref 0

  let fromCilStmt : Cil.stmt -> t
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
end

module Cmd = struct
  type t = 
  | Cinstr of instr list 
  | Cif of exp * block * block * location 
  | CLoop of location
  (* final graph has the following cmds only *)
  | Cset of lval * exp * location
  | Cexternal of lval * location 
  | Calloc of lval * exp * static * location
  | Csalloc of lval * string * location
  | Cfalloc of lval * fundec * location
  | Cassume of exp * location
  | Cseq of t list 
  | Ccall of lval option * exp * exp list * location 
  | Creturn of exp option * location
  | Casm of attributes * string list * 
            (string option * string * lval) list *
            (string option * string * exp) list *
            string list * location
  | Cskip
  and static = bool

  let fromCilStmt: Cil.stmtkind -> t 
  =fun s -> 
    match s with
    | Instr instrs -> Cinstr instrs
    | If (exp,b1,b2,loc) -> Cif (exp,b1,b2,loc)
    | Loop (_,loc,_,_) -> CLoop loc
    | Return (expo,loc) -> Creturn (expo,loc)
    | _ -> Cskip
  
  let rec to_string : t -> string
  =fun c ->
    match c with
    | Cinstr instrs -> s_instrs instrs
    | Cset (lv,e,_) -> "set("^s_lv lv^","^s_exp e^")"
    | Cexternal (lv,_) -> "extern("^s_lv lv^")"
    | Calloc (lv,e,_,_) -> "alloc("^s_lv lv^",["^s_exp e^"])"
(*    | Calloc (lv,Struct comp,_) -> "alloc("^s_lv lv^",{"^comp.cname^"})"*)
    | Csalloc (lv, s, _) -> "salloc("^s_lv lv^", \""^s^"\")"
    | Cfalloc (lv, f, _) -> "falloc("^s_lv lv^", "^f.svar.vname^")"
    | Cseq l -> List.fold_left (fun s c -> s^to_string c^";\n") "" l
    | Ccall (Some lv,fexp,params,_) -> 
       s_lv lv ^ ":= call(" ^ s_exp fexp ^ ", "^s_exps params ^ ")"
    | Ccall (None,fexp,params,_) -> 
      "call(" ^ s_exp fexp ^ s_exps params ^ ")"
    | Creturn (Some e,_) -> "return " ^ s_exp e
    | Creturn (None,_) -> "return" 
    | Cif (e,b1,b2,loc) -> "if"
    | Cassume (e,loc) -> "assume(" ^ s_exp e ^ ")"
    | CLoop _ -> "loop"
    | Casm _ -> "asm"
    | Cskip -> "skip"

  let location_of : t -> Cil.location
  =fun c ->
    match c with
    | Cset (_,_,l)
    | Cexternal (_,l)
    | Calloc (_,_,_,l)
    | Csalloc (_, _, l)
    | Cfalloc (_, _, l)
    | Ccall (_,_,_,l) -> l
    | Creturn (_,l) -> l
    | Cassume (_,l) -> l
    | Cseq _
    | Cif (_,_,_,_) | CLoop _ | Casm _ | Cskip
    | Cinstr _ -> Cil.locUnknown

end

type node = Node.t
type cmd  = Cmd.t

module G = Graph.Persistent.Digraph.ConcreteBidirectional(Node)
module Topo = Graph.Topological.Make(G)
module Scc = Graph.Components.Make(G)

module GDom = struct
  module V = Node
  type t = G.t
  let empty = G.empty
  let fromG g = g
  let pred = G.pred
  let succ = G.succ
  let fold_vertex = G.fold_vertex
  let iter_vertex = G.iter_vertex
  let iter_succ = G.iter_succ
  let nb_vertex = G.nb_vertex
  let add_edge g a b = ()
  let create : ?size:int -> unit -> t = fun ?size:int () -> empty
end

module Dom = Graph.Dominator.Make_graph (GDom)

type t = {
  fd                : Cil.fundec;
  graph             : G.t;
  cmd_map           : (node, cmd) BatMap.t;
  dom_fronts        : dom_fronts;
  dom_tree          : dom_tree;
  scc_list          : node list list
}
and dom_fronts = (Node.t, Node.t BatSet.t) BatMap.t
and dom_tree = G.t


let empty : Cil.fundec -> t
=fun fd -> {
  fd                = fd;
  graph             = G.empty;
  cmd_map           = BatMap.empty;
  dom_fronts        = BatMap.empty;
  dom_tree          = G.empty;
  scc_list          = []
}

let get_pid : t -> string
=fun g -> g.fd.svar.vname

let get_formals : t -> string list
=fun g -> List.map (fun svar -> svar.vname) g.fd.sformals

let get_formals_lval : t -> Cil.lval list
= fun g -> List.map Cil.var g.fd.sformals

let get_dom_tree g = g.dom_tree

let children_of_dom_tree node dom_tree = BatSet.remove node (BatSet.of_list (G.succ dom_tree node))

let parent_of_dom_tree node dom_tree = 
  match G.pred dom_tree node with
  | [] -> None
  | [p] -> Some p
  | _ -> raise (Failure "IntraCfg.parent_of_dom_tree: fatal")

let get_dom_fronts g = g.dom_fronts

let nodesof : t -> node list
=fun g -> BatSet.elements (G.fold_vertex BatSet.add g.graph BatSet.empty)

let add_edge : node -> node -> t -> t
=fun n1 n2 g -> {g with graph = G.add_edge g.graph n1 n2 }

let add_node : node -> t -> t
=fun n g -> {g with graph = G.add_vertex g.graph n}

let find_cmd : node -> t ->  cmd
=fun n g -> 
  try if n = Node.ENTRY || n = Node.EXIT then Cmd.Cskip
      else BatMap.find n g.cmd_map 
  with _ -> 
     raise (Failure ("Can't find cmd of " ^ Node.to_string n))

let add_cmd : node -> cmd -> t -> t
=fun n c g -> {g with cmd_map = BatMap.add n c g.cmd_map }

let add_node_with_cmd : node -> cmd -> t -> t
=fun n c g -> g |> add_node n |> add_cmd n c

let remove_edge : node -> node -> t -> t
=fun n1 n2 g -> {g with graph = G.remove_edge g.graph n1 n2 }

let remove_node : node -> t -> t
=fun n g -> 
  {g with graph = G.remove_vertex g.graph n ;
          cmd_map = BatMap.remove n g.cmd_map ;
          dom_fronts = BatMap.remove n g.dom_fronts;
          dom_tree = G.remove_vertex g.dom_tree n }

(* should be used only after all Cil nodes are made *)
let add_new_node : node -> cmd -> node -> t -> t
=fun n cmd s g ->
  let new_node = Node.make() in
    (add_cmd new_node cmd 
     >>> remove_edge n s
     >>> add_edge n new_node
     >>> add_edge new_node s) g

let pred : node -> t -> node list
=fun n g -> G.pred g.graph n

let succ : node -> t -> node list
=fun n g -> G.succ g.graph n

let fold_vertex f g a = G.fold_vertex f g.graph a
let fold_edges f g a = G.fold_edges f g.graph a

let is_entry : node -> bool
=fun node ->
  match node with
  | Node.ENTRY -> true
  | _ -> false

let is_exit : node -> bool
=fun node ->
  match node with
  | Node.EXIT -> true
  | _ -> false

let is_callnode : node -> t -> bool
=fun n g ->
  match find_cmd n g with
  | Cmd.Ccall _ -> true
  | _ -> false

let is_returnnode : node -> t -> bool
=fun n g ->
  List.length (pred n g) = 1 &&
  is_callnode (List.hd (pred n g)) g

let entryof _ = Node.ENTRY
let exitof _ = Node.EXIT

let returnof : node -> t -> node 
=fun n g ->
  if is_callnode n g then (
    assert (List.length (succ n g) = 1);
    List.hd (succ n g))
  else failwith "IntraCfg.returnof: given node is not a call-node"

let is_inside_loop : node -> t -> bool
=fun n g -> List.exists (fun scc -> List.length scc > 1 && List.mem n scc) g.scc_list
      
let callof : node -> t -> node
=fun r g ->
  try
    List.find (fun c -> is_callnode c g && returnof c g = r) (nodesof g)
  with _ -> failwith "IntraCfg.callof: given node may not be a return-node"

let generate_assumes : t -> t
=fun g -> 
  try 
    fold_vertex (fun n g ->
      match find_cmd n g with
      | Cmd.Cif (e,tb,fb,loc) ->
        let succs = succ n g in (* successors of if-node *)
        let _ = assert (List.length succs = 1 || List.length succs = 2) in
          if List.length succs = 2 then (* normal case *)
            let s1,s2 = List.nth succs 0, List.nth succs 1 in
            let tbn,fbn = (* true-branch node, false-branch node *)
              match tb.bstmts, fb.bstmts with
              | [],[] -> s1,s2
              | t::l,_ -> if t.sid = Node.getid s1 then s1,s2 else s2,s1
              | _,t::l -> if t.sid = Node.getid s2 then s1,s2 else s2,s1 in
            let tassert = Cmd.Cassume (e,loc) in
            let fassert = Cmd.Cassume (UnOp (LNot,e,Cil.typeOf e),loc) in
              (add_new_node n fassert fbn
              >>> add_new_node n tassert tbn) g
          else (* XXX : when if-statement has only one successor. 
                        seems to happen inside dead code *)
            let tbn = List.nth succs 0 in
            let tassert = Cmd.Cassume (e,loc) in
              add_new_node n tassert tbn g
      | _ -> g
    ) g g
  with _ -> assert (false)

(* If and Loop are unnecessary in cfg *)
let remove_if_loop : t -> t
=fun g ->
  fold_vertex (fun n g ->
    match find_cmd n g with
    | Cmd.Cif _ 
    | Cmd.CLoop _ -> add_cmd n Cmd.Cskip g
    | _ -> g
  ) g g

(* remove all nodes s.t. n1 -> empty_node -> n2 *)
let remove_empty_nodes : t -> t
=fun g -> 
  fold_vertex (fun n g ->
    if find_cmd n g = Cmd.Cskip &&
       List.length (succ n g) = 1 && 
       List.length (pred n g) = 1 
    then 
      let p = List.nth (pred n g) 0 in
      let s = List.nth (succ n g) 0 in
        g |> remove_node n |> add_edge p s
    else g
  ) g g

(* split instructions into set/call/asm *)
let flatten_instructions : t -> t
=fun g ->
  fold_vertex (fun n g ->
    match find_cmd n g with
    | Cmd.Cinstr instrs when instrs <> [] ->
      let cmds = 
        List.map (fun i ->
          match i with
          | Set (lv,e,loc) -> Cmd.Cset (lv,e,loc)
          | Call (lvo,f,args,loc) -> Cmd.Ccall (lvo,f,args,loc)
          | Asm (a,b,c,d,e,f) -> Cmd.Casm (a,b,c,d,e,f)
        ) instrs in
      let pairs = List.map (fun c -> (Node.make(),c)) cmds in
      let first,_ = List.nth pairs 0 in
      let last,_ = List.nth pairs (List.length pairs - 1) in
      let preds,succs = pred n g, succ n g in
        g 
          |> (fun g -> (* add nodes in instrs *)
                List.fold_left (fun g (n,c) ->
                  add_node_with_cmd n c g) g pairs) 
          |> (fun g -> (* connect edges between instrs *)
                fst (List.fold_left (fun (g,p) (n,c) ->
                            (add_edge p n g, n)) (g,n) pairs))
          |> list_fold (fun p -> add_edge p first) preds
          |> list_fold (fun s -> add_edge last s) succs
          |> remove_node n 

    | Cmd.Cinstr [] -> add_cmd n Cmd.Cskip g
    | _ -> g  
  ) g g

let make_array : Cil.fundec -> Cil.lval -> Cil.typ -> Cil.exp -> Cil.location -> node -> t -> (node * t)
= fun fd lv typ exp loc entry g ->
  let alloc_node = Node.make () in
  let size = Cil.BinOp (Cil.Mult, Cil.SizeOf typ, exp, Cil.intType) in
  let alloc_cmd = Cmd.Calloc (lv, size, true, loc) in
  (alloc_node, g |> add_cmd alloc_node alloc_cmd |> add_edge entry alloc_node)

let rec make_nested_array : Cil.fundec -> Cil.lval -> Cil.typ -> Cil.exp -> Cil.location -> node -> t -> (node * t)
= fun fd lv typ exp loc entry g ->
  let typ = unrollTypeDeep typ in
  match typ with
    TArray (t, Some size, _) ->
      (* i = 0 *)
      let init_node = Node.make () in
      let idxinfo = Cil.makeTempVar fd (Cil.TInt (IInt, [])) in
      let idx = (Cil.Var idxinfo, Cil.NoOffset) in 
      let init_value = Cil.Const (Cil.CInt64 (Int64.zero, IInt, None)) in
      let init_cmd = Cmd.Cset (idx, init_value, loc) in
      let g = add_cmd init_node init_cmd g in
      (* while (i < exp) *)
      let skip_node = Node.make () in
      let g = add_cmd skip_node Cmd.Cskip g in
      let g = add_edge init_node skip_node g in 
      let g = add_edge entry init_node g in
      let assume_node = Node.make () in
      let cond = Cil.BinOp (Cil.Lt, Cil.Lval idx, exp, Cil.intType) in
      let assume_cmd = Cmd.Cassume (cond, loc) in
      let g = add_cmd assume_node assume_cmd g in
      let g = add_edge skip_node assume_node g in
      let nassume_node = Node.make () in
      let nassume_cmd = Cmd.Cassume (Cil.UnOp (Cil.LNot, cond, typ), loc) in
      let g = add_cmd nassume_node nassume_cmd g in
      let g = add_edge skip_node nassume_node g in
      let element = Cil.addOffsetLval (Index (Lval (Var idxinfo, NoOffset), NoOffset)) lv in
      (* tmp = malloc(size); lv[i] = tmp *)
      let tmp = (Cil.Var (Cil.makeTempVar fd (Cil.TPtr (Cil.TVoid [], []))), Cil.NoOffset) in
      let (term, g) = make_array fd tmp t size loc assume_node g in
      let cast_node = Node.make () in  
      let cast_cmd = Cmd.Cset (element, Cil.CastE (TPtr (t, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      
      let (term, g) = make_nested_array fd element t size loc cast_node g in
      (* i++ *)
      let incr_node = Node.make () in
      let incr_cmd = Cmd.Cset (idx, Cil.BinOp (Cil.PlusA, Cil.Lval idx, Cil.Const (Cil.CInt64 (Int64.one, IInt, None)), typ), loc) in
      let g = add_cmd incr_node incr_cmd g in
      let g = add_edge term incr_node g in
      let g = add_edge incr_node skip_node g in
        (nassume_node, g) 
  | TComp(comp, _) -> 
      (* i = 0 *)
      let init_node = Node.make () in
      let idxinfo = Cil.makeTempVar fd (Cil.TInt (IInt, [])) in
      let idx = (Cil.Var idxinfo, Cil.NoOffset) in 
      let init_value = Cil.Const (Cil.CInt64 (Int64.zero, IInt, None)) in
      let init_cmd = Cmd.Cset (idx, init_value, loc) in
      let g = add_cmd init_node init_cmd g in
      (* while (i < exp) *)
      let skip_node = Node.make () in
      let g = add_cmd skip_node Cmd.Cskip g in
      let g = add_edge init_node skip_node g in 
      let g = add_edge entry init_node g in
      let assume_node = Node.make () in
      let cond = Cil.BinOp (Cil.Lt, Cil.Lval idx, exp, Cil.intType) in
      let assume_cmd = Cmd.Cassume (cond, loc) in
      let g = add_cmd assume_node assume_cmd g in
      let g = add_edge skip_node assume_node g in
      let nassume_node = Node.make () in
      let nassume_cmd = Cmd.Cassume (Cil.UnOp (Cil.LNot, cond, typ), loc) in
      let g = add_cmd nassume_node nassume_cmd g in
      let g = add_edge skip_node nassume_node g in
      let element = Cil.addOffsetLval (Index (Lval (Var idxinfo, NoOffset), NoOffset)) lv in
      (* tmp = malloc(size); lv[i] = tmp *)
      let tmp = (Cil.Var (Cil.makeTempVar fd (Cil.TPtr (Cil.TVoid [], []))), Cil.NoOffset) in
      let (term, g) = make_array fd tmp typ Cil.one loc assume_node g in
      let cast_node = Node.make () in  
      let cast_cmd = Cmd.Cset (element, Cil.CastE (TPtr (typ, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      let (term, g) = generate_allocs_field comp.cfields element fd cast_node g in
      (* i++ *)
      let incr_node = Node.make () in
      let incr_cmd = Cmd.Cset (idx, Cil.BinOp (Cil.PlusA, Cil.Lval idx, Cil.Const (Cil.CInt64 (Int64.one, IInt, None)), typ), loc) in
      let g = add_cmd incr_node incr_cmd g in
      let g = add_edge term incr_node g in
      let g = add_edge incr_node skip_node g in
        (nassume_node, g) 

  | _ -> (entry, g)

and generate_allocs_field : Cil.fieldinfo list -> Cil.lval -> Cil.fundec -> node -> t ->  (node * t) 
=fun fl lv fd entry g ->
  assert (not (isPointerType (typeOfLval lv) || isArrayType (typeOfLval lv)));
  match fl with 
    [] -> (entry, g)
  | fieldinfo::t -> 
      begin
      match fieldinfo.ftype with 
      | TArray (typ, Some exp, _) -> 
          let field = addOffsetLval (Cil.Field (fieldinfo, Cil.NoOffset)) lv in 
          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp typ exp fieldinfo.floc entry g in
          let cast_node = Node.make () in  
          let cast_cmd = Cmd.Cset (field, Cil.CastE (Cil.TPtr (typ, []), Cil.Lval tmp), fieldinfo.floc) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = make_nested_array fd field typ exp fieldinfo.floc cast_node g in
            generate_allocs_field t lv fd term g
      | TComp (comp, _) ->
          let field = addOffsetLval (Cil.Field (fieldinfo, Cil.NoOffset)) lv in 
          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp fieldinfo.ftype Cil.one fieldinfo.floc entry g in
          let cast_node = Node.make () in  
          let cast_cmd = Cmd.Cset (field, Cil.CastE (Cil.TPtr (fieldinfo.ftype, []), Cil.Lval tmp), fieldinfo.floc) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = generate_allocs_field comp.cfields field fd cast_node g in
          
(*          let (term, g) = generate_allocs_field comp.cfields field fd entry g in*)
          generate_allocs_field t lv fd term g
      | TNamed (typeinfo, _) -> 
          let fieldinfo = {fieldinfo with ftype = typeinfo.ttype} in
          generate_allocs_field (fieldinfo::t) lv fd entry g
      | _ -> generate_allocs_field t lv fd entry g
      end
and get_base_type typ = 
  match typ with
    TArray (t, _, _)
  | TPtr (t, _) -> get_base_type t
  | _ -> typ

let rec generate_allocs : Cil.fundec -> Cil.varinfo list -> node -> t -> (node * t) 
=fun fd vl entry g ->
  match vl with 
    [] -> (entry, g)
  | varinfo::t ->
      begin
      match varinfo.vtype with 
     | TArray (typ, Some exp, _) ->
          let lv = (Cil.Var varinfo, Cil.NoOffset) in 
          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp typ exp varinfo.vdecl entry g in
          let cast_node = Node.make () in  
          let new_typ = match unrollTypeDeep typ with TComp (_,_) -> TPtr(typ,[]) | _ -> typ in
          let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (new_typ, []), Cil.Lval tmp), varinfo.vdecl) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = make_nested_array fd lv typ exp varinfo.vdecl cast_node g in
            generate_allocs fd t term g
      | TComp (comp, _) ->
          let lv = (Cil.Var varinfo, Cil.NoOffset) in 
          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp varinfo.vtype Cil.one varinfo.vdecl entry g in
          let cast_node = Node.make () in  
          let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (varinfo.vtype, []), Cil.Lval tmp), varinfo.vdecl) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = generate_allocs_field comp.cfields lv fd cast_node g in
          generate_allocs fd t term g
      | TNamed (typeinfo, _) -> 
          let varinfo = {varinfo with vtype = typeinfo.ttype} in
          generate_allocs fd (varinfo::t) entry g
      | _ -> generate_allocs fd t entry g
      end

let replace_node_graph : node -> node -> node -> t -> t
= fun old entry exit g ->
  let preds = pred old g in
  let succs = succ old g in 
  let g = remove_node old g in
  let g = List.fold_left (fun g p -> add_edge p entry g) g preds in
  let g = List.fold_left (fun g s -> add_edge exit s g) g succs in
  g

(* transform field acc.  dot to arrow *)
let rec trans_lv lv original_typ offset =
  let rec is_same_type x y = 
    match unrollTypeDeep x, unrollTypeDeep y with 
      TComp (c1,_), TComp (c2,_) -> c1.cname = c2.cname
    | TArray (t1, _, _), TArray(t2,_,_) -> is_same_type t1 t2
    | _, _ -> false
  in
    let typ = unrollTypeDeep (typeOfLval lv) in
    let original_typ = unrollTypeDeep original_typ in
    match (lv, offset, original_typ) with 
      ((Var v, NoOffset), Field (f, off), TComp(_,_)) -> 
        let v = { v with vtype = TPtr (v.vtype, []) } in
        let typ = f.ftype in
        let f = match unrollType f.ftype with TComp(_,_) -> { f with ftype = TPtr (f.ftype, []) } | _ -> f in
        let lv = addOffsetLval (Field (f, NoOffset)) (Mem (Lval (Var v, snd lv)),NoOffset) in
        trans_lv lv typ off
    | ((Mem e, NoOffset), Field (f, off), TComp(_,_)) when not (is_same_type typ original_typ) ->
        let lv = (Mem (trans_exp e), NoOffset) in
        let typ = f.ftype in
        let f = match unrollType f.ftype with TComp(_,_) -> { f with ftype = TPtr (f.ftype, []) } | _ -> f in
        let lv =  addOffsetLval (Field (f, NoOffset)) (Mem (Lval lv),NoOffset) in
        trans_lv lv typ off
    | (_, Field (f, off), TComp(_,_)) when not (is_same_type typ original_typ) ->
        let typ = f.ftype in
        let f = match unrollType f.ftype with TComp(_,_) -> { f with ftype = TPtr (f.ftype, []) } | _ -> f in
        let lv =  addOffsetLval (Field (f, NoOffset)) (Mem (Lval lv),NoOffset) in
        trans_lv lv typ off
    | (_, Field (f,off),_) -> 
      trans_lv (addOffsetLval (Field (f, NoOffset)) lv) f.ftype off
    | ((Var v, o), Index (i,off), TArray ((TComp(_,_) as t),_,_)) ->
      let v = match unrollTypeDeep v.vtype with 
                TArray(TComp(_,_) as tc, x, y) ->
                  { v with vtype = TArray(TPtr (tc, []),x,y) }
              | _ -> raise (Failure "trans_lv")
      in
      let lv =  addOffsetLval (Index (i, NoOffset)) (Var v, o) in
      trans_lv lv t off
    | (_, Index (i,off), TArray (t,_,_)) -> 
      trans_lv (addOffsetLval (Index (i, NoOffset)) lv) t off
    | ((Var v, NoOffset), NoOffset, TArray(TComp (_, _) as tc,x,y)) -> 
        let v = { v with vtype = TPtr (TPtr (tc, []),[]) } in
        (Var v, NoOffset)
    | ((Var v, NoOffset), NoOffset, TComp(_,_)) -> 
        let v = { v with vtype = TPtr (v.vtype, []) } in
        (Var v, NoOffset)
    | (_, NoOffset,_) -> 
      lv
    | _ -> raise (Failure "trans_field")
  and trans_exp e = 
    match e with 
      Lval (l,o) -> Lval (trans_lv (l,NoOffset) (typeOfLval (l, NoOffset)) o)
    | AlignOfE e -> AlignOfE (trans_exp e)
    | UnOp (u,e,t) -> UnOp(u, trans_exp e, t)
    | BinOp (b,e1,e2,t) -> BinOp (b, trans_exp e1, trans_exp e2, t)
    | Question (e1,e2,e3,t) -> Question (trans_exp e1, trans_exp e2, trans_exp e3, t)
    | CastE (t, e) -> CastE (t, trans_exp e)
    | AddrOf (l,o) -> 
      let lv = (l, NoOffset) in
      (match Cil.unrollTypeDeep (Cil.typeOfLval (l,o)) with 
         TComp (_, _) -> Lval (trans_lv lv (typeOfLval lv) o)
       | _ -> AddrOf (trans_lv lv (typeOfLval lv) o))
    | StartOf (l,o) -> 
      let lv = (l,NoOffset) in
      (match Cil.unrollTypeDeep (Cil.typeOfLval (l,o)) with
         TArray (TComp (_,_),_,_) -> Lval (Mem (Lval (trans_lv lv (typeOfLval lv) o)), NoOffset)
       | _ -> StartOf (trans_lv lv (typeOfLval lv) o))
    | _ -> e

let transform_field_access fd g =
  fold_vertex (fun n g ->
      flush stderr;
      match find_cmd n g with 
        Cmd.Cset (lv, e, loc) ->
            let l = (fst lv, NoOffset) in
            add_cmd n (Cmd.Cset (trans_lv l (typeOfLval l) (snd lv), trans_exp e, loc)) g
      | Cmd.Cassume (e, loc) -> 
            add_cmd n (Cmd.Cassume (trans_exp e, loc)) g
      | Cmd.Ccall (Some lv, f, el, loc) -> 
            let l = (fst lv, NoOffset) in
            add_cmd n (Cmd.Ccall (Some (trans_lv l (typeOfLval l) (snd lv)), trans_exp f, List.map trans_exp el, loc)) g
      | Cmd.Ccall (None, f, el, loc) -> 
            add_cmd n (Cmd.Ccall (None, trans_exp f, List.map trans_exp el, loc)) g
      | Cmd.Creturn (Some e, loc) ->
            add_cmd n (Cmd.Creturn (Some (trans_exp e), loc)) g
      | _ -> g) g g

let transform_str_allocs : Cil.fundec -> t -> t
= fun fd g ->
  let rec replace_str : Cil.exp -> Cil.exp * (Cil.lval * string) list
  = fun e -> 
    match e with
      Const (CStr s) -> 
        let tempinfo = Cil.makeTempVar fd (Cil.TPtr (Cil.TInt (IChar, []), [])) in
        let temp = (Cil.Var tempinfo, Cil.NoOffset) in 
          (Lval temp, [(temp, s)])
    | Lval (Mem e, off) -> 
        let (exp', l) = replace_str e in
        (match l with [] -> (e, l) | _ -> (Lval (Mem exp', off), l))
    | SizeOfStr s -> 
        let tempinfo = Cil.makeTempVar fd (Cil.TPtr (Cil.TInt (IChar, []), [])) in
        let temp = (Cil.Var tempinfo, Cil.NoOffset) in 
          (Lval temp, [(temp, s)])
    | SizeOfE exp -> 
        let (exp', l) = replace_str exp in
        (match l with [] -> (e, l) | _ -> (SizeOfE exp', l))
    | AlignOfE exp ->
        let (exp', l) = replace_str exp in
        (match l with [] -> (e, l) | _ -> (AlignOfE exp', l))
    | UnOp (u, exp, t) -> 
        let (exp', l) = replace_str exp in
        (match l with [] -> (e, l) | _ -> (UnOp (u, exp', t), l))
    | BinOp (b, e1, e2, t) ->
        let (e1', l1) = replace_str e1 in
        let (e2', l2) = replace_str e2 in
        (match l1@l2 with [] -> (e, []) | _ -> (BinOp (b, e1', e2', t), l1@l2))
    | CastE (t, exp) -> 
        let (exp', l) = replace_str exp in
        (match l with [] -> (e, l) | _ -> (CastE (t, exp'), l))
    | _ -> (e, [])
  in
  let generate_sallocs : (Cil.lval * string) list -> Cil.location -> node -> t -> (node * t)
  = fun l loc node g ->
    List.fold_left (fun (node, g) (lv, s) ->
                    let new_node = Node.make () in
                    let g = add_edge node new_node g in
                    let cmd = Cmd.Csalloc (lv, s, loc) in
                    let g = add_cmd new_node cmd g in
                    (new_node, g)) (node, g) l
  in
  fold_vertex (fun n g -> 
      match find_cmd n g with 
        Cmd.Cset (lv, e, loc) ->
          (match replace_str e with
            (_, []) -> g
          | (e, l) -> 
            let (empty_node, last_node) = (Node.make (), Node.make ()) in
            let g = add_cmd empty_node Cmd.Cskip g in
            let (node, g) = generate_sallocs l loc empty_node g in
            let cmd = Cmd.Cset (lv, e, loc) in
            let g = add_cmd last_node cmd g in
            let g = add_edge node last_node g in
              replace_node_graph n empty_node last_node g)
      | Cmd.Cassume (e, loc) -> 
          (match replace_str e with 
            (_, []) -> g
          | (e, l) -> 
            let (empty_node, last_node) = (Node.make (), Node.make ()) in
            let g = add_cmd empty_node Cmd.Cskip g in
            let (node, g) = generate_sallocs l loc empty_node g in
            let cmd = Cmd.Cassume (e, loc) in
            let g = add_cmd last_node cmd g in
            let g = add_edge node last_node g in
              replace_node_graph n empty_node last_node g)
      | Cmd.Ccall (lv, f, el, loc) -> 
          let (el, l) = List.fold_left (fun (el, l) param -> 
              let (e', l') = replace_str param in
              (el@[e'], l@l')) ([], []) el in
          (match (el, l) with 
            (_, []) -> g
          | (el, l) -> 
            let (empty_node, last_node) = (Node.make (), Node.make ()) in
            let g = add_cmd empty_node Cmd.Cskip g in
            let (node, g) = generate_sallocs l loc empty_node g in
            let cmd = Cmd.Ccall (lv, f, el, loc) in
            let g = add_cmd last_node cmd g in
            let g = add_edge node last_node g in
              replace_node_graph n empty_node last_node g)
      | Cmd.Creturn (Some e, loc) ->
           (match replace_str e with 
            (_, []) -> g
          | (e, l) -> 
            let (empty_node, last_node) = (Node.make (), Node.make ()) in
            let g = add_cmd empty_node Cmd.Cskip g in
            let (node, g) = generate_sallocs l loc empty_node g in
            let cmd = Cmd.Creturn (Some e, loc) in
            let g = add_cmd last_node cmd g in
            let g = add_edge node last_node g in
              replace_node_graph n empty_node last_node g)
      | _ -> g) g g

(** transform malloc to Calloc *)
let transform_allocs : Cil.fundec -> t -> t
= fun fd g ->
  let rec transform lv exp loc node g =
    match exp with 
      BinOp (Mult, SizeOf typ, e, _)
    | BinOp (Mult, e, SizeOf typ, _) ->
      begin
        let typ = Cil.unrollTypeDeep typ in
        match lv, typ with 
          (Var v, NoOffset), TComp (_, _) -> (* TODO *)
            let g = add_cmd node Cmd.Cskip g in
              let lv = (Var { v with vtype = Cil.TArray (typ,None,[]) }, NoOffset) in
              make_nested_array fd lv typ e loc node g
        | _ ->
            let cmd = Cmd.Calloc (lv, exp, false, loc) in
            let g = add_cmd node cmd g in
            (node, g)
      end
    | SizeOf typ | CastE (_, SizeOf typ) ->
      begin
        let typ = Cil.unrollTypeDeep typ in
        match lv, typ with 
          (Var v, NoOffset), TComp (comp, _) ->   (* dynamic struct allc *) 
            let cmd = Cmd.Calloc (lv, exp, false, loc) in
            let g = add_cmd node cmd g in 
            let lv = (Var { v with vtype = Cil.TPtr (typ,[]) }, NoOffset) in
            generate_allocs_field comp.cfields (Mem (Lval lv), NoOffset) fd node g
        | _, _ -> 
          let cmd = Cmd.Calloc (lv, exp, false, loc) in
          let g = add_cmd node cmd g in
            (node, g)
      end
    | SizeOfE e -> transform lv (SizeOf (Cil.typeOf e)) loc node g
    | e ->  
      let cmd = Cmd.Calloc (lv, exp, false, loc) in
      let g = add_cmd node cmd g in
      (node, g)
  in
  fold_vertex (fun n g ->
      match find_cmd n g with 
        Cmd.Ccall (Some (Var v, NoOffset), Lval (Var varinfo, _), args, loc) ->
          if varinfo.vname = "malloc" || varinfo.vname = "__builtin_alloca"  then
            let lv = (Var { v with vtype = voidPtrType }, NoOffset) in
            let new_node = Node.make () in
            let preds = pred n g in 
            let succs = succ n g in
            let g = List.fold_left (fun g s -> remove_edge n s g) g succs in 
            let g = List.fold_left (fun g p -> remove_edge p n g) g preds in 
            let g = remove_node n g in
            let g = List.fold_left (fun g p -> add_edge p new_node g) g preds in 
            let (term, g) = transform lv (List.hd args) loc new_node g in
              List.fold_left (fun g s -> add_edge term s g) g succs
          else g
      | _ -> g) g g

(** for each call-node, insert a corresponding return-node *)
let insert_return_nodes : t -> t
=fun g -> 
  List.fold_left (fun g c ->
    if is_callnode c g then
      let r = returnof c g in
      add_new_node c Cmd.Cskip r g
    else g
  ) g (nodesof g) 

(** before each exit-node, insert a return cmd if there is not *)
let insert_return_before_exit : t -> t
=fun g ->
  let add_return node acc =
    match find_cmd node g with
    | Cmd.Creturn _ -> acc
    | _ -> add_new_node node (Cmd.Creturn (None, locUnknown)) Node.EXIT acc
  in
  list_fold add_return (pred Node.EXIT g) g

let compute_dom : t -> t
=fun g -> 
  let dom_functions = Dom.compute_all (GDom.fromG g.graph) Node.ENTRY in
  let dom_tree = 
    List.fold_left (fun dom_tree node ->
      List.fold_left (fun dom_tree child ->
        G.add_edge dom_tree node child
      ) dom_tree (dom_functions.Dom.dom_tree node)
    ) G.empty (nodesof g) in
  let dom_fronts = 
    List.fold_left (fun dom_fronts node ->
      BatMap.add node (BatSet.of_list (dom_functions.Dom.dom_frontier node)) dom_fronts
    ) BatMap.empty (nodesof g) in
    {g with dom_tree = dom_tree;
            dom_fronts = dom_fronts}

let compute_scc : t -> t
=fun g -> { g with scc_list = Scc.scc_list g.graph }
 
let rec process_gvardecl : Cil.fundec -> Cil.lval -> Cil.location -> node -> t -> (node * t)
= fun fd lv loc entry g ->
  match Cil.typeOfLval lv with 
  | TArray (typ, Some exp, _) ->
      let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
      let (term, g) = make_array fd tmp typ exp loc entry g in
      let cast_node = Node.make () in  
      let new_typ = match unrollTypeDeep typ with TComp (_,_) -> TPtr(typ,[]) | _ -> typ in
      let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (new_typ, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      let (term, g) = make_nested_array fd lv typ exp loc cast_node g in
      (term, g)
  | TInt (_, _) | TFloat (_, _) ->
      let node = Node.make () in
      let cmd = Cmd.Cset (lv, Cil.zero, loc) in
      (node, g |> add_cmd node cmd |> add_edge entry node)
  | TComp (comp, _) ->
      let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
      let (term, g) = make_array fd tmp (Cil.typeOfLval lv) Cil.one loc entry g in
      let cast_node = Node.make () in  
      let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (Cil.typeOfLval lv, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      let (term, g) = generate_allocs_field comp.cfields lv fd cast_node g in
      (term, g)
  | TNamed (typeinfo,_) ->
      let varinfo = match lv with (Var var, _) -> var | _ -> raise (Failure "process_gvardecl") in
      let varinfo = { varinfo with vtype = typeinfo.ttype} in
      let lval = (Cil.Var varinfo, Cil.NoOffset) in
      process_gvardecl fd lval loc entry g
  | _ -> (entry, g)

let rec process_init : Cil.fundec -> Cil.lval -> Cil.init -> Cil.location -> node -> t -> (node * t)
= fun fd lv i loc entry g ->
  match i with 
    SingleInit exp ->
      let new_node = Node.make () in
      let cmd = Cmd.Cset (lv, exp, loc) in
      let g = add_edge entry new_node (add_cmd new_node cmd g) in
      (new_node, g)
  | CompoundInit (typ, ilist) -> 
      List.fold_left (fun (node, g) (offset, init) ->
          let lv = trans_lv lv typ offset in
(*          let lv = Cil.addOffsetLval offset lv in*)
          process_init fd lv init loc node g) (entry, g) ilist

let rec process_gvar : Cil.fundec -> Cil.lval -> Cil.initinfo -> Cil.location -> node -> t -> (node * t)
= fun fd lv i loc entry g ->
  match (Cil.typeOfLval lv, i.init) with 
    (_, None) -> process_gvardecl fd lv loc entry g
  | (_, Some (SingleInit exp as init)) -> process_init fd lv init loc entry g
  | (_, Some (CompoundInit (typ, ilist) as init)) -> 
      let (node, g) = process_gvardecl fd lv loc entry g in
      process_init fd lv init loc node g

let get_main_dec : Cil.global list -> (Cil.fundec * Cil.location) option
= fun globals ->
  List.fold_left (fun s g ->
                  match g with 
                    Cil.GFun (fundec, loc) 
                    when fundec.svar.vname = "main" -> Some (fundec, loc)
                  | _ -> s) None globals

let process_fundecl : Cil.fundec -> Cil.fundec -> Cil.location -> node -> t -> (node * t)
= fun fd fundecl loc node g ->
  let new_node = Node.make () in
  let cmd = Cmd.Cfalloc ((Var fundecl.svar, NoOffset), fundecl, loc) in
  let g = add_edge node new_node (add_cmd new_node cmd g) in
  (new_node, g)

let fromFunDec : Cil.fundec -> Cil.location -> t
=fun fd loc -> 
  let entry = Node.fromCilStmt (List.nth fd.sallstmts 0) in
  let g = 
    (* add nodes *)
    (list_fold (fun s -> 
        add_node_with_cmd (Node.fromCilStmt s) (Cmd.fromCilStmt s.skind)
      ) fd.sallstmts 
    >>>
    (* add edges *)
    list_fold (fun stmt ->
        list_fold (fun succ -> 
          add_edge (Node.fromCilStmt stmt) (Node.fromCilStmt succ)
        ) stmt.succs
      ) fd.sallstmts
    ) (empty fd) in

  let (term, g) = 
    if fd.svar.vname = "main" && List.length fd.sformals >= 2 then
      let (argc, argv) = ((Cil.Var (List.nth fd.sformals 0), Cil.NoOffset), (Cil.Var (List.nth fd.sformals 1), Cil.NoOffset)) in
      let arg_node = Node.make () in
      let arg_cmd = Cmd.Ccall (None, Cil.Lval (Cil.Var (Cil.makeGlobalVar "sparrow_arg" Cil.voidType), Cil.NoOffset), [Cil.Lval argc; Cil.Lval argv], loc) in
    
      let (optind, optarg) = 
        ((Cil.Var (Cil.makeGlobalVar "optind" Cil.intType), Cil.NoOffset),
         (Cil.Var (Cil.makeGlobalVar "optarg" Cil.charPtrType), Cil.NoOffset))
      in
      let opt_node = Node.make () in
      let opt_cmd = Cmd.Ccall (None, Cil.Lval (Cil.Var (Cil.makeGlobalVar "sparrow_opt" Cil.voidType), Cil.NoOffset), [Cil.Lval optind; Cil.Lval optarg], loc) in
      let g = g |> add_cmd arg_node arg_cmd |> add_cmd opt_node opt_cmd |> add_edge Node.ENTRY arg_node |> add_edge arg_node opt_node in
      (opt_node, g)
    else (Node.ENTRY, g)
  in
  (* generate alloc cmds for static allocations *) 
  let (term, g) = generate_allocs fd fd.slocals term g in
  let g = add_edge term entry g in
  let nodes = nodesof g in
  let lasts = List.filter (fun n -> succ n g = []) nodes in 
    g
    |> list_fold (fun last -> add_edge last Node.EXIT) lasts  
    |> generate_assumes    
    |> flatten_instructions
    |> remove_if_loop 
    |> transform_allocs fd        (* generate alloc cmds for dynamic allocations *)
    |> transform_str_allocs fd    (* generate salloc (string alloc) cmds *)
    |> transform_field_access fd  (* transform s.f -> *s.f *)
    |> remove_empty_nodes
    |> insert_return_nodes
    |> insert_return_before_exit

let generate_global_proc : Cil.global list -> Cil.fundec -> t 
= fun globals fd ->
  let entry = Node.ENTRY in
  let (term, g) = 
    List.fold_left (fun (node, g) x ->
        match x with
          Cil.GVar (var, init, loc) -> 
            process_gvar fd (Cil.var var) init loc node g
        | Cil.GVarDecl (var, loc) -> process_gvardecl fd (Cil.var var) loc node g
        | Cil.GFun (fundec, loc) -> process_fundecl fd fundec loc node g
        | _ -> (node, g)) (entry, empty fd) globals
  in
  let (main_dec, main_loc) = match get_main_dec globals with Some (d, l) -> (d, l) 
               | _ -> raise (Failure "Not Found: main")
  in
  let call_node = Node.make () in
  let call_cmd = Cmd.Ccall (None, Lval (Var main_dec.svar, NoOffset), [], main_loc) in
    g 
    |> add_cmd call_node call_cmd
    |> add_edge term call_node
    |> add_edge call_node Node.EXIT
    |> generate_assumes 
    |> flatten_instructions
    |> remove_if_loop
    |> transform_str_allocs fd        (* generate salloc (string alloc) cmds *)
    |> transform_field_access fd  (* transform s.f -> *s.f *)
    |> remove_empty_nodes
    |> insert_return_nodes
 
let unreachable_node : t -> node BatSet.t
=fun g ->
  let all_nodes = list2set (nodesof g) in
  let rec remove_reachable_node' work acc =
    if BatSet.is_empty work then acc else
      let (node, work) = BatSet.pop work in
      if BatSet.mem node acc then
        let acc = BatSet.remove node acc in
        let succs = BatSet.remove node (list2set (succ node g)) in
        let work = BatSet.union work succs in
        remove_reachable_node' work acc
      else remove_reachable_node' work acc in
  remove_reachable_node' (BatSet.singleton Node.ENTRY) all_nodes

let print_dot : out_channel -> t -> unit
=fun chan g ->
  fprintf chan "digraph %s {\n" g.fd.svar.vname;
  fprintf chan "{\n";
  fprintf chan 
    "node [shape=box]\n";
  G.iter_vertex (fun v ->
    fprintf chan 
      "%s [label=\"%s: %s\" %s]\n" 
      (Node.to_string v) 
      (Node.to_string v)
      (Cmd.to_string (find_cmd v g))
      (if is_callnode v g then "style=filled color=grey"
       else "")
  ) g.graph;
  fprintf chan "}\n";
  G.iter_edges (fun v1 v2 -> 
      fprintf chan "%s -> %s\n" (Node.to_string v1) (Node.to_string v2)
  ) g.graph;
  fprintf chan "}\n"

let print_dom_fronts dom_fronts = 
  BatMap.iter (fun node fronts ->
    prerr_string (Node.to_string node ^ ": ");
    BatSet.iter (fun fr -> prerr_string (Node.to_string fr ^ " "))
    fronts;
    prerr_endline ""
  ) dom_fronts

let print_dom_tree dom_tree = 
  prerr_endline (string_of_map Node.to_string Node.to_string dom_tree)
 
module Json = Yojson.Safe

let to_json : t -> Json.json
= fun g ->
  let nodes = `Assoc (G.fold_vertex (fun v nodes -> 
              (Node.to_string v, 
                `List [
                  `String (Cmd.to_string (find_cmd v g)); 
                  `Bool false;
                  `Bool (is_callnode v g)])::nodes) g.graph [])
  in
  let edges = `List (G.fold_edges (fun v1 v2 edges ->
              (`List [`String (Node.to_string v1); 
                      `String (Node.to_string v2)
                     ])::edges) g.graph []) in
  `Assoc [("nodes", nodes); 
          ("edges", edges)]


module BasicBlocks = Graph.Leaderlist.Make(G)

let opt_basic_block : t -> t 
= fun cfg ->
  let bbs = BasicBlocks.leader_lists cfg.graph (entryof cfg) in
  let bbs = List.map (fun block ->
                let block = if List.exists (fun node -> is_entry node) block then BatList.drop 1 block else block in
                let block = if List.exists (fun node -> is_exit node) block then BatList.take (List.length block -1) block else block in
                block
      ) bbs 
  in
  let rec split block =
    match block with 
      [] -> []
    | l -> 
      if List.exists (fun node -> is_callnode node cfg) block then
        let hd = BatList.take_while (fun node -> not (is_callnode node cfg)) block in
        let tl = BatList.drop_while (fun node -> not (is_callnode node cfg)) block in
        [List.nth tl 0]::[List.nth tl 1]::hd::(split (BatList.drop 2 tl))
      else [l]
  in 
  let bbs = List.fold_left (fun bbs block ->
              bbs@(split block)
              ) [] bbs 
  in

  List.fold_left (fun g block -> 
      if List.length block <= 1 then g
      else 
        let hd = List.nth block 0 in
        let tl = List.nth block (List.length block - 1) in
        let preds = pred hd g in 
        let succs = succ tl g in
        let cmd = Cmd.Cseq (List.map (fun node -> find_cmd node g) block) in
        let node = Node.make () in
        let g = add_node_with_cmd node cmd g in
        let g = List.fold_left (fun g pred -> add_edge pred node g) g preds in
        let g = List.fold_left (fun g succ -> add_edge node succ g) g succs in
        List.fold_left (fun g node -> remove_node node g) g block
      ) cfg bbs

