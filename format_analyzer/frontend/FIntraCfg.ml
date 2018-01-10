(** CFG for a Function. *)

open VocabB
open Cil
open Printf 
open DFSetAVL
module Cmd = struct
  type t = 
  | Cinstr of instr list 
  | Cif of exp * block * block * location 
  | CLoop of location
  (* final graph has the following cmds only *)
  | Cset of lval * exp * location
  | Cexternal of lval * location 
  | Calloc of lval * alloc * location
  | Csalloc of lval * string * location
  | Cfalloc of lval * fundec * location
  | Cassume of exp * location
  | Ccall of lval option * exp * exp list * location 
  | Creturn of exp option * location
  | Casm of attributes * string list * 
            (string option * string * lval) list *
            (string option * string * exp) list *
            string list * location
  | Crefute of alarm_exp * location
  | Cskip
  and alloc = 
  | Array of exp 
  and alarm_exp = 
  | ArrayExp of lval * exp * location
  | DerefExp of exp * location

  let s_location : Cil.location -> string
  =fun loc ->
    let file = try 
      let idx = String.rindex loc.file '/' in
      let len = String.length loc.file in
        String.sub loc.file (idx+1) (len-idx-1) 
      with _ -> loc.file
    in file ^ ":" ^ string_of_int loc.line

  let fromCilStmt: Cil.stmtkind -> t 
  =fun s -> 
    match s with
    | Instr instrs -> Cinstr instrs
    | If (exp,b1,b2,loc) -> Cif (exp,b1,b2,loc)
    | Loop (_,loc,_,_) -> CLoop loc
    | Return (expo,loc) -> Creturn (expo,loc)
    | _ -> Cskip

end

type node = FIntraNode.t
type cmd  = Cmd.t

module G = Graph.Persistent.Digraph.ConcreteBidirectional(FIntraNode)
module Topo = Graph.Topological.Make(G)
module Scc = Graph.Components.Make(G)
module Dom = Graph.Dominator.Make (G)

type t = {
  fd                : Cil.fundec;
  graph             : G.t;
  cmd_map           : (node, cmd) BatMap.t;
  dom_fronts        : (node, node BatSet.t) BatMap.t;
  dom_tree          : G.t;
  scc_list          : node list list
}

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
  try if n = FIntraNode.ENTRY || n = FIntraNode.EXIT then Cmd.Cskip
      else BatMap.find n g.cmd_map 
  with _ -> 
     raise (Failure ("Can't find cmd of " ^ FIntraNode.to_string n))

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
  let new_node = FIntraNode.make() in
    (add_cmd new_node cmd 
     >>> remove_edge n s
     >>> add_edge n new_node
     >>> add_edge new_node s) g

let pred : node -> t -> node list
=fun n g -> G.pred g.graph n

let succ : node -> t -> node list
=fun n g -> G.succ g.graph n

let fold_vertex f g a = G.fold_vertex f g.graph a

let is_entry : node -> bool
=fun node ->
  match node with
  | FIntraNode.ENTRY -> true
  | _ -> false

let is_exit : node -> bool
=fun node ->
  match node with
  | FIntraNode.EXIT -> true
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

let entryof _ = FIntraNode.ENTRY
let exitof _ = FIntraNode.EXIT

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
              | t::l,_ -> if t.sid = FIntraNode.getid s1 then s1,s2 else s2,s1
              | _,t::l -> if t.sid = FIntraNode.getid s2 then s1,s2 else s2,s1 in
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
      let pairs = List.map (fun c -> (FIntraNode.make(),c)) cmds in
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
  let alloc_node = FIntraNode.make () in
  let size = Cil.BinOp (Cil.Mult, Cil.sizeOf typ, exp, Cil.intType) in
  let alloc_cmd = Cmd.Calloc (lv, Cmd.Array size, loc) in
  (alloc_node, g |> add_cmd alloc_node alloc_cmd |> add_edge entry alloc_node)

let rec make_nested_array : Cil.fundec -> Cil.lval -> Cil.typ -> Cil.exp -> Cil.location -> node -> t -> (node * t)
= fun fd lv typ exp loc entry g ->
  match typ with
    TArray (t, Some size, _) ->
      let init_node = FIntraNode.make () in
      let idxinfo = Cil.makeTempVar fd (Cil.TInt (IInt, [])) in
      let idx = (Cil.Var idxinfo, Cil.NoOffset) in 
      let init_value = Cil.Const (Cil.CInt64 (Int64.zero, IInt, None)) in
      let init_cmd = Cmd.Cset (idx, init_value, loc) in
      let g = add_cmd init_node init_cmd g in
      let skip_node = FIntraNode.make () in
      let g = add_cmd skip_node Cmd.Cskip g in
      let g = add_edge init_node skip_node g in 
      let g = add_edge entry init_node g in
      let assume_node = FIntraNode.make () in
      let typ = Cil.TInt (Cil.IInt, []) in
      let cond = Cil.BinOp (Cil.Lt, Cil.Lval idx, exp, typ) in
      let assume_cmd = Cmd.Cassume (cond, loc) in
      let g = add_cmd assume_node assume_cmd g in
      let g = add_edge skip_node assume_node g in
      let nassume_node = FIntraNode.make () in
      let nassume_cmd = Cmd.Cassume (Cil.UnOp (Cil.LNot, cond, typ), loc) in
      let g = add_cmd nassume_node nassume_cmd g in
      let g = add_edge skip_node nassume_node g in
      let element = Cil.addOffsetLval (Index (Lval (Var idxinfo, NoOffset), NoOffset)) lv in

      let tmp = (Cil.Var (Cil.makeTempVar fd (Cil.TPtr (Cil.TVoid [], []))), Cil.NoOffset) in
      let (term, g) = make_array fd tmp typ size loc assume_node g in
      let cast_node = FIntraNode.make () in  
      let cast_cmd = Cmd.Cset (element, Cil.CastE (TPtr (typ, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      
      let (term, g) = make_nested_array fd element t size loc cast_node g in
      let incr_node = FIntraNode.make () in
      let incr_cmd = Cmd.Cset (idx, Cil.BinOp (Cil.PlusA, Cil.Lval idx, Cil.Const (Cil.CInt64 (Int64.one, IInt, None)), typ), loc) in
      let g = add_cmd incr_node incr_cmd g in
      let g = add_edge term incr_node g in
      let g = add_edge incr_node skip_node g in
        (nassume_node, g) 
  | _ -> (entry, g)



(* struct S lv[e] *)
let rec make_comp_array : Cil.fundec -> Cil.lval -> Cil.typ -> Cil.exp -> Cil.location -> node -> t -> (node * t)
= fun fd lv typ exp loc entry g ->
  match typ with 
    TComp (comp, _) -> 
      let tmp = (Cil.Var (Cil.makeTempVar fd (Cil.TPtr (Cil.TVoid [], []))), Cil.NoOffset) in
      let (term, g) = make_array fd tmp typ exp loc entry g in
      let cast_node = FIntraNode.make () in  
      let cast_cmd = Cmd.Cset (lv, Cil.CastE (TPtr (typ, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in

      let init_node = FIntraNode.make () in
      let tempinfo = Cil.makeTempVar fd (Cil.TInt (IInt, [])) in
      let temp = (Cil.Var tempinfo, Cil.NoOffset) in 
      let init_value = Cil.Const (Cil.CInt64 (Int64.zero, IInt, None)) in
      let init_cmd = Cmd.Cset (temp, init_value, loc) in
      let g = add_cmd init_node init_cmd g in
      let g = add_edge cast_node init_node g in
      let skip_node = FIntraNode.make () in
      let g = add_cmd skip_node Cmd.Cskip g in
      let g = add_edge init_node skip_node g in 
      let assume_node = FIntraNode.make () in
      let typ = Cil.TInt (Cil.IInt, []) in
      let cond = Cil.BinOp (Cil.Lt, Cil.Lval temp, exp, typ) in
      let assume_cmd = Cmd.Cassume (cond, loc) in
      let g = add_cmd assume_node assume_cmd g in
      let g = add_edge skip_node assume_node g in
      let nassume_node = FIntraNode.make () in
      let nassume_cmd = Cmd.Cassume (Cil.UnOp (Cil.LNot, cond, typ), loc) in
      let g = add_cmd nassume_node nassume_cmd g in
      let g = add_edge skip_node nassume_node g in
      let element = Cil.addOffsetLval (Index (Lval (Var tempinfo, NoOffset), NoOffset)) lv in
      let (term, g) = generate_allocs_field comp.cfields element fd assume_node g in
      let incr_node = FIntraNode.make () in
      let incr_cmd = Cmd.Cset (temp, Cil.BinOp (Cil.PlusA, Cil.Lval temp, Cil.Const (Cil.CInt64 (Int64.one, IInt, None)), typ), loc) in
      let g = add_cmd incr_node incr_cmd g in
      let g = add_edge term incr_node g in
      let g = add_edge incr_node skip_node g in
        (nassume_node, g) 
  | TNamed (typeinfo, _) -> make_comp_array fd lv typeinfo.ttype exp loc entry g
  | _ -> (entry, g)


and generate_allocs_field : Cil.fieldinfo list -> Cil.lval -> Cil.fundec -> node -> t ->  (node * t) 
=fun fl lv fd entry g ->
  match fl with 
    [] -> (entry, g)
  | fieldinfo::t -> 
      begin
      match fieldinfo.ftype with 
        TArray ((TComp (_, _)) as typ, Some exp, _) ->
          let field = (Cil.Mem (Cil.Lval lv), Cil.Field (fieldinfo, Cil.NoOffset)) in 
          let (term, g) = make_comp_array fd field typ exp fieldinfo.floc entry g in
          generate_allocs_field t lv fd term g
      | TArray (typ, Some exp, _) -> 
          let field = (Cil.Mem (Cil.Lval lv), Cil.Field (fieldinfo, Cil.NoOffset))  in 
          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp typ exp fieldinfo.floc entry g in
          let cast_node = FIntraNode.make () in  
          let cast_cmd = Cmd.Cset (field, Cil.CastE (Cil.TPtr (typ, []), Cil.Lval tmp), fieldinfo.floc) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = make_nested_array fd field typ exp fieldinfo.floc cast_node g in
            generate_allocs_field t lv fd term g
      | TComp (comp, _) ->
          let field = (Cil.Mem (Cil.Lval lv), Cil.Field (fieldinfo, Cil.NoOffset)) in 
(*          let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
          let (term, g) = make_array fd tmp fieldinfo.ftype Cil.one fieldinfo.floc entry g in
          let cast_node = FIntraNode.make () in  
          let cast_cmd = Cmd.Cset (field, Cil.CastE (Cil.TPtr (fieldinfo.ftype, []), Cil.Lval tmp), fieldinfo.floc) in
          let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
          let (term, g) = generate_allocs_field comp.cfields field fd cast_node g in
*)          
          let (term, g) = generate_allocs_field comp.cfields field fd entry g in
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
  | [] -> (entry, g)
  | varinfo::t ->
    begin
      match varinfo.vtype with 
        TArray ((TComp (_, _)) as typ, Some exp, _) -> (* for (i = 0; i < exp; i++) arr[i] = malloc(comp) *)
        let lv = (Cil.Var varinfo, Cil.NoOffset) in 
        let (term, g) = make_comp_array fd lv typ exp varinfo.vdecl entry g in
        generate_allocs fd t term g
      | TArray (typ, Some exp, _) ->
        let lv = (Cil.Var varinfo, Cil.NoOffset) in 
        let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
        let (term, g) = make_array fd tmp typ exp varinfo.vdecl entry g in
        let cast_node = FIntraNode.make () in  
        let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (typ, []), Cil.Lval tmp), varinfo.vdecl) in
        let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
        let (term, g) = make_nested_array fd lv typ exp varinfo.vdecl cast_node g in
        generate_allocs fd t term g
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
    
(* NOTE: CWStr is not supported. *)
let transform_str_allocs : Cil.fundec -> t -> t
= fun fd g ->
  let rec replace_str : Cil.exp -> Cil.exp * (Cil.lval * string) list
  = fun e -> 
    match e with
    | Const (CStr s) -> 
        let tempinfo = Cil.makeTempVar fd (Cil.TPtr (Cil.TInt (IChar, []), [])) in
        let temp = (Cil.Var tempinfo, Cil.NoOffset) in 
          (Lval temp, [(temp, s)])
    | Const (CWStr ws) ->
       let tempinfo = Cil.makeTempVar fd (Cil.TPtr (Cil.TInt (IChar, []), [])) in
       let temp = (Cil.Var tempinfo, Cil.NoOffset) in
       let s = list_fold (fun _ acc -> "a" ^ acc) ws "" in
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
                    let new_node = FIntraNode.make () in
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
            let (empty_node, last_node) = (FIntraNode.make (), FIntraNode.make ()) in
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
            let (empty_node, last_node) = (FIntraNode.make (), FIntraNode.make ()) in
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
            let (empty_node, last_node) = (FIntraNode.make (), FIntraNode.make ()) in
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
            let (empty_node, last_node) = (FIntraNode.make (), FIntraNode.make ()) in
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
        match typ with 
          TComp (_, _) -> 
            let g = add_cmd node Cmd.Cskip g in
              make_comp_array fd lv typ e loc node g
        | _ ->
            let cmd = Cmd.Calloc (lv, Cmd.Array exp, loc) in
            let g = add_cmd node cmd g in
            (node, g)
      end
    | SizeOf typ | CastE (_, SizeOf typ) ->
      begin
        let typ = Cil.unrollTypeDeep typ in
        match typ with 
          TComp (comp, _) -> 
            let cmd = Cmd.Calloc (lv, Cmd.Array exp, loc) in
            let g = add_cmd node cmd g in 
              generate_allocs_field comp.cfields lv fd node g
        | _ -> 
          let cmd = Cmd.Calloc (lv, Cmd.Array exp, loc) in
          let g = add_cmd node cmd g in
            (node, g)
      end
    | SizeOfE e -> transform lv (SizeOf (Cil.typeOf e)) loc node g
    | e ->  
      let cmd = Cmd.Calloc (lv, Cmd.Array exp, loc) in
      let g = add_cmd node cmd g in
      (node, g)
  in
  fold_vertex (fun n g ->
      match find_cmd n g with 
        Cmd.Ccall (Some lv, Lval (Var varinfo, _), args, loc) ->
          if varinfo.vname = "malloc" || varinfo.vname = "__builtin_alloca"  then
            let new_node = FIntraNode.make () in
            let preds = pred n g in 
            let succs = succ n g in
            let g = List.fold_left (fun g s -> remove_edge n s g) g succs in 
            let g = List.fold_left (fun g p -> remove_edge p n g) g preds in 
            let g = remove_node n g in
            let g = List.fold_left (fun g p -> add_edge p new_node g) g preds in 
            let (term, g) = transform lv (List.hd args) varinfo.vdecl new_node g in
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
    | _ -> add_new_node node (Cmd.Creturn (None, locUnknown)) FIntraNode.EXIT acc
  in
  list_fold add_return (pred FIntraNode.EXIT g) g

let compute_dom : t -> t
=fun g -> 
  let idom = Dom.compute_idom g.graph FIntraNode.ENTRY in
  let dom_tree_f = Dom.idom_to_dom_tree g.graph idom in
  let dom_fronts_f = Dom.compute_dom_frontier g.graph dom_tree_f idom in
  let nodes = nodesof g in
  let dom_tree = 
    let add_edge x y graph = G.add_edge graph x y in
    let add_idoms x graph = list_fold (add_edge x) (dom_tree_f x) graph in
    list_fold add_idoms nodes G.empty in
  let dom_fronts = 
    let add_fronts x graph =
      BatMap.add x (BatSet.of_list (dom_fronts_f x)) graph in
    list_fold add_fronts nodes BatMap.empty in
  { g with dom_tree = dom_tree; dom_fronts = dom_fronts }

let compute_scc : t -> t
=fun g -> { g with scc_list = Scc.scc_list g.graph }


let loopInfo = ref []

let add_loopinfo (pid, loophead, break, continue, bodyList) =
 let t_intra = function
  | FIntraNode.ENTRY -> IntraNode.Entry
  | FIntraNode.EXIT -> IntraNode.Exit
  | FIntraNode.Node i -> IntraNode.Node i
 in
 let fold_stmts acc stmt =
   (pid, t_intra (FIntraNode.fromCilStmt stmt)) :: acc
 in
 let bodyList = List.fold_left fold_stmts [] bodyList in
 loopInfo :=
   ((pid, t_intra loophead), (pid, t_intra break), (pid, t_intra continue), bodyList) ::
   !loopInfo;
 ()

let fromFunDec : Cil.fundec -> string -> t
=fun fd pid -> 
  let entry = FIntraNode.fromCilStmt (List.nth fd.sallstmts 0) in
  let g = 
    (* add nodes *)
    (list_fold (fun s ->
        add_node_with_cmd (FIntraNode.fromCilStmt s) (Cmd.fromCilStmt s.skind)
      ) fd.sallstmts 
    >>>
    (* add edges *)
    list_fold (fun stmt ->
      list_fold (fun succ ->
          match stmt.skind with
          | Loop (block, _, Some continue, Some break) ->         
            add_loopinfo
              (pid, FIntraNode.fromCilStmt succ, FIntraNode.fromCilStmt break,
            FIntraNode.fromCilStmt continue, block.bstmts);
            add_edge (FIntraNode.fromCilStmt stmt) (FIntraNode.fromCilStmt succ)
          | _ ->
            add_edge (FIntraNode.fromCilStmt stmt) (FIntraNode.fromCilStmt succ)
        ) stmt.succs
      ) fd.sallstmts
    ) (empty fd) in
  (* generate alloc cmds for static allocations *) 
  let (term, g) = generate_allocs fd fd.slocals FIntraNode.ENTRY g in
  let g = add_edge term entry g in
  let nodes = nodesof g in
  let lasts = List.filter (fun n -> succ n g = []) nodes in 
    g
    |> list_fold (fun last -> add_edge last FIntraNode.EXIT) lasts  
    |> generate_assumes    
    |> flatten_instructions
    |> remove_if_loop 
    |> transform_allocs fd        (* generate alloc cmds for dynamic allocations *)
    |> transform_str_allocs fd    (* generate salloc (string alloc) cmds *)
    (* |> remove_empty_nodes *)
    |> insert_return_nodes
    |> insert_return_before_exit

let rec process_gvardecl : Cil.fundec -> Cil.lval -> Cil.location -> node -> t -> (node * t)
= fun fd lv loc entry g ->
  match Cil.typeOfLval lv with 
    TArray ((TComp (comp, _)) as typ, Some exp, _) -> (* for (i = 0; i < exp; i++) arr[i] = malloc(comp) *)
      make_comp_array fd lv typ exp loc entry g
  | TArray (typ, Some exp, _) ->
      let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
      let (term, g) = make_array fd tmp typ exp loc entry g in
      let cast_node = FIntraNode.make () in  
      let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (typ, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      let (term, g) = make_nested_array fd lv typ exp loc cast_node g in
      (term, g)
  | TInt (_, _) | TFloat (_, _) ->
      let node = FIntraNode.make () in
      let cmd = Cmd.Cset (lv, Cil.zero, loc) in
      (node, g |> add_cmd node cmd |> add_edge entry node)
(*  | TComp (comp, _) ->
      let tmp = (Cil.Var (Cil.makeTempVar fd Cil.voidPtrType), Cil.NoOffset) in
      let (term, g) = make_array fd tmp (Cil.typeOfLval lv) Cil.one loc entry g in
      let cast_node = FIntraNode.make () in  
      let cast_cmd = Cmd.Cset (lv, Cil.CastE (Cil.TPtr (Cil.typeOfLval lv, []), Cil.Lval tmp), loc) in
      let g = g |> add_cmd cast_node cast_cmd |> add_edge term cast_node in
      let (term, g) = generate_allocs_field comp.cfields lv fd cast_node g in
      (term, g)
  | TNamed (typeinfo,_) ->
      let varinfo = match lv with (Var var, _) -> var | _ -> raise (Failure "process_gvardecl") in
      let varinfo = { varinfo with vtype = typeinfo.ttype} in
      let lval = (Cil.Var varinfo, Cil.NoOffset) in
      process_gvardecl fd lval loc entry g
*)  | _ -> (entry, g)

let rec process_init : Cil.fundec -> Cil.lval -> Cil.init -> Cil.location -> node -> t -> (node * t)
= fun fd lv i loc entry g ->
  match i with 
    SingleInit exp ->
      let new_node = FIntraNode.make () in
      let cmd = Cmd.Cset (lv, exp, loc) in
      let g = add_edge entry new_node (add_cmd new_node cmd g) in
      (new_node, g)
  | CompoundInit (typ, ilist) -> 
      List.fold_left (fun (node, g) (offset, init) ->
          let lv = Cil.addOffsetLval offset lv in
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
  let new_node = FIntraNode.make () in
  let cmd = Cmd.Cfalloc ((Var fundecl.svar, NoOffset), fundecl, loc) in
  let g = add_edge node new_node (add_cmd new_node cmd g) in
  (new_node, g)

let generate_global_proc : Cil.global list -> Cil.fundec -> t 
= fun globals fd ->
  let entry = FIntraNode.ENTRY in
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
  let (argc, argv) = 
    ((Cil.Var (Cil.makeTempVar fd (Cil.TInt (IInt, []))), Cil.NoOffset),
     (Cil.Var (Cil.makeTempVar fd (Cil.TPtr (Cil.TPtr (Cil.TInt (IChar, []), []), []))), Cil.NoOffset))
  in
  let (argc_node, argv_node) = (FIntraNode.make (), FIntraNode.make ()) in
  let (argc_cmd, argv_cmd) = 
    (Cmd.Cexternal (argc, main_loc), Cmd.Calloc (argv, Cmd.Array (Cil.Lval argc), main_loc))
  in
  let assume_argc_node = FIntraNode.make () in
  let assume_argc_cmd = Cmd.Cassume (Cil.BinOp (Cil.Ge, Cil.Lval argc, Cil.one, Cil.intType), main_loc) in
  let alloc_argv_node = FIntraNode.make () in
  let alloc_argv_cmd = Cmd.Cexternal ((Cil.Mem (Cil.Lval argv), Cil.NoOffset), main_loc) in

  let (optind, optarg) = 
    ((Cil.Var (Cil.makeGlobalVar "optind" Cil.intType), Cil.NoOffset),
     (Cil.Var (Cil.makeGlobalVar "optarg" Cil.charPtrType), Cil.NoOffset))
  in
  let (optind_node, optarg_node) = (FIntraNode.make (), FIntraNode.make ()) in
  let (optind_cmd, optarg_cmd) = 
    (Cmd.Cexternal (optind, main_loc), 
     Cmd.Cexternal (optarg, main_loc))
  in

  let progname =
    (Cil.Var (Cil.makeGlobalVar "__progname" Cil.charPtrType), Cil.NoOffset) in
  let progname_node = FIntraNode.make () in
  let progname_cmd = Cmd.Cexternal (progname, main_loc) in

  let call_node = FIntraNode.make () in
  let call_cmd = Cmd.Ccall (None, Lval (Var main_dec.svar, NoOffset), 
                      [Lval argc; Lval argv], main_loc) 
  in
    g 
    |> add_cmd argc_node argc_cmd 
    |> add_cmd argv_node argv_cmd 
    |> add_cmd assume_argc_node assume_argc_cmd
    |> add_cmd alloc_argv_node alloc_argv_cmd
    |> add_cmd optind_node optind_cmd 
    |> add_cmd optarg_node optarg_cmd 
    |> add_cmd progname_node progname_cmd
    |> add_cmd call_node call_cmd
    |> add_edge term argc_node
    |> add_edge argc_node assume_argc_node
    |> add_edge assume_argc_node argv_node
    |> add_edge argv_node alloc_argv_node
    |> add_edge alloc_argv_node optind_node
    |> add_edge optind_node optarg_node
    |> add_edge optarg_node progname_node
    |> add_edge progname_node call_node
    |> add_edge call_node FIntraNode.EXIT
    |> generate_assumes 
    |> flatten_instructions
    |> remove_if_loop
    |> transform_str_allocs fd        (* generate salloc (string alloc) cmds *)
    (* |> remove_empty_nodes *)
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
  remove_reachable_node' (BatSet.singleton FIntraNode.ENTRY) all_nodes
