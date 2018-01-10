open Vocab
open BasicDom
open ItvSem
open ItvDom
open Cil
open IntraCfg
open Cmd
open AlarmExp 
open ArrayBlk

type target = BO | ND | DZ 
type status = Proven | UnProven | BotAlarm
type part_unit = Cil.location 

let status_to_string = function Proven -> "Proven" | UnProven -> "UnProven" | _ -> "BotAlarm"

type query = {
  node : InterCfg.node;
  exp : AlarmExp.t;
  loc : Cil.location;
  allocsite : Allocsite.t option; 
  status : status;
  desc : string
}

let is_unproven : query -> bool
=fun q -> q.status = UnProven

let get_pid : query -> string
=fun q -> InterCfg.Node.get_pid q.node

let get qs status =  
  List.filter (fun q -> q.status = status) qs

let string_of_alarminfo offset size = 
  "offset: " ^ Itv.to_string offset ^ ", size: " ^ Itv.to_string size

let partition : query list -> (part_unit, query list) BatMap.t
=fun queries ->
  list_fold (fun q m ->
    let p_als = try BatMap.find q.loc m with _ -> [] in
      BatMap.add q.loc (q::p_als) m
  ) queries BatMap.empty

let ignore_alarm a arr offset =
  (!Options.opt_bugfinder >= 1 
    && (Allocsite.is_string_allocsite a
       || arr.ArrInfo.size = Itv.top
       || arr.ArrInfo.size = Itv.one
       || offset = Itv.top && arr.ArrInfo.size = Itv.nat
       || offset = Itv.zero))
  || (!Options.opt_bugfinder >= 2
      && not (Itv.is_const arr.ArrInfo.size))
  || (!Options.opt_bugfinder >= 3
       && (offset = Itv.top 
          || Itv.meet arr.ArrInfo.size Itv.zero <> Itv.bot
          || (offset = Itv.top && arr.ArrInfo.offset <> Itv.top)))


let check_bo v1 v2opt : (status * Allocsite.t option * string) list = 
  let arr = Val.array_of_val v1 in
  if ArrayBlk.eq arr ArrayBlk.bot then [(BotAlarm, None, "Array is Bot")] else
    ArrayBlk.foldi (fun a arr lst ->
      let offset = 
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (Val.itv_of_val v2) in
      let status =
        try 
          if Itv.is_bot offset || Itv.is_bot arr.ArrInfo.size then BotAlarm
          else if ignore_alarm a arr offset then Proven
          else 
            let (ol, ou) = (Itv.lower offset, Itv.upper offset) in
            let sl = Itv.lower arr.ArrInfo.size in
            if ou >= sl || ol < 0 then UnProven
            else Proven
        with _ -> UnProven
      in
      (status, Some a, string_of_alarminfo offset arr.ArrInfo.size)::lst
    ) arr []

let check_nd v1 : (status * Allocsite.t option * string) list = 
  let ploc = Val.pow_loc_of_val v1 in
  if PowLoc.eq ploc PowLoc.bot then [(BotAlarm, None, "PowLoc is Bot")] else
    if PowLoc.mem Loc.null ploc then 
      [(UnProven, None, "Null Dereference")]
    else [(Proven, None, "")]

let inspect_aexp_bo : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
    | ArrayExp (lv,e,loc) ->
        let v1 = Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv mem) mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
    | DerefExp (e,loc) ->
        let v = eval (InterCfg.Node.get_pid node) e mem in
        let lst = check_bo v None in 
          if Val.eq Val.bot v then 
            List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
          else 
            List.map (fun (status,a,desc) -> 
              if status = BotAlarm
              then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a; desc = "valid pointer dereference" }
              else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; desc = desc }) lst
    | Strcpy (e1, e2, loc) ->
        let v1 = eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = eval (InterCfg.Node.get_pid node) e2 mem in
        let v2 = Val.of_itv (ArrayBlk.nullof (Val.array_of_val v2)) in 
        let lst = check_bo v1 (Some v2) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
    | Strcat (e1, e2, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in 
        let np1 = ArrayBlk.nullof (Val.array_of_val v1) in
        let np2 = ArrayBlk.nullof (Val.array_of_val v2) in
        let np = Val.of_itv (Itv.plus np1 np2) in
        let lst = check_bo v1 (Some np) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
    | Strncpy (e1, e2, e3, loc) 
    | Memcpy (e1, e2, e3, loc) 
    | Memmove (e1, e2, e3, loc) ->
        let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 mem in
        let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 mem in
        let e3_1 = Cil.BinOp (Cil.MinusA, e3, Cil.mone, Cil.intType) in
        let v3 = ItvSem.eval (InterCfg.Node.get_pid node) e3_1 mem in
        let lst1 = check_bo v1 (Some v3) in
        let lst2 = check_bo v2 (Some v3) in
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) (lst1@lst2)
    | _ -> []) @ queries


let inspect_aexp_nd : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  (match aexp with
  | DerefExp (e,loc) ->
    let v = eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_nd v in 
      if Val.eq Val.bot v then 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
      else 
        List.map (fun (status,a,desc) -> 
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a; desc = "valid pointer dereference" }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; desc = desc }) lst
  | _ -> []) @ queries

let check_dz v = 
  let v = Val.itv_of_val v in
  if Itv.le Itv.zero v then 
    [(UnProven, None, "Divide by "^Itv.to_string v)]
  else [(Proven, None, "")]

let inspect_aexp_dz : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
= fun node aexp mem queries -> 
  (match aexp with 
      DivExp (_, e, loc) ->
      let v = eval (InterCfg.Node.get_pid node) e mem in
      let lst = check_dz v in 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = None; status = status; desc = desc }) lst
  | _ -> []) @ queries

let machine_gen_code : query -> bool 
= fun q -> 
  (* yacc-generated code *)
  Filename.check_suffix q.loc.Cil.file ".y" || Filename.check_suffix q.loc.Cil.file ".yy.c" ||
  Filename.check_suffix q.loc.Cil.file ".simple" ||
  (* sparrow-generated code *)
  InterCfg.Node.get_pid q.node = InterCfg.global_proc 
  
let rec unsound_exp : Cil.exp -> bool
= fun e ->
  match e with 
  | Cil.BinOp (Cil.PlusPI, Cil.Lval (Cil.Mem _, _), _, _) -> true
  | Cil.BinOp (b, _, _, _) when b = Mod || b = Cil.Shiftlt || b = Shiftrt || b = BAnd 
      || b = BOr || b = BXor || b = LAnd || b = LOr -> true
  | Cil.BinOp (bop, Cil.Lval (Cil.Var _, _), Cil.Lval (Cil.Var _, _), _) 
    when bop = Cil.PlusA || bop = Cil.MinusA -> true 
  | Cil.BinOp (_, e1, e2, _) -> (unsound_exp e1) || (unsound_exp e2)
  | Cil.CastE (_, e) -> unsound_exp e
  | Cil.Lval lv -> unsound_lv lv
  | _ -> false

and unsound_lv : Cil.lval -> bool = function 
  | (_, Cil.Index _) -> true
  | (Cil.Var v, _) -> is_global_integer v || is_union v.vtype || is_temp_integer v
  | (Cil.Mem _, Cil.NoOffset) -> true
  | (_, _) -> false
and is_global_integer v = v.vglob && Cil.isIntegralType v.vtype
and is_union typ = 
  match Cil.unrollTypeDeep typ with 
    Cil.TPtr (Cil.TComp (c, _), _) -> not c.cstruct
  | _ -> false
and is_temp_integer v = 
  !Options.opt_bugfinder >= 2
  && (try String.sub v.vname 0 3 = "tmp" with _ -> false)
  && Cil.isIntegralType v.vtype

let unsound_aexp : AlarmExp.t -> bool = function 
  | ArrayExp (lv, e, _) -> unsound_exp e
  | DerefExp (e, _) -> unsound_exp e
  | _ -> false

let formal_param : Global.t -> query -> bool
= fun global q ->
  let cfg = InterCfg.cfgof global.Global.icfg (InterCfg.Node.get_pid q.node) in
  let formals = IntraCfg.get_formals cfg in
  let rec find_exp = function
    | Cil.BinOp (_, e1, e2, _) -> (find_exp e1) || (find_exp e2)
    | Cil.CastE (_, e) -> find_exp e
    | Cil.Lval lv -> find_lv lv
    | _ -> false
  and find_lv = function 
    | (Cil.Var v, _) -> (List.mem v.vname formals) && Cil.isIntegralType v.vtype
    | (_, _) -> false
  in
  match q.exp with 
  | ArrayExp (_, e, _) | DerefExp (e, _) -> find_exp e
  | _ -> false
 
let unsound_filter : Global.t -> query list -> query list
= fun global ql ->
  let filtered = 
    List.filter (fun q -> 
      not (machine_gen_code q) 
      && not (unsound_aexp q.exp) 
(*     not (formal_param global q)*)) ql
  in
  let partition = 
    list_fold (fun q m ->
      let p_als = try BatMap.find (q.loc,q.node) m with _ -> [] in
        BatMap.add (q.loc,q.node) (q::p_als) m
    ) filtered BatMap.empty
  in
  BatMap.fold (fun ql result ->
      if List.length (get ql UnProven) > 3 then 
        (List.map (fun q -> { q with status = Proven}) ql)@result
      else ql@result) partition []

let filter : query list -> status -> query list
= fun qs s -> List.filter (fun q -> q.status = s) qs

let rec collect : InterCfg.Node.t -> Cmd.t -> Mem.t * Global.t -> (AlarmExp.t * Mem.t) list
= fun node cmd (mem,global) ->
  match cmd with 
    Cmd.Cseq cmds ->
      List.fold_left (fun (l,mem,pos) c -> 
          let aexps = collect node c (mem, global) in
          let mem = ItvSem.run_cmd ~position:pos AbsSem.Strong node c (mem,global) |> fst in
          (aexps@l, mem, pos+1)) ([], mem, 0) cmds 
      |> (fun (l, _, _) -> l)
  | _ -> AlarmExp.collect cmd |> List.map (fun x -> (x, mem))

let generate : Global.t * Table.t * target -> query list
=fun (global,inputof,target) ->
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof icfg node in
    let aexps = collect node cmd (mem,global) in
    let qs = list_fold (fun (aexp,mem) ->
      if mem = Mem.bot then id (* dead code *)
      else 
        match target with 
          BO -> inspect_aexp_bo node aexp mem
        | ND -> inspect_aexp_nd node aexp mem
        | DZ -> inspect_aexp_dz node aexp mem
      ) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst
  |> opt (!Options.opt_bugfinder > 0) (unsound_filter global)

let generate_with_mem : Global.t * Mem.t * target -> query list
=fun (global,mem,target) ->
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
    list_fold (fun node ->
      let cmd = InterCfg.cmdof icfg node in
      let aexps = AlarmExp.collect cmd in 
        if mem = Mem.bot then id (* dead code *)
        else 
          match target with 
            BO -> list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps 
          | ND -> list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps
          | DZ -> list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps
    ) nodes []

let sort_queries : query list -> query list = 
fun queries ->
  List.sort (fun a b -> 
    if Pervasives.compare a.loc.file b.loc.file = 0 then 
    begin
      if Pervasives.compare a.loc.line b.loc.line = 0 then 
        Pervasives.compare a.exp b.exp 
      else Pervasives.compare a.loc.line b.loc.line
    end
    else Pervasives.compare a.loc.file b.loc.file) queries 

let sort_partition : (part_unit * query list) list -> (part_unit * query list) list = 
fun queries ->
  List.sort (fun (a,_) (b,_) -> 
    if Pervasives.compare a.file b.file = 0 then 
      Pervasives.compare a.line b.line 
    else Pervasives.compare a.file b.file) queries 

let get_status : query list -> status
=fun queries -> 
  if List.exists (fun q -> q.status = BotAlarm) queries then BotAlarm
  else if List.exists (fun q -> q.status = UnProven) queries then  UnProven
  else if List.for_all (fun q -> q.status = Proven) queries then Proven
  else raise (Failure "Report.ml: get_status")

let get_proved_query_point : query list -> part_unit BatSet.t
=fun queries ->
  let all = partition queries in
  let unproved = partition (get queries UnProven) in
  let all_loc = BatMap.foldi (fun l _ -> BatSet.add l) all BatSet.empty in
  let unproved_loc = BatMap.foldi (fun l _ -> BatSet.add l) unproved BatSet.empty in
    BatSet.diff all_loc unproved_loc
 
let string_of_query q = 
  (CilHelper.s_location q.loc)^ " "^
  (AlarmExp.to_string q.exp) ^ " @" ^
  (InterCfg.Node.to_string q.node) ^ ":  " ^ 
  (match q.allocsite with 
    Some a -> Allocsite.to_string a
   | _ -> "") ^ "  " ^
  q.desc ^ " " ^ status_to_string (get_status [q])

let display_alarms title alarms_part = 
  prerr_endline "";
  prerr_endline ("= " ^ title ^ " =");
  let alarms_part = BatMap.bindings alarms_part in
  let alarms_part = sort_partition alarms_part in
  ignore (List.fold_left (fun k (part_unit, qs) ->
    prerr_string (string_of_int k ^ ". " ^ CilHelper.s_location part_unit ^ " "); 
    prerr_string (string_of_set id (list2set (List.map (fun q -> InterCfg.Node.get_pid q.node) qs)));
    prerr_string (" " ^ status_to_string (get_status qs));
    prerr_newline ();
    List.iter (fun q -> 
      prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
      prerr_string (InterCfg.Node.to_string q.node);
      prerr_string ( ":  " ^ q.desc ^ " " ^ status_to_string (get_status [q]));
      (match q.allocsite with Some a ->
        prerr_endline ( ", allocsite: " ^ Allocsite.to_string a)
       | _ -> prerr_newline ())
    ) qs;
   k+1
  ) 1 alarms_part) 
 
let print : bool -> query list -> unit
=fun summary_only queries ->
  let all = partition queries in
  let unproven = partition (get queries UnProven) in
  let bot = partition (get queries BotAlarm) in
  if not summary_only then
    begin
      display_alarms "Alarms" (if !Options.opt_show_all_query then all else unproven); 
(*      display_alarms "Alarms with Bottom" bot*)
    end
  else ();
  prerr_endline "";
  prerr_endline ("#queries                 : " ^ i2s (List.length queries));
  prerr_endline ("#queries mod alarm point : " ^ i2s (BatMap.cardinal all));
  prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries)));
  prerr_endline ("#unproven                : " ^ i2s (BatMap.cardinal unproven));
  prerr_endline ("#bot-involved            : " ^ i2s (BatMap.cardinal bot))
    

let print_raw : bool -> query list -> unit
=fun summary_only queries ->
  let unproven = List.filter (fun x -> x.status = UnProven) queries in
  let botalarm = List.filter (fun x -> x.status = BotAlarm) queries in
  prerr_newline ();
  prerr_endline ("= "^"Alarms"^ "=");
  ignore (List.fold_left (fun k q ->
    prerr_string (string_of_int k ^ ". "); 
    prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
    prerr_string (InterCfg.Node.to_string q.node);
    prerr_string ("  ");
    prerr_string (CilHelper.s_location q.loc);
    prerr_endline ( ":  " ^ q.desc );
    k+1
    ) 1 (sort_queries unproven)); 
  prerr_endline "";
  prerr_endline ("#queries                 : " ^ i2s (List.length queries));
  prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries)));
  prerr_endline ("#unproven                : " ^ i2s (List.length unproven));
  prerr_endline ("#bot-involved            : " ^ i2s (List.length botalarm))
