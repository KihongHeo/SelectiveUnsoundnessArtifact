open Cil
open Vocab
open BasicDom

class extractGlobalVisitor () = object(self)
  inherit nopCilVisitor
  method vglob (g: Cil.global) = 
    match g with
    | Cil.GVar (var, _, loc)
    | Cil.GVarDecl (var, loc) when not (Cil.isFunctionType var.vtype) ->
      print_endline (CilHelper.s_location loc ^ ":" ^ var.vname);
      Cil.DoChildren
    | _ -> Cil.DoChildren
  method vinst (i: Cil.instr) = 
    match i with
    | Cil.Set (((Var v, _) as lval), _, loc) when v.vglob ->
      print_endline (CilHelper.s_location loc ^ ":" ^ CilHelper.s_lv lval);
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let is_proc_G loc = 
  try 
    (Loc.get_proc loc) = InterCfg.global_proc
  with _ -> false

let unsound_target k v =
  if ItvDom.Val.pow_proc_of_val v |> PowProc.is_empty then
    match k with
    | Loc.GVar _ -> true 
    | Loc.Allocsite a -> Allocsite.is_global_allocsite a
    | _ -> false
  else false

let extract_global mem =
  ItvDom.Mem.iter (fun k v ->
      if unsound_target k v then
        print_endline (Loc.to_string k)
      else ()) mem;

(*  let vis = new extractGlobalVisitor () in
  ignore (Cil.visitCilFile vis f);*)
  ()
