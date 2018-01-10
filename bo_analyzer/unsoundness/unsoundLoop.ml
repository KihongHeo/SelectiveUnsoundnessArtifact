open Cil
open Vocab

class loopRemoveVisitor (loops: string BatSet.t) = object(self)
  inherit nopCilVisitor
  method vstmt (s: Cil.stmt) = 
    match s.skind with
      Loop (blk,loc,_,_) when BatSet.mem (CilHelper.s_location loc) loops ->
        Cil.ChangeDoChildrenPost (Cil.mkStmt (Cil.Block blk), id)
    | _ -> Cil.DoChildren
end

let remove_loop : Cil.file -> Cil.file 
= fun f ->
  let vis = new loopRemoveVisitor (!Options.opt_unsound_loop) in
  ignore(Cil.visitCilFile vis f);
  Frontend.makeCFGinfo f
