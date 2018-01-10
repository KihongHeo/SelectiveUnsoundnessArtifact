open Cil
open Vocab

class extractUpdateVisitor () = object(self)
  inherit nopCilVisitor
  method vinst (i: Cil.instr) = 
    match i with
    | Set ((Var _, NoOffset), _, _) -> Cil.DoChildren
    | Set (lval, _, location) -> 
      print_endline (CilHelper.s_location location ^ ":" ^ CilHelper.s_lv lval);
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

let extract_update f =
  let vis = new extractUpdateVisitor () in
  ignore (Cil.visitCilFile vis f);
  ()
