(** Main. *)

open Graph
open Cmdline
open Cil
open VocabA
open VocabB
open PPVocab
open UserInputType
open UserInput.Input
open Vali.Vali

let validate (g, fs_locs, access_map, d_io, d_oo) =
  if valid g fs_locs access_map d_io d_oo then
    prerr_endline "Validation succeeded."
  else prerr_endline "Validation failed."

let main () =

let t0 = Sys.time () in
Printexc.record_backtrace true;
Format.set_formatter_out_channel stderr;

(* process arguments *)
let usageMsg = "Usage: Main.native [options] source-files" in
Arg.parse Options.opts args usageMsg;


Step.big_side !Options.opt_marshal_bool "Marshal"
Marshaling.marshal_in !Options.opt_marshal_filename;


Cil.initCIL ();
List.fold_left (fun a e -> prerr_endline e) () !Options.opt_inline;
let one = Step.big "Front-end" frontend () in
let _ =
  if not ((List.length !Options.opt_inline) = 0) then
    Cmdline.inline !Options.opt_inline one (fun x -> false)
  else
    false
in


try
makeCFGinfo one;
if !E.hadErrors then E.s (E.error "Cabs2cil had some errors");
let icfg = Step.big "Translation to graphs" FInterCfg.init one in
let icfg = Trans.t_inter_cfg icfg in
let g = { G.icfg = icfg
        ; G.callgraph = Global.Callgraph.empty
        ; G.dump = Dump.empty } in

let (pre, g) = Step.big "Pre-analysis" Pre.analyze g in
let (dug, fs_locs, memFI, io, oo, d_io, d_oo, ord) =
  Step.big "Main Sparse Analysis" Sparse.analyze (pre, g) in
let access_map = pre.Pre.access_reach in
Step.big_side !Options.opt_validate_bool "Validation"
  validate (g, fs_locs, access_map, d_io, d_oo);
Step.big_side !Options.opt_debug "Debug mode"
  Debug.debug (g, memFI, fs_locs, access_map, dug, io, oo, d_io, d_oo);
Step.big_side true "Generate report" Report.run (g, pre, memFI, dug, io, oo, ord,one);

prerr_endline hr;
Printf.eprintf "Finished: %.1f sec\n" (Sys.time() -. t0)

with exc ->
  prerr_endline (Printexc.to_string exc);
  prerr_endline (Printexc.get_backtrace())

let _ = main ()
