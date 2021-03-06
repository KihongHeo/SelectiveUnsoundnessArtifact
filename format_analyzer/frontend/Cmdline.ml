open Cil

module C = Cil
module F = Frontc
module E = Errormsg

let files = ref []  
let marshal_file = ref ""
 
let args f =
  if Sys.file_exists f then
    if Filename.check_suffix f ".i" ||
    Filename.check_suffix f ".c" then
      files := f :: !files
    else
      (if not !Options.opt_validate_bool then
         raise (Arg.Bad (f ^ ": Not a preprocessed C"))
       else marshal_file := f)
  else
    raise (Arg.Bad (f ^ ": No such file"))


let parseOneFile (fname: string) : C.file =
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
  if (not !Epicenter.doEpicenter) then (
    (Rmtmps.removeUnusedTemps cil)
  );
  cil

let frontend () =
  Mergecil.ignore_merge_conflicts := true;
  match List.map parseOneFile !files with
    [one] -> one
  | [] -> E.s (E.error "No arguments are given")
  | files ->
    let merged = Stats.time "merge" (Mergecil.merge files) "merged" in
      if !E.hadErrors then
        E.s (E.error "There were errors during merging");
      merged

let makeCFGinfo : Cil.file -> unit
=fun f -> 
  ignore (Partial.calls_end_basic_blocks f) ; 
  ignore (Partial.globally_unique_vids f) ; 
  Cil.iterGlobals f (fun glob -> match glob with
    Cil.GFun(fd,_) -> 
                  Cil.prepareCFG fd ;
                  (* jc: blockinggraph depends on this "true" arg *)
                  ignore (Cil.computeCFGInfo fd true)
  | _ -> ())

(* true if the given function has variable number of arguments *)
let is_varargs : string -> Cil.file -> bool
=fun fid file ->
  Cil.foldGlobals file (fun b global -> 
    match global with
    | GFun (fd,_) when fd.svar.vname = fid ->
        (match fd.svar.vtype with
        | TFun (_,_,b_va,_) -> b_va
        | _ -> b)
    | _ -> b
  ) false

let inline : string list -> Cil.file -> (FInterCfg.pid -> bool) -> bool
=fun slist f is_recursive ->
  let regexps = List.map (fun str -> Str.regexp (".*" ^ str ^ ".*")) slist in
  let to_inline = 
    VocabB.list_fold (fun global to_inline ->
      match global with
      | GFun (fd,_) when List.exists (fun regexp -> Str.string_match regexp fd.svar.vname 0) regexps -> 
        fd.svar.vname :: to_inline
      | _ -> to_inline
    ) f.globals [] in
  let varargs_procs = List.filter (fun fid -> is_varargs fid f) to_inline in
  let recursive_procs = List.filter is_recursive to_inline in
  let to_exclude = varargs_procs @ recursive_procs in
  prerr_endline ("To inline : " (*^ VocabB.string_of_list VocabB.id to_inline*));
  prerr_endline ("Excluded variable-arguments functions : " (*^ VocabB.string_of_list VocabB.id varargs_procs*));
  prerr_endline ("Excluded recursive functions : " (*^ VocabB.string_of_list VocabB.id recursive_procs*));
  Inline.toinline := List.filter (fun fid -> not (List.mem fid to_exclude)) to_inline;
  Inline.doit f;
  not (!Inline.toinline = [])
