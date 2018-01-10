open Arg

(* IL *)
let opt_il = ref false
let opt_cfg = ref false
let opt_dug = ref false
let opt_optil = ref false
let opt_bb = ref false

(* Context & Flow Sensitivity *)
let opt_inline = ref ["alloc"; "fatal"]

(* Octagon Analysis *)
let opt_oct = ref false
let opt_pack_impact = ref true
let opt_pack_manual = ref false

(* Analyzer *)
let opt_nobar = ref false
let opt_narrow = ref false
let opt_profile = ref false
let opt_scaffold = ref false

(* Unsoundness *)
let opt_unsound_loop = ref BatSet.empty
let opt_unsound_lib = ref BatSet.empty
let opt_unsound_update_all = ref true
let opt_unsound_global = ref BatSet.empty
let opt_bugfinder = ref 2

(* Alarm Report *)
let opt_noalarm = ref false
let opt_bo = ref true
let opt_nd = ref false
let opt_dz = ref false
let opt_show_all_query = ref false


(* Marshaling *)
let opt_marshal_in = ref false
let opt_marshal_out = ref false
let opt_marshal_dir = ref "marshal"

(* Debug *)
let opt_debug = ref false
let opt_oct_debug = ref false

(* ETC *)
let opt_print_premem = ref false
let opt_verbose = ref 1
let opt_magic = ref false

let opts =
  [
  ("-il", (Arg.Set opt_il), "Show the input program in IL");
  ("-cfg", (Arg.Set opt_cfg), "Print Cfg");
  ("-dug", (Arg.Set opt_dug), "Print Def-Use graph");
  ("-noalarm", (Arg.Set opt_noalarm), "Do not print alarms");
  ("-verbose", (Arg.Int (fun x -> opt_verbose := x)), "Verbose level (default: 1)");
  ("-debug", (Arg.Set opt_debug), "Print debug information");
  ("-oct_debug", (Arg.Set opt_oct_debug), "Print debug information for octagon analysis");
  ("-pack_impact", (Arg.Set opt_pack_impact), "Packing by impact pre-analysis");
  ("-pack_manual", (Arg.Set opt_pack_manual), "Pacing by manual annotation");
  ("-nd", (Arg.Set opt_nd), "Print Null-dereference alarms");
  ("-bo", (Arg.Set opt_bo), "Print Buffer-overrun alarms");
  ("-dz", (Arg.Set opt_dz), "Print Divide-by-zero alarms");
  ("-bugfinder", (Arg.Int (fun x -> opt_bugfinder := x)), "Unsoundness level in bugfinding mode (default: 0)");
  ("-inline", (Arg.String (fun s -> opt_inline := s::(!opt_inline))), "Inline *alloc* functions");
  ("-oct", (Arg.Set opt_oct), "Do octagon analysis");
  ("-profile", (Arg.Set opt_profile), "Profiler");
  ("-narrow", (Arg.Set opt_narrow), "Do narrowing");
  ("-unsound_loop", (Arg.String (fun s -> opt_unsound_loop := BatSet.add s !opt_unsound_loop)), "Unsound loops");
  ("-unsound_lib", (Arg.String (fun s -> opt_unsound_lib := BatSet.add s !opt_unsound_lib)), "Unsound libs");
  ("-unsound_global", (Arg.String (fun s -> opt_unsound_global := BatSet.add s !opt_unsound_global)), "Unsound libs");
  ("-unsound_update_all", (Arg.Set opt_unsound_update_all), "Unsound update");
  ("-scaffold", (Arg.Set opt_scaffold), "Use scaffolding semantics");
  ("-nobar", (Arg.Set opt_nobar), "No progress bar");
  ("-show_all_query", (Arg.Set opt_show_all_query), "Show all queries");
  ("-optil", (Arg.Set opt_optil), "Optimize IL");
  ("-bb", (Arg.Set opt_bb), "Basic Block");
  ("-marshal_in", (Arg.Set opt_marshal_in), "Read analysis results from marshaled data");
  ("-marshal_out", (Arg.Set opt_marshal_out), "Write analysis results to marshaled data");
  ("-marshal_dir", (Arg.String (fun s -> opt_marshal_dir := s)), "Directory where the marshaled data exists (default: marshal/)");
  ("-magic", (Arg.Set opt_magic), "special");
  ]
