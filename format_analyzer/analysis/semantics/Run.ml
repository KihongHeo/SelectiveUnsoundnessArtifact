open VocabA
open VocabB
open UserInputType
open UserInput.Input
open Vali.Vali

let get_cmd node g = get_some (InterCfg.get_cmd (G.icfg g) node)

let run ?(mode = Weak) ?(fs_locs = Mem.PowA.empty)
    phase node (m, g) =
  let cmd = get_cmd node g in
  run_only ((mode, phase), fs_locs) node cmd (m, g)

let accessof ?(mode = Weak) ?(fs_locs = Mem.PowA.empty) phase node (m, g) =
  let cmd = get_cmd node g in
  let (_, access) = run_access ((mode, phase), fs_locs) node cmd (m, g) in
  access

let qaccessof node (m, g) =
  let pid = InterNode.get_pid node in
  let cmd = get_cmd node g in
  let add_access (aexp, _) acc =
    let (_, access) = check_query_access pid m aexp in
    Acc.join acc access in
  list_fold add_access (collect_query cmd) Acc.bot
