open BatMap
open Format
open VocabB
open PPVocab
open UserInput.Input
open Vali.Vali

let query_find pos m = try PMap.find pos m with Not_found -> []
let query_add pos (q, status) m =
  PMap.add pos ((q, status) :: query_find pos m) m

let rec partition_rec acc queries =
  match queries with
  | [] -> acc
  | ((q, pos), status) :: tl ->
    partition_rec (query_add pos (q, status) acc) tl
let partition queries = partition_rec PMap.empty queries

let is_proven _ v = List.for_all (fun (_, status) -> status = Proven) v
let print_stats m =
  let all_n = PMap.cardinal m in
  let proven_n = PMap.cardinal (PMap.filter is_proven m) in
  prerr_endline ("#all: " ^ string_of_int all_n);
  prerr_endline ("#proven: " ^ string_of_int proven_n);
  prerr_endline ("#unproven: " ^ string_of_int (all_n - proven_n))

let string_of_query = function
  | ArrayExp (lv, e) ->
    PPIL.string_of_lval true lv ^ "[" ^ PPIL.string_of_exp false e ^ "]"
  | DerefExp e -> "*" ^ PPIL.string_of_exp true e

let string_of_status = function
  | Proven -> "proven"
  | UnProven -> "unproven"
  | BotAlarm -> "bottom alarm"

let rec print_detail_1 pos v =
  match v with
  | [] -> ()
  | (q, status) :: tl ->
    let str =
      Syn.string_of_pos pos ^ ":" ^ string_of_query q ^ ":"
      ^ string_of_status status in
    prerr_endline str;
    print_detail_1 pos tl
    
let print_detail m = PMap.iter print_detail_1 m

let run : G.t * Table.t -> unit
= fun (g, inputof) ->
  let res = collect_alarm_result g inputof |> partition in
  Step.small "Query status" print_stats res;
  Step.small_side !Options.opt_print_all_query "All queries" print_detail res
