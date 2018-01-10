open Batteries
let counter = ref 0    
let filter_time = ref 0.0
let add_filter_time t =
  let t' = Sys.time () in
  filter_time := !filter_time +. (t' -. t)
                         
type prop =
  | True
  | False
  | Var of string

let pp_prop = function
  | True -> ""
  | False -> ""
  | Var x -> x

type formula =
  | Prop of prop
  | Or of formula * formula
  | And of formula * formula
  | Neg of formula

let rec p_formula f =
  match f with
  | Prop p ->
    (
      match p with
      | True -> "T"
      | False -> "F"
      | Var x -> x
    )
  | Or (f1, f2) ->
    "(" ^ p_formula f1 ^ " OR "  ^ p_formula f2 ^ ")"
  | And (f1, f2) ->
    "(" ^ p_formula f1 ^ " AND "  ^ p_formula f2 ^ ")"
  | Neg (f1) ->
    "NOT(" ^ p_formula f1 ^ ")"

module CNF = struct
  type literal =
    | Pos of prop
    | Neg of prop

  let lit_compare x y =
    match x, y with
    | Pos (Var a), Pos (Var b) ->
      compare a b
    | Pos _, Neg _ -> 1
    | Neg _, Pos _ -> -1
    | _, _ ->
      compare x y 

  module LiteralSet =
    BatSet.Make (struct type t = literal let compare = lit_compare end)
      
  (* module LiteralSet =
     Set.Make (struct type t = literal let compare = lit_compare end)*)
      
  type clause = LiteralSet.t

  let clause_ston = LiteralSet.singleton
  let clause_or = LiteralSet.union
  
  module ClauseSet = BatSet.Make (LiteralSet)

  type cnf = ClauseSet.t
  let cnf_ston = ClauseSet.singleton
  let cnf_and = ClauseSet.union

  let buf = BatBuffer.create 20000000
  let buf_size = 19900000
  let current_count = ref 0

  let buffer_flush () =
    Format.print_string (BatBuffer.contents buf);
    BatBuffer.clear buf;
    current_count := 0
      
  let write_buf str =
    let strlen = String.length str in
    (
      if !current_count + strlen < buf_size then
        (
          current_count := !current_count + strlen;
        )
      else
        (
          buffer_flush ();
          current_count := strlen;
        )
    );
    BatBuffer.add_string buf str
  
  let cnf_map f s =
    let iter1 e acc = ClauseSet.add (f e) acc in
    ClauseSet.fold iter1 s ClauseSet.empty
      
  let cnf_fold = ClauseSet.fold
  let cnf_empty = ClauseSet.empty

  let or_dist_sub c cs : cnf = cnf_map (clause_or c) cs

  let or_dist x y : cnf =
    let iter1 c acc = cnf_and (or_dist_sub c x) acc in
    cnf_fold iter1 y cnf_empty

  let p_space () = write_buf " "
  let p_linefeed () = write_buf "\n"
  
  let pp_infix_set infix pp_elem set_fold s =
    let iter1 x acc =
      if acc = "" then pp_elem x
      else acc ^ infix ^ pp_elem x
    in
    set_fold iter1 s ""

  (* TODO: use stream if input formula is big *)
  let is_bool_prop = function
    | Pos x
    | Neg x ->
      if x = True then true else
      if x = False then true else
        false
          
  let pp_literal lit =
    match lit with
    | Pos (Var x) -> x
    | Neg (Var x) -> write_buf "-"; x
    | _ -> ""
          
  let literal_is_true = function
    | Pos True
    | Neg False -> true
    | _ -> false

  let literal_is_not_false = function
    | Pos False
    | Neg True -> false
    | _ -> true

  let clause_has_true x =
    LiteralSet.mem (Pos True) x || LiteralSet.mem (Neg False) x

  
  let clause_remove_false x = LiteralSet.filter literal_is_not_false x

  let p_clause x =
    let fold_lit lit =
      if literal_is_not_false lit then
        (write_buf (pp_literal lit);
         write_buf " ")
      else ()
    in
    let _ = LiteralSet.iter fold_lit x in
    write_buf "0\n"
      
  let p_wcnf_clause buf chan clause_cnt x =
    let b = clause_has_true x in
    if b then () else
      (
        write_buf (string_of_int clause_cnt);
        write_buf " ";
        p_clause x
      )
      
  (* let pp_cnf x = pp_infix_set "\n" pp_clause ClauseSet.fold x *)
      
  (* let p_cnf x = pp_infix_set "\n" p_clause ClauseSet.fold x *)
  let p_wcnf_header var_cnt clause_cnt top =
    write_buf "p wcnf ";
    write_buf ((string_of_int var_cnt) ^ " ");
    write_buf ((string_of_int clause_cnt) ^ " ");
    write_buf ((string_of_int top) ^ "\n")
      
  let p_wcnf var_cnt hard_constraint soft_constraint filename =
    let chan = open_out filename in
    Format.set_formatter_out_channel chan;
    
    let hard_cnt = ClauseSet.cardinal hard_constraint in
    let soft_cnt = ClauseSet.cardinal soft_constraint in
    let clause_cnt = hard_cnt + soft_cnt in
    let top = clause_cnt in
    p_wcnf_header var_cnt clause_cnt top;
    ClauseSet.iter ((p_wcnf_clause buf chan) top) hard_constraint;
    p_linefeed ();
    ClauseSet.iter ((p_wcnf_clause buf chan) 1) soft_constraint;
    (* Flush buffer *)
    buffer_flush ();
    close_out chan;
    Format.set_formatter_out_channel stderr;
    
end

let rec neg_apply' x cont =
  match x with
  | Prop _ -> cont (Neg x)
  | Or (a, b) ->
    neg_apply' a (fun a' -> neg_apply' b (fun b' -> cont (And (a', b'))))
  | And (a, b) ->
    neg_apply' a (fun a' -> neg_apply' b (fun b' -> cont (Or (a', b'))))
  | Neg a -> cont a

let rec neg_apply_r' x  =
  match x with
  | Prop _ -> (Neg x)
  | Or (a, b) ->
    let x = neg_apply_r' a in
    let y = neg_apply_r' b in
    And (x, y)
  | And (a, b) ->
    let x = neg_apply_r' a in
    let y = neg_apply_r' b in
    (Or (x, y))
  | Neg a -> a


let neg_apply x = neg_apply_r' x (* (fun x -> x) *)

let rec cnf_conv' f cont =
  match f with
  | Prop x -> cont (CNF.cnf_ston (CNF.clause_ston (CNF.Pos x)))
  | Or (x, y) ->
    cnf_conv' x (fun x' -> cnf_conv' y (fun y' -> cont (CNF.or_dist x' y')))
  | And (x, y) ->
    cnf_conv' x (fun x' -> cnf_conv' y (fun y' -> cont (CNF.cnf_and x' y')))
  | Neg (Prop x) -> cont (CNF.cnf_ston (CNF.clause_ston (CNF.Neg x)))
  | Neg (Neg x) -> cnf_conv' x cont
  | Neg (Or (x, y)) -> cnf_conv' (And (neg_apply x, neg_apply y)) cont
  | Neg (And (x, y)) -> cnf_conv' (Or (neg_apply x, neg_apply y)) cont

let rec cnf_conv_n' f =
  match f with
  | Prop x -> (CNF.cnf_ston (CNF.clause_ston (CNF.Pos x)))
  | Or (x, y) ->
    let a = cnf_conv_n' x in
    let b = cnf_conv_n' y in
    CNF.or_dist a b
  | And (x, y) ->
    let a = cnf_conv_n' x in
    let b = cnf_conv_n' y in
    CNF.cnf_and a b
  | Neg (Prop x) -> (CNF.cnf_ston (CNF.clause_ston (CNF.Neg x)))
  | Neg (Neg x) -> cnf_conv_n' x
  | Neg (Or (x, y)) -> cnf_conv_n' (And (neg_apply x, neg_apply y))
  | Neg (And (x, y)) -> cnf_conv_n' (Or (neg_apply x, neg_apply y))



let cnf_conv f = cnf_conv_n' f (* (fun x -> x) *)

let rec ground_facts cnf =
  let facts = 
    CNF.ClauseSet.fold (fun clause acc ->
  	if (CNF.LiteralSet.cardinal clause) = 1 then 
  	  (CNF.LiteralSet.choose clause) :: acc
  	else acc
      ) cnf []
  in
  if (List.length facts) = 0 then [] 
  else
    let cnf' = 
      CNF.ClauseSet.fold (fun clause set ->
  	  let result = 
  	    List.fold_left (fun clause_acc literal ->
  		let clause_acc = 
  		  if (CNF.LiteralSet.mem literal clause_acc) then 
  		    CNF.LiteralSet.remove literal clause_acc
  		  else clause_acc
  		in
  		let neg_literal = 
                  match literal with
                  | CNF.Pos x -> CNF.Neg x
                  | CNF.Neg x -> CNF.Pos x
                in 
                if (CNF.LiteralSet.mem neg_literal clause_acc) then 
  		  CNF.LiteralSet.empty
  		else clause_acc
  	      ) clause facts  
  	  in CNF.ClauseSet.add result set 
  	) cnf CNF.ClauseSet.empty
    in 
    facts @ (ground_facts cnf')

(* TEST *)

let mk_imply f1 f2 = Or (Neg f1, f2)
let mk_var i = Prop (Var i)
let mk_false = Prop False
let mk_true = Prop True
(* let conv_test t = *)
(*   print_endline (CNF.pp_cnf (cnf_conv t)) *)

(* let t1 = Prop (Var 3) *)
(* let t2 = Neg (Prop (Var 1)) *)
(* let t3 = mk_imply t1 t2 *)
(* let t4 = Prop (Var 5) *)
(* let t5 = *)
(*   Or (And (Neg (And (Prop (Var 1), Prop (Var 2))), Prop (Var 3)), *)
(*       And (Prop (Var 4), Prop (Var 5))) *)

(* let print_formula hard_cons soft_cons var_cnt =*)
  (* print_endline (CNF.p_wcnf var_cnt hard_cons soft_cons) *)
(* let _ = conv_test t5 *)
