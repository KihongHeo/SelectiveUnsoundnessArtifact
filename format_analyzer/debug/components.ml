(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: components.ml,v 1.9 2004-10-22 14:42:06 signoles Exp $ *)
open Graph
open Graph.Util

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Make(G: G) = struct
 
  type comp_vertex = C of int | V of int
  module H = Hashtbl.Make(G.V)
  module S = Set.Make(G.V)
	module CV = Set.Make(struct type t = comp_vertex let compare = Pervasives.compare end)

	let scc g =
    let root = H.create 997 in
    let hashcomp = H.create 997 in
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    let rec pop x c = function
      | (y, w) :: l when y > x -> 
	  H.add hashcomp w !numcomp;
	  pop x (S.add w c) l
      | l -> c,l
    in
    let rec visit v = 
      if not (H.mem root v) then
			begin
			  let n = incr numdfs; !numdfs in
			  H.add root v n; 
			  G.iter_succ 
			    (fun w -> 
			       visit w;
			       if not (H.mem hashcomp w) then 
				 				H.replace root v (min (H.find root v) (H.find root w))
					)
			    g v;
			  if H.find root v = n then 
			    (H.add hashcomp v !numcomp;
			     let comp,s = pop n (S.add v S.empty) !stack in
					 ( (* if (S.equal (S.add v S.empty) comp) then H.remove hashcomp v else *) incr numcomp); 
			     stack := s
			     )
			  else stack := (n,v)::!stack;
			end
    in 
    G.iter_vertex visit g;
    (!numcomp,(fun v -> H.find hashcomp v))
	
(***************** SIMPLE_TC *******************)
	
	let transitive_closure g =
    let root = H.create 997 in
    let hashcomp = H.create 997 in
		let succ = H.create 997 in  (* G.V -> G.V list *)
		
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    
		let rec pop x c = 
			function
      | (y, w) :: l when y > x -> 
	  		H.add hashcomp w !numcomp; 
	  		pop x (S.add w c) l
      | l -> c,l
    in
		let find hashtbl v default = try H.find hashtbl v with _ -> default in   
    let rec visit v = 
      if not (H.mem root v) then
			begin
			  let n = incr numdfs; !numdfs in
			  H.add root v n;
				G.iter_succ (fun w -> H.replace succ v (w::(find succ v []))) g v;    
			  G.iter_succ 
			    (fun w -> 
			       visit w;
			       (if not (H.mem hashcomp w) then 
				 		   H.replace root v (min (H.find root v) (H.find root w))
						 )
						;
						 H.replace succ v ((find succ v [])@(find succ w []))
					)
			    g v;
			  if H.find root v = n then 
			    (incr numcomp;
					 H.add hashcomp v !numcomp;
			     let comp,s = pop n (S.add v S.empty) !stack in 
			     stack:= s;
			     )
			  else stack := (n,v)::!stack;
			end
    in 
		G.iter_vertex visit g;
		(H.fold (fun v succs l -> (v,succs) :: l) succ [])

(***************** CC_TC *******************)
	
	let transitive_closure_cc g int2vertex is_forward_edge =
		let vertex_num = ref 0 in
		G.iter_vertex (fun v -> incr vertex_num) g;
		let init_num = !vertex_num * 2 in 
		
    let root = H.create init_num in   (* G.V -> int *)
    let hashcomp = H.create init_num in   (* G.V -> int *)
		let succ = Hashtbl.create init_num in  (* comp_vertex -> comp_vertex list *)
		
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    
		let rec pop x c = 
			function
      | (y, w) :: l when y > x -> 
	  		H.add hashcomp w !numcomp;
	  		pop x (S.add w c) l
      | l -> c,l
    in
		let find hashtbl v = try Hashtbl.find hashtbl v with _ -> CV.empty in   
    let rec visit v = 
      if not (H.mem root v) then
			begin
			  let n = incr numdfs; !numdfs in
(*				let _ = prerr_endline ("n : "^(string_of_int n)^"\t stack : "^(string_of_int (List.length !stack))^"\t succ : "^(string_of_int (Hashtbl.length succ))) in *)
			  H.add root v n;
				stack := (n,v)::!stack;
			  G.iter_succ 
			    (fun w -> 
			       (if not (H.mem root w) then visit w);
			       if not (H.mem hashcomp w) then 
				 		   H.add root v (min (H.find root v) (H.find root w))
						(** line 8 *)
						 else
						 begin 
							 if (not (is_forward_edge v w)) && (H.mem hashcomp w) then
							 let c_w = C (H.find hashcomp w) in
							 let succ_root_v = find succ (V (H.find root v)) in 
							 if (not (CV.mem c_w succ_root_v)) then
							(** line 9 *)
								let succ_root_v = CV.add c_w (CV.union succ_root_v (find succ c_w)) in 
(*								let _ = prerr_endline ("succ_root_v : "^(string_of_int (CV.cardinal succ_root_v))) in*)
								Hashtbl.add succ (V (H.find root v)) succ_root_v
						 end
					)
			    g v;
			  if H.find root v = n then 
			    (
					 let succ_root_v = find succ (V (H.find root v))  in						
					 ( 
						 (** line 13 *)
					   if (fst (List.hd !stack)) = n then Hashtbl.add succ (C !numcomp) succ_root_v
						 (** line 14 *)
					   else
					 	   Hashtbl.add succ (C !numcomp) (CV.add (C !numcomp) succ_root_v)
					 );
					 H.add hashcomp v !numcomp;
			     let comp,s = pop n (S.add v S.empty) !stack in
					 S.iter 
							(fun w ->
								(** line 18 *) 
								if (Pervasives.compare w v) <> 0 then 
									Hashtbl.add succ (C !numcomp) (CV.union (find succ (C !numcomp) ) (find succ (V (H.find root w)) ))
							) comp;  
			     stack:= s;
					 incr numcomp
			     )
			end
    in 
		let t0 = Sys.time() in 
		G.iter_vertex visit g;
		prerr_endline ("Computing transitive closure completes : "^(string_of_float ((Sys.time()) -. t0)));
		 
		let t0 = Sys.time() in
		
		let rroot = Hashtbl.create init_num in   (* int -> G.V *)
		let _ = H.iter (fun v n -> Hashtbl.add rroot n v) root in
(*		let comps = Hashtbl.create 997 in   (* int -> G.V list ref *)*)
(*		let _ =                                                      *)
(*			H.iter                                                     *)
(*				(fun v n ->                                              *)
(*					let l = try Hashtbl.find comps n with _ -> ref [] in   *)
(*					l := v :: !l                                           *)
(*					Hashtbl.add comps n l)                                 *)
(*				hashcomp                                                 *)
(*		in                                                           *)

    let trans cv =
			match cv with 
			| V n -> (try Hashtbl.find rroot n with _ -> prerr_endline ("V : "^(string_of_int n)); (List.hd []))
			| C n -> (int2vertex n)
		in
		let result = Hashtbl.create init_num in  
		
		(Hashtbl.iter 
			(fun cv cvset ->
				let v = trans cv in 
				let vlst = List.map trans (CV.elements cvset) in   
				Hashtbl.add result v vlst   
			)
			succ
		);        
		
		prerr_endline ("Constructing pair list : "^(string_of_float ((Sys.time()) -. t0)));
		result
		
(*		(H.fold (fun v succs l -> (v,succs) :: l) succ [])*)


  let scc_array g =
    let n,f = scc g in
    let t = Array.make n [] in
    G.iter_vertex 
      (fun v -> let i = f v in t.(i) <- v::t.(i)) g;
    t

  let scc_list g =
    let _,scc = scc g in
    let tbl = Hashtbl.create 97 in
    G.iter_vertex 
      (fun v -> 
	 let n = scc v in
	 try
	   let l = Hashtbl.find tbl n in
	   l := v :: !l
	 with Not_found ->
	   Hashtbl.add tbl n (ref [ v ]))
      g;
    Hashtbl.fold (fun _ v l -> !v :: l) tbl []


end
