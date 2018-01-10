(** Abstract Domain *)

open Vocab
open InterCfg

module Node = InterCfg.Node
module Proc = InterCfg.Proc
module PowProc = PowDom.Make (Proc)

module ExtAllocsite =
struct
  type t = Input | Unknown of string
  let compare = compare
  let input = Input
  let unknown s = Unknown s
  let is_cmd_arg x = 
    match x with 
      Unknown s -> s = "arg"
    | _ -> false
  let to_string = function
    | Input -> "__extern__"
    | Unknown s -> "__extern__" ^ s
end

module IntAllocsite = 
struct 
  type t = Node.t * position * is_string
  and position = int  (* position in the node. e.g. Cseq [...] *)
  and is_string = bool   
  
  let compare = compare
  let to_string (node,i,_) = Node.to_string node ^ string_of_int i
end

module Allocsite = 
struct 
  type t = Internal of IntAllocsite.t | External of ExtAllocsite.t
  let compare = compare
  let allocsite_of_node : Node.t -> int -> t 
  = fun n p -> Internal (n,p,false)
  let allocsite_of_string : Node.t -> int -> t 
  = fun n p -> Internal (n,p,true)

  let is_node_allocsite = function Internal (_,_,false) -> true | _ -> false
  let is_string_allocsite = function Internal (_,_,true) -> true | _ -> false
  let is_global_allocsite = function Internal (node,_,_) -> (Node.get_pid node = "_G_") | _ -> false
  let is_ext_allocsite = function External _ -> true | _ -> false
  let is_cmd_arg = function External e -> ExtAllocsite.is_cmd_arg e | _ -> false

  let allocsite_of_ext : string option -> t 
  = function None -> External (ExtAllocsite.input)
  | Some fid -> External (ExtAllocsite.unknown fid)

  let to_string 
  = function Internal i -> IntAllocsite.to_string i
  | External e -> ExtAllocsite.to_string e
end

module Loc = 
struct 
  type t = GVar of string | LVar of Proc.t * string | Allocsite of Allocsite.t 
  | Field of Allocsite.t * field
  and field = string 

  let compare = compare

  let to_string = function 
    | GVar g -> g
    | LVar (p,x) -> "(" ^ Proc.to_string p ^ "," ^ x ^ ")"
    | Allocsite a -> Allocsite.to_string a
    | Field (a, f) -> Allocsite.to_string a ^ "." ^ f

  let dummy = GVar "__dummy__"
  let null = GVar "NULL"

  let is_null x = (x = null)
  let is_var : t -> bool = function 
    | GVar _ | LVar _ -> true
    | _ -> false
   
  let is_gvar : t -> bool = function
    | GVar _ -> true
    | _ -> false

  let is_lvar : t -> bool = function
    | LVar _ -> true
    | _ -> false 

  let is_allocsite : t -> bool = function
    | Allocsite _ -> true
    | _ -> false

  let is_string_allocsite : t -> bool = function
    | Allocsite a -> Allocsite.is_string_allocsite a
    | _ -> false
  
  let is_ext_allocsite : t -> bool = function 
    | Allocsite a -> Allocsite.is_ext_allocsite a
    | _ -> false  

  let is_field : t -> bool = function
    | Field (_,_) -> true
    | _ -> false

  let is_local_of : Proc.t -> t -> bool = fun p x ->
    match x with 
    | LVar (p',_) -> p = p'
    | _ -> false

  let get_proc : t -> Proc.t 
  = function LVar (p, _) -> p | _ -> raise Not_found

  let of_gvar x = GVar x
  let of_lvar (p,x) = LVar (p,x) 
  let of_allocsite : Allocsite.t -> t = fun x -> Allocsite x 

  let append_field x f = 
    match x with 
      Allocsite a -> Field (a,f)
    | _ -> x
end

module PowLoc = 
struct 
  include PowDom.Make (Loc)

  let prune op x e = 
    match op with 
      Cil.Eq when Cil.isZero e -> singleton Loc.null
    | Cil.Ne when Cil.isZero e -> remove Loc.null x
    | _ -> x
  let null = singleton Loc.null
  let append_field : t -> string -> t = fun ls f ->
    let add_appended l acc = 
      if Loc.is_ext_allocsite l then add l acc
      else if Loc.is_null l || Loc.is_string_allocsite l then acc 
      else add (Loc.append_field l f) acc
    in
    fold add_appended ls bot
end

module Dump = MapDom.Make (Proc) (PowLoc)
