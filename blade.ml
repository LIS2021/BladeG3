open Ast
open Graph
open Def_use_gen

module H = HashTableGen
module G = UseGraph

(** Module modeling the template 
    for the weight model to utilize
    during the construction of the graph
    to correctly assign a cost to each arc **)

module type WeightModel = sig
    (** Given a command and the current cost
        returns the cost to be assigned to the arc **)
    val cost_f : cmd -> int -> int

    (** Given a command and the current cost
        returns the new cost to be given to all 
        the commands in the same block **)
    val cost_r : cmd -> int -> int

end

(** Simple implementation where each arc 
    has the same constant cost **)
module ConstantWeight : WeightModel = struct
    
    let cost_f (c : cmd) (oc : int) : int =
      match c with
        | VarAssign _ -> oc
        | _ -> -1

    let cost_r (c : cmd) (oc : int) : int = oc

end

(** Simple implementation where commands
    inside a while loop are penalized,
    as well as assignments to a variable
    where the rhs in not a value read from 
    an array(hence not SLH-protectable) **)
module SimpleWeight : WeightModel = struct

    let cost_f (c : cmd) (oc : int) : int =
      match c with
        | VarAssign (_, rhs) -> 
          (match rhs with
            | ArrayRead _ -> oc
            | _ -> oc + 4)
        | _ -> -1

    let cost_r (c : cmd) (oc : int) : int =
      match c with
        | While(_, _) -> oc + 10
        | _ -> oc 

end

(** Simple implementation where only commands
    inside a while loop are penalized **)
module PlainProtectWeight : WeightModel = struct

    let cost_f (c : cmd) (oc : int) : int =
      match c with
        | VarAssign (_, rhs) -> oc
        | _ -> -1

    let cost_r (c : cmd) (oc : int) : int =
      match c with
        | While(_, _) -> oc + 10
        | _ -> oc 

end


(** Module modeling the template 
    of the blade program **)
module type IBlade = sig

    (** Given a weight model, a command(a program)
        and a boolean flag for spectre1.1,
        returns the program correctly protected **)
    val blade : (module WeightModel) -> bool -> cmd -> cmd

    (** Given a command and a list of identifiers to protect
        returns the command updated with the correct type of protect
        if the command was an assignment to an identifier present in the list,
        the command unaltered otherwise **)
    val protect_cmd : cmd -> (identifier * rhs) list -> cmd

end

module Blade : IBlade = struct

    let rec protect_cmd (c : cmd) (lprot : (identifier * rhs) list) : cmd =
        match c with
            | Skip -> Skip
            | Fail -> Fail
            | VarAssign(id, r) -> 
                let p = (match r with
                    | ArrayRead(_, _) -> Slh
                    | _ -> Fence) in
                (match List.find_opt (fun (x, r') -> x = id && r' = r) lprot with
                    | Some(_) -> Protect(id, p, r)
                    | None -> c)
            | PtrAssign(e1, e2, l) -> c
            | ArrAssign(a, e1, e2) -> c
            | Seq(c1, c2) -> Seq(protect_cmd c1 lprot, protect_cmd c2 lprot)
            | If(e, c1, c2) -> If(e, protect_cmd c1 lprot, protect_cmd c2 lprot) 
            | While(e, c) -> While(e, protect_cmd c lprot)
            | Protect(id, p, r) -> c

    let blade (model : (module WeightModel)) (spectre: bool) (c : cmd) : cmd = 
        let module C = (val model : WeightModel) in
        let gen = H.populate_graph c C.cost_f C.cost_r spectre in
        let g = H.get_graph gen in
        let pairs = H.get_pairs gen in
        let (g', cut) = G.edmonds_karp g in
        let lprot = G.filter_assoc pairs cut in
        protect_cmd c lprot 

end

