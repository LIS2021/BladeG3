open Ast
open Graph
open Def_use_gen

module H = HashTableGen
module G = UseGraph
 
module type WeightModel = sig

    val cost_f : cmd -> int -> int

    val cost_r : cmd -> int -> int

end

module ConstantWeight : WeightModel = struct
    
    let cost_f (c : cmd) (oc : int) : int =
      match c with
        | VarAssign _ -> oc
        | _ -> -1

    let cost_r (c : cmd) (oc : int) : int = oc

end

module SimpleWeight : WeightModel = struct

    let cost_f (c : cmd) (oc : int) : int =
      match c with
        | VarAssign (_, rhs) -> 
          match rhs with
            | ArrayRead _ -> 1
            | _ -> 5
        | _ -> -1

    let cost_r (c : cmd) (oc : int) : int =
      match dir, conf with
        | While(_, _) -> oc + 10
        | _ -> oc 

end


module type IBlade = sig

    val blade : cmd -> cmd

    val protect_cmd : cmd -> identifier list -> cmd

end

module Blade : IBlade = struct

    let rec protect_cmd (c : cmd) (lprot : identifier list) : cmd =
        match c with
            | Skip -> Skip
            | Fail -> Fail
            | VarAssign(id, r) -> 
                let p = (match r with
                    | ArrayRead(_, _) -> Slh
                    | _ -> Fence) in
                (match List.find_opt (fun s -> s = id) lprot with
                    | Some(_) -> Protect(id, p, r)
                    | None -> c)
            | PtrAssign(e1, e2, l) -> c
            | ArrAssign(a, e1, e2) -> c
            | Seq(c1, c2) -> Seq(protect_cmd c1 lprot, protect_cmd c2 lprot)
            | If(e, c1, c2) -> If(e, protect_cmd c1 lprot, protect_cmd c2 lprot) 
            | While(e, c) -> While(e, protect_cmd c lprot)
            | Protect(id, p, r) -> c

    let blade (model : (module WeightModel)) (c : cmd) : cmd = 
        let C = (val model : WeightModel) in
        let gen = H.populate_graph c C.cost in
        let g = H.get_graph gen in
        let pairs = H.get_pairs gen in
        let (_, cut) = G.edmonds_karp g in
        let lprot = G.filter_assoc pairs cut in
        protect_cmd c lprot 

end

