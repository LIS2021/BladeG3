open Graph
open Def_use_gen

module H = HashTableGen
module G = UseGraph
 
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
                    (match List.find_opt (fun s -> s = id) lprot with
                        | Some(_) -> Protect(id, Auto, r)
                        | None -> c)
            | PtrAssign(e1, e2, l) -> c
            | ArrAssign(a, e1, e2) -> c
            | Seq(c1, c2) -> Seq(protect_cmd c1 lprot, protect_cmd c2 lprot)
            | If(e, c1, c2) -> If(e, protect_cmd c1 lprot, protect_cmd c2 lprot) 
            | While(e, c) -> While(e, protect_cmd c lprot)
            | Protect(id, p, r) -> c

    let blade (c : cmd) : cmd = 
        let gen = H.new_gen () in
        let gen = H.populate_graph gen c 1 in
        let g = H.get_graph gen in
        let (_, cut) = G.edmonds_karp g in
        let pairs = H.get_pairs gen in
        let lprot = G.filter_assoc pairs cut in
        protect_cmd c lprot 

end

