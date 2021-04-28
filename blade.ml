open Ast
open Graph
open Def_use_gen

module type IBlade = sig

    val Blade cmd -> cmd

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

    let Blade (c : cmd) : cmd = protect_cmd c []

end

