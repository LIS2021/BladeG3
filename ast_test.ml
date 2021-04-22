(* This file contains definitions depending on functions defined in ast_stemp.ml *)

let printRho (m : value StringMap.t) = 
    let printer k v = 
        printf "%s: " k;
        match v with
            | CstI(i) -> printf "%d; " i
            | CstB(b) -> printf "%s; " (if b then "true" else "false")
            | CstA(a) -> printf "array; "
    in StringMap.iter printer m;;


let printInstr (instr : instruction) = 
    printf "%s; " (match instr with
        | Nop -> "Nop"
        | AssignE(_,_)     -> "AssignE"
        | AssignV(_,_)     -> "AssignV"
        | Load(_,_,_)      -> "Load"
        | StoreE(_,_)      -> "StorE" 
        | StoreV(_,_)      -> "StorV" 
        | IProtectE(_,_,_) -> "IProtectE"
        | IProtectV(_,_)   -> "IProtectV"
        | Guard(_,_,_,_)   -> "Guard"
        | Fail(_)          -> "Fail");;

let printCmd (c : cmd) = 
    let rec helper (c1 : cmd) = match c1 with
        | Skip             -> "Skip; "     
        | Fail             -> "Fail; "
        | VarAssign(_,_)   -> "VarAssign; "
        | PtrAssign(_,_,_) -> "PtrAssign; "
        | ArrAssign(_,_,_) -> "ArrAssign; "
        | Seq(c1,c2)       -> "Seq(" ^ (helper c1) ^ ", " ^ (helper c2) ^ "); "
        | If(_,c1,c2)      -> "If e then " ^ (helper c1) ^ " else " ^ (helper c2) ^ " fi; "
        | While(_,c)       -> "While e do " ^ (helper c) ^ " done; "
        | Protect(_,_,_)   -> "Protect; "
    in printf "%s" (helper c);;

let printObs (o : observation) = 
    printf "%s; " (match o with
        | None        -> "None"     
        | Read(_,_)   -> "Read"    
        | Write(_)    -> "Write"   
        | Fail(_)     -> "Fail"    
        | Rollback(_) -> "Rollback");;


let printOutput ((conf : configuration), (obs : observation list), (count : int)): unit =
    printf "conf : {\n";
    printf "  [" ; List.iter printInstr conf.is ; printf "]\n";
    printf "  [" ; List.iter printCmd conf.cs ; printf "]\n";
    printf "  [" ; Array.iter (printf "%d; ") conf.mu ; printf "]\n";
    printf "  {" ; printRho conf.rho ; printf "}\n";
    printf "}\n";
    printf "obs : [" ; List.iter printObs obs ; printf "]\n";
    printf "count : %d\n\n" count;;

let evalListTest conf attacker = evalList' conf attacker printOutput;;



let conf = {
    is=[]; 
    cs=[
        Seq(
            VarAssign("x", Expr(Cst(CstI(0)))), 
            VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(1)), Add)))
        )
    ]; 
    mu=[||]; 
    rho=StringMap.empty
};;

let dirs = [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];;
printOutput (conf, [], 0);;
evalListTest conf dirs;;



let a1 = { base=0 ; length=4 ; label=() };;
let b1 = { base=10 ; length=5 ; label=() };; 
let conf1 = {
  is=[]; 
  cs=[
      Seq( ArrAssign(a1, Cst(CstI(0)), Cst(CstI(1))), 
      Seq( ArrAssign(a1, Cst(CstI(1)), Cst(CstI(2))), 
      Seq( ArrAssign(b1, Cst(CstI(3)), Cst(CstI(42))), 
      Seq( VarAssign("x", ArrayRead(a1, Cst(CstI(0)))), 
      Seq( VarAssign("y", ArrayRead(a1, Cst(CstI(1)))), 
      Seq( VarAssign("z", Expr(BinOp(Var("x"), Var("y"), Add))), 
           VarAssign("w", ArrayRead(b1, Var("z"))) 
      ))))))
  ]; 
  mu= Array.make 100 0; 
  rho=StringMap.empty
};;

let dirs1 = [Fetch; Fetch; PFetch(true); Fetch; Fetch; Fetch; PFetch(true); Fetch; Fetch; Fetch; PFetch(true); Fetch; Exec 0; Retire; Exec 0; Retire; Exec 0; Retire; Exec 0; Retire; Exec 0; Retire; Exec 0; Retire];;

printOutput (conf1, [], 0);;
evalListTest conf1 dirs1;;





(* EXAMPLES *)
(* step conf Fetch [] 0;; *)
(* eval conf (fun _ -> Fetch);; *)
(* evalList conf [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];; *)


