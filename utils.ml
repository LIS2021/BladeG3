(* This file contains definitions depending on functions defined in ast_stemp.ml *)
open Printf
open Ast
open Evaluator

let strArr (a : arr) =
    sprintf "{base = %d, length = %d, label = ()}" a.base a.length;;

let strProt (p : protect) =
    match p with
        | Slh   -> "Slh"
        | Fence -> "Fence"
        | Auto  -> "Auto";;

let strValue (v : value) =
    match v with
        | CstI(i) -> sprintf "CstI(%d)" i
        | CstB(b) -> sprintf "CstB(%b)" b
        | CstA(a) -> sprintf "CstA(%s)" (strArr a);;

let strOp (o : op) =
    match o with
        | Add    -> "Add"
        | Lte    -> "Lte"
        | Lt     -> "Lt"
        | BitAnd -> "BitAnd";;

let strExpr (e : expr) =
    let rec helper (e : expr) = match e with
        | Cst(v)               -> sprintf "Cst(%s)" (strValue v)
        | Var(x)               -> sprintf "Var(%s)" x
        | BinOp(e1, e2, o)     -> sprintf "BinOp(%s, %s, %s)" (helper e1) (helper e2) (strOp o)
        | InlineIf(e1, e2, e3) -> sprintf "InlineIf(%s, %s, %s)" (helper e1) (helper e2) (helper e3)
        | Length(e)            -> sprintf "Length(%s)" (helper e)
        | Base(e)              -> sprintf "Base(%s)" (helper e)
    in helper e;;

let strRhs (r : rhs) =
    match r with
        | Expr(e) -> sprintf "Expr(%s)" (strExpr e)
        | PtrRead(e, l) -> sprintf "PtrRead(%s, ())" (strExpr e)
        | ArrayRead(a, e) -> sprintf "ArrayRead(%s, %s)" (strArr a) (strExpr e);;

let strCmd (c : cmd) =
    let rec helper (c1 : cmd) = match c1 with
        | Skip               -> sprintf "Skip"
        | Fail               -> sprintf "Fail"
        | VarAssign(x,r)     -> sprintf "%s := %s" x (strRhs r)
        | PtrAssign(e1,e2,_) -> sprintf "*(%s) := %s" (strExpr e1) (strExpr e2)
        | ArrAssign(a,e1,e2) -> sprintf "%s[%s] := %s" (strArr a) (strExpr e1) (strExpr e2)
        | Seq(c1,c2)         -> sprintf "Seq(%s,\n\t%s)" (helper c1) (helper c2)
        | If(e,c1,c2)        -> sprintf "If %s then %s else %s fi" (strExpr e) (helper c1) (helper c2)
        | While(e,c)         -> sprintf "While %s do %s done" (strExpr e) (helper c)
        | Protect(x,p,r)     -> sprintf "%s := protect_%s(%s)" x (strProt p) (strRhs r)
    in helper c;;

let strInstr (instr : instruction) =
    match instr with
        | Nop              -> "Nop"
        | AssignE(x,e)     -> sprintf "AssignE(%s, %s)" x (strExpr e)
        | AssignV(x,v)     -> sprintf "AssignV(%s, %s)" x (strValue v)
        | Load(x,_,e)      -> sprintf "Load(%s, (), %s)" x (strExpr e)
        | StoreE(e1,e2)    -> sprintf "StoreE(%s, %s)" (strExpr e1) (strExpr e2)
        | StoreV(n,v)      -> sprintf "StoreV(%d, %d)" n v
        | IProtectE(x,p,e) -> sprintf "IProtectE(%s, %s, %s)" x (strProt p) (strExpr e)
        | IProtectV(x,v)   -> sprintf "IProtectV(%s, %s)" x (strValue v)
        | Guard(e,p,cl,id) -> sprintf "Guard(%s, %B, [%s], %d)" (strExpr e) p (String.concat ", " (List.map strCmd cl)) id
        | IFail(id)        -> sprintf "IFail(%d)" id;;

let strObserv (o : observation) =
    match o with
        | None        -> sprintf "None"
        | Read(n, ps)   -> sprintf "Read(%d, %s)" n (String.concat ", " (List.map string_of_int ps))
        | Write(n, ps) -> sprintf "Write(%d, %s)" n (String.concat ", " (List.map string_of_int ps))
        | OFail(id)    -> sprintf "OFail(%d)" id
        | Rollback(p) -> sprintf "Rollback(%d)" p;;

let strRho (m : value StringMap.t) =
    let helper k v it =
        sprintf "%s%s: %s; " it k (strValue v)
    in StringMap.fold helper m "";;

let strMu (a : int array) =
    let helper it v =
        sprintf "%s%d; " it v
    in Array.fold_left helper "" a;;

let strObs (obs : observation list) =
    let helper it o =
        sprintf "%s%s; " it (strObserv o)
    in List.fold_left helper "" obs;;

let strIs (is : instruction list) =
    let helper it i =
        sprintf "%s%s;\n\t" it (strInstr i)
    in match is with
        | [] -> ""
        | _  -> "\n\n\t" ^ (List.fold_left helper "" is) ^ "\n  ";;

let strCs (cs : cmd list) =
    let helper it c =
        sprintf "%s%s;\n\t" it (strCmd c)
    in match cs with
        | [] -> ""
        | _  -> "\n\n\t" ^ (List.fold_left helper "" cs) ^ "\n  ";;

let printOutput (out : out_channel) ((conf : configuration), (obs : observation list), (count : int)): unit =
    let res =
        sprintf "conf : {\n"^
        sprintf "  is:  [%s]\n" (strIs conf.is)^
        sprintf "  cs:  [%s]\n" (strCs conf.cs)^
        sprintf "  mu:  [%s]\n" (strMu conf.mu)^
        sprintf "  rho: {%s}\n" (strRho conf.rho)^
        sprintf "}\n"^
        sprintf "obs : [%s]\n" (strObs obs)^
        sprintf "count : %d\n\n" count
    in Printf.fprintf out "%s"res


let evalListTest conf attacker (out : out_channel) =
    printOutput out (conf, [], 0);
    evalList' conf attacker (printOutput out) Evaluator.UniformCost.cost;;

let evalListPrint conf attacker =
    evalListTest conf attacker stdout;;

let evalListFile conf attacker =
    let oc = open_out "out" in
    let _ = evalListTest conf attacker oc in
    close_out oc;;

