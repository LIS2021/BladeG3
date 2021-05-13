(* This file contains definitions depending on functions defined in ast_stemp.ml *)
open Printf
open Ast
open Evaluator

let str_arr (a : arr) =
  sprintf "{base = %d, length = %d, label = ()}" a.base a.length;;

let str_prot (p : protect) =
  match p with
    | Slh   -> "Slh"
    | Fence -> "Fence"
    | Auto  -> "Auto";;

let str_value (v : value) =
  match v with
    | CstI(i) -> sprintf "CstI(%d)" i
    | CstB(b) -> sprintf "CstB(%b)" b
    | CstA(a) -> sprintf "CstA(%s)" (str_arr a);;

let str_op (o : op) =
  match o with
    | Add    -> "Add"
    | Lte    -> "Lte"
    | Lt     -> "Lt"
    | BitAnd -> "BitAnd";;

let str_expr (e : expr) =
  let rec helper (e : expr) = match e with
    | Cst(v)               -> sprintf "Cst(%s)" (str_value v)
    | Var(x)               -> sprintf "Var(%s)" x
    | BinOp(e1, e2, o)     -> sprintf "BinOp(%s, %s, %s)" (helper e1) (helper e2) (str_op o)
    | InlineIf(e1, e2, e3) -> sprintf "InlineIf(%s, %s, %s)" (helper e1) (helper e2) (helper e3)
    | Length(e)            -> sprintf "Length(%s)" (helper e)
    | Base(e)              -> sprintf "Base(%s)" (helper e) in
  helper e;;

let str_rhs (r : rhs) =
  match r with
    | Expr(e)         -> sprintf "Expr(%s)" (str_expr e)
    | PtrRead(e, l)   -> sprintf "PtrRead(%s, ())" (str_expr e)
    | ArrayRead(a, e) -> sprintf "ArrayRead(%s, %s)" (str_arr a) (str_expr e);;

let str_cmd (c : cmd) =
  let rec helper (c1 : cmd) = match c1 with
      | Skip               -> sprintf "Skip"
      | Fail               -> sprintf "Fail"
      | VarAssign(x,r)     -> sprintf "%s := %s" x (str_rhs r)
      | PtrAssign(e1,e2,_) -> sprintf "*(%s) := %s" (str_expr e1) (str_expr e2)
      | ArrAssign(a,e1,e2) -> sprintf "%s[%s] := %s" (str_arr a) (str_expr e1) (str_expr e2)
      | Seq(c1,c2)         -> sprintf "Seq(%s,\n\t%s)" (helper c1) (helper c2)
      | If(e,c1,c2)        -> sprintf "If %s then %s else %s fi" (str_expr e) (helper c1) (helper c2)
      | While(e,c)         -> sprintf "While %s do %s done" (str_expr e) (helper c)
      | Protect(x,p,r)     -> sprintf "%s := protect_%s(%s)" x (str_prot p) (str_rhs r) in
  helper c;;

let str_instr (instr : instruction) =
  match instr with
    | Nop              -> "Nop"
    | AssignE(x,e)     -> sprintf "AssignE(%s, %s)" x (str_expr e)
    | AssignV(x,v)     -> sprintf "AssignV(%s, %s)" x (str_value v)
    | Load(x,_,e)      -> sprintf "Load(%s, (), %s)" x (str_expr e)
    | StoreE(e1,e2)    -> sprintf "StoreE(%s, %s)" (str_expr e1) (str_expr e2)
    | StoreV(n,v)      -> sprintf "StoreV(%d, %d)" n v
    | IProtectE(x,p,e) -> sprintf "IProtectE(%s, %s, %s)" x (str_prot p) (str_expr e)
    | IProtectV(x,v)   -> sprintf "IProtectV(%s, %s)" x (str_value v)
    | Guard(e,p,cl,id) -> sprintf "Guard(%s, %B, [%s], %d)" (str_expr e) p (String.concat ", " (List.map str_cmd cl)) id
    | IFail(id)        -> sprintf "IFail(%d)" id;;

let str_observ (o : observation) =
  match o with
    | None         -> sprintf "None"
    | Read(n, ps)  -> sprintf "Read(%d, %s)" n (String.concat ", " (List.map string_of_int ps))
    | Write(n, ps) -> sprintf "Write(%d, %s)" n (String.concat ", " (List.map string_of_int ps))
    | OFail(id)    -> sprintf "OFail(%d)" id
    | Rollback(p)  -> sprintf "Rollback(%d)" p;;

let string_of_rho (m : value StringMap.t) =
  let helper k v it = sprintf "%s%s: %s; " it k (string_of_value v) in
  StringMap.fold helper m "";;

let string_of_mu (a : int array) =
  let helper it v = sprintf "%s%d; " it v in
  Array.fold_left helper "" a;;

let string_of_obs_list (obs : observation list) =
  let helper it o = sprintf "%s%s; " it (str_observ o) in
  List.fold_left helper "" obs;;

let string_of_is (is : instruction list) =
  let helper it i = sprintf "%s%s;\n\t" it (str_instr i) in
  match is with
    | [] -> ""
    | _  -> "\n\n\t" ^ (List.fold_left helper "" is) ^ "\n  ";;

let string_of_cs (cs : cmd list) =
  let helper it c = sprintf "%s%s;\n\t" it (str_cmd c) in
  match cs with
    | [] -> ""
    | _  -> "\n\n\t" ^ (List.fold_left helper "" cs) ^ "\n  ";;

let print_output (out : out_channel) ((conf : configuration), (obs : observation list), (count : int)) : unit =
  let res =
    String.make 20 '-'^
    sprintf "is:\n%s\n\n" (string_of_is conf.is)^
    sprintf "cs:\n%s\n\n"  (string_of_cs conf.cs)^
    sprintf "mu: [%s]\n\n" (string_of_mu conf.mu)^
    sprintf "rho: {%s}\n\n" (string_of_rho conf.rho)^
    sprintf "obs:\n[%s]\n\n" (string_of_obs_list obs)^
    sprintf "count: %d\n\n" count in
  Printf.fprintf out "%s" res;;

let eval_list_test conf attacker (out : out_channel) =
  print_output out (conf, [], 0);
  eval_list' conf attacker (print_output out) Evaluator.UniformCost.cost;;

let eval_list_print conf attacker =
  eval_list_test conf attacker stdout;;

let eval_list_file conf attacker =
  let oc = open_out "out" in
  let _ = eval_list_test conf attacker oc in
  close_out oc;;
