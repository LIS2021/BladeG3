open Format

module StringMap = Map.Make(String)

type identifier = string;;

type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

type label = unit;;

type arr = {base : int; length : int; label : label};;

type value =
  | CstI of int
  | CstB of bool
  | CstA of arr;;

type expr =
  | Cst of value
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

type protect = Slh | Fence | Auto;;

type cmd =
  | Skip
  | Fail
  | VarAssign of identifier * rhs
  | PtrAssign of expr * expr * label
  | ArrAssign of arr * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Protect of identifier * protect * rhs;;

(** 		DIRECTIVES 		**)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

type guard_fail_id = int;;

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | Fail of guard_fail_id
  | Rollback of int

(**		INSTRUCTION SET		**)
type instruction =
  | Nop
  | AssignE of identifier * expr
  | AssignV of identifier * value
  | Load of identifier * label * expr 			(* 	id := load(e) 		*)
  | StoreE of expr * expr
  | StoreV of int * int
  | IProtectE of identifier * protect * expr 	(* 	id := protect(e) 	*)
  | IProtectV of identifier * value 	(* 	id := protect(v) 	*)
  | Guard of expr * prediction * cmd list * guard_fail_id
  | Fail of guard_fail_id ;;

(**		CONFIGURATIONS 		**)
type configuration = {
	is : instruction list ;
	cs : cmd list ;
	mu : int array ;
	rho : value StringMap.t ;
};;

type decl_type =
  | TypI
  | TypA of int * int
  | TypP;;

let splitIs (ls : 'a list) (n : int) =
    let rec split_is_rec (fls : 'a list) (ls : 'a list) (n : int) =
        (match ls, n with
            | a :: cls, n -> if n == 0 then
                                    (fls, a, cls)
                                else if n > 0 then
                                    split_is_rec (fls @ [a]) cls (n - 1)
                                else
                                    failwith "invalid directive"
            | [], n       ->  failwith "invalid directive") in
        split_is_rec [] ls n;;

let isStore (i : instruction) : bool =
  match i with
      | StoreE(_, _) -> true
      | StoreV(_, _) -> true
      | _ -> false;;

let isGuard (i : instruction) : bool =
  match i with
      | Guard(_, _, _, _) -> true
      | _ -> false;;

let freshName (name : string) (rho : value StringMap.t) : string =
  let rec helper (name : string) (rho : value StringMap.t) (counter : int) =
    if StringMap.mem (name ^ (string_of_int counter)) rho then
      helper name rho (counter + 1)
    else
      name ^ (string_of_int counter)
   in helper name rho 0


let rec phi (rho : value StringMap.t) (is : instruction list) : value StringMap.t =
  match is with
      | [] -> rho
      | AssignV(x, v) :: is -> phi (StringMap.add x v rho) is
      | AssignE(x, e) :: is -> phi (StringMap.remove x rho) is
      | Load(x, l, e) :: is -> phi (StringMap.remove x rho) is
      | IProtectE(x, p, e) :: is -> phi (StringMap.remove x rho) is
      | i :: is -> phi rho is;;

let rec pending (is : instruction list) : guard_fail_id list =
  match is with
      | [] -> []
      | Guard(e, b, cs, p) :: is -> p :: pending is
      | Fail(p) :: is -> p :: pending is
      | i :: is -> pending is

let rec evalExpr (e : expr) (rho : value StringMap.t) : value =
  match e with
      | Cst(v) -> v
      | Var(x) -> StringMap.find x rho
      | BinOp(e1, e2, Add) ->
          (match evalExpr e1 rho, evalExpr e2 rho with
               | CstI(v1), CstI(v2) -> CstI(v1 + v2)
               | _, _ -> failwith "invalid operand type, expected integers for addition")
      | BinOp(e1, e2, Lte) ->
          (match evalExpr e1 rho, evalExpr e2 rho with
               | CstI(v1), CstI(v2) -> CstB(v1 <= v2)
               | _, _ -> failwith "invalid operand type, expected integers for less than equal")
      | BinOp(e1, e2, Lt) ->
          (match evalExpr e1 rho, evalExpr e2 rho with
               | CstI(v1), CstI(v2) -> CstB(v1 < v2)
               | _, _ -> failwith "invalid operand type, expected integers for less than")
      | BinOp(e1, e2, BitAnd) ->
          (match evalExpr e1 rho, evalExpr e2 rho with
               | CstI(v1), CstI(v2) -> CstI(v1 land v2)
               | _, _ -> failwith "invalid operand type, expected integers for less than equal")
      | InlineIf(e1, e2, e3) ->
          (match evalExpr e1 rho with
               | CstB(true) -> evalExpr e2 rho
               | CstB(false) -> evalExpr e3 rho
               | _ -> failwith "invalid operand type, expected boolean for if then else guard")
      | Length(e) ->
          (match evalExpr e rho with
               | CstA(a) -> CstI(a.length)
               | _ -> failwith "invalid operand type, expected array for length")
      | Base(e) ->
          (match evalExpr e rho with
               | CstA(a) -> CstI(a.base)
               | _ -> failwith "invalid operand type, expected array for base");;

let rec eval (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int =
    match conf.is, conf.cs, attacker conf with
        | [], [], _       -> (conf, obs, count)
        | _, _, Fetch     -> printf "fetch"; evalFetch conf decls attacker obs count
        | _, _, PFetch(b) -> printf "pfetch"; evalPFetch conf decls attacker obs count b
        | _, _, Exec(n)   -> printf "exec"; evalExec conf decls attacker obs count n
        | _, _, Retire    -> printf "retire"; evalRetire conf decls attacker obs count

and

evalFetch (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int =
    match conf.cs with
        | Skip :: cs1       -> eval {conf with cs = cs1} decls attacker (None :: obs) (count + 1)
        | Fail :: cs1       -> eval {conf with cs = cs1; is = conf.is @ [Fail(count)]} decls attacker (None :: obs) (count + 1)
        | VarAssign(x, Expr(e)) :: cs1 -> eval {conf with cs = cs1; is = conf.is @ [AssignE(x, e)]} decls attacker (None :: obs) (count + 1)
        | Seq(c1, c2) :: cs1  -> eval {conf with cs = c1 :: c2 :: cs1} decls attacker (None :: obs) count
        | VarAssign(x, PtrRead(e, l)) :: cs1 -> eval {conf with cs = cs1; is = conf.is @ [Load(x, l, e)]} decls attacker (None :: obs) (count + 1)
        | PtrAssign(e1, e2, l) :: cs1 -> eval {conf with cs = cs1; is = conf.is @ [StoreE(e1, e2)]} decls attacker (None :: obs) (count + 1)
        | VarAssign(x, ArrayRead(a, e1)) :: cs1 -> let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
                                                 let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
                                                 let c' = If(e, (VarAssign(x, PtrRead(e', a.label))), Fail) in
                                                 eval {conf with cs = c' :: cs1} decls attacker (None :: obs) (count + 1)
        | ArrAssign(a, e1, e2) :: cs1 -> let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
                                         let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
                                         let c' = If(e, (PtrAssign(e', e, a.label)), Fail) in
                                         eval {conf with cs = c' :: cs1} decls attacker (None :: obs) (count + 1)
        | While(e, c) :: cs1 -> let c1 = Seq(c, While(e, c)) in
                                let c2 = If(e, c1, Skip) in
                                eval {conf with cs = c2 :: cs1} decls attacker (None :: obs) (count + 1)
        (* TODO: Protect *)
        | Protect(x, Slh, ArrayRead(a, e)) :: cs1 ->
            let e1 = BinOp(e, Length(Cst(CstA(a))), Lt) in
            let e2 = BinOp(Base(Cst(CstA(a))), e, Add) in
            let c1 = VarAssign("mask", Expr(e1)) in
            let c2 = VarAssign("mask", Expr(InlineIf(Var("mask"), Cst(CstB(true)), Cst(CstB(false))))) in
            let c3 = VarAssign(x, PtrRead(BinOp(e2, Var("mask"), BitAnd), a.label)) in
            let c' = Seq(c1, If(Var("mask"), Seq(c2, c3), Fail)) in
            eval {conf with cs = c' :: cs1} decls attacker (None :: obs) (count + 1)
        | Protect(x, p, PtrRead(e, l)) :: cs1 ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', PtrRead(e, l)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            eval {conf with cs = c1 :: c2 :: cs1} decls attacker (None :: obs) (count + 1)
        | Protect(x, p, ArrayRead(a, e)) :: cs1 ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', ArrayRead(a, e)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            eval {conf with cs = c1 :: c2 :: cs1} decls attacker (None :: obs) (count + 1)
        | Protect(x, p, Expr(e)) :: cs1 ->
            eval {conf with cs = cs1; is = conf.is @ [IProtectE(x, p, e)]} decls attacker (None :: obs) (count + 1)
        | [] -> (conf, obs, count)
        | _  -> failwith "not implemented"
and

evalPFetch (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) (b : prediction): configuration * observation list * int =
    match conf.cs, b with
        | If(e, c1, c2) :: cs1, true  ->
            let conf' = {conf with is = conf.is @ [Guard(e, true, c2 :: cs1, count)]; cs = c1 :: cs1}
             in eval conf' decls attacker (None :: obs) (count + 1)
        | If(e, c1, c2) :: cs1, false ->
            let conf' = {conf with is = conf.is @ [Guard(e, false, c1 :: cs1, count)]; cs = c2 :: cs1}
             in eval conf' decls attacker (None :: obs) (count + 1)
        | _ -> failwith "invalid directive"

and

evalExec (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) (n: int): configuration * observation list * int =
    match splitIs conf.is n with
        | (fs, i , ls) -> let rho1 = phi conf.rho fs in
            (match i with
                 | Nop -> failwith "invalid directive"
                 | AssignE(id, e)      -> let v = evalExpr e rho1 in
                                          let i1 = AssignV(id, v) in
                                          eval {conf with is = fs @ [i1] @ ls} decls attacker (None :: obs) (count + 1)
                 | Guard(e, b, cls, p) -> (match evalExpr e rho1 with
                                               | CstB(b1) -> if b1 == b then
                                                               eval {conf with is = fs @ [Nop] @ ls} decls attacker (None :: obs) (count + 1)
                                                             else
                                                               eval {conf with is = fs @ [Nop]; cs = cls} decls attacker (Rollback(p) :: obs) (count + 1)
                                               | _ -> failwith "guard must be a boolean")
                 | Load(x, l, e) -> if List.exists isStore fs then
                                      failwith "invalid directive"
                                    else
                                      (match evalExpr e rho1 with
                                          | CstI(n) -> let ps = pending fs in
                                                       let i' = AssignV(x, CstI(conf.mu.(n))) in
                                                       eval {conf with is = fs @ [i'] @ ls} decls attacker (Read(n, ps) :: obs) (count + 1)
                                          | _ -> failwith "index must be an integer")
                 | StoreE(e1, e2) -> (match evalExpr e1 rho1 with
                                          | CstI(n) -> let ps = pending fs in
                                                       (match evalExpr e2 rho1 with
                                                            | CstI(v) -> let i' = StoreV(n, v) in
                                                                         eval {conf with is = fs @ [i'] @ ls} decls attacker (Write(n, ps) :: obs) (count + 1)
                                                            | _ -> failwith "value must be an integer")
                                          | _ -> failwith "index must be an integer")
                 | IProtectE(x, p, e) -> let v = evalExpr e rho1 in
                                         let i' = IProtectV(x, v) in
                                         eval {conf with is = fs @ [i'] @ ls} decls attacker (None :: obs) (count + 1)
                 | IProtectV(x, v) -> if List.exists isGuard fs then
                                        failwith "invalid directive"
                                      else
                                        eval {conf with is = fs @ [AssignV(x, v)] @ ls} decls attacker (None :: obs) (count + 1)
                 | _ -> failwith "invalid directive")

and

evalRetire (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int =
  match conf.is with
      | []                  -> failwith "invalid directive"
      | Nop :: is           -> eval {conf with is = is} decls attacker (None :: obs) (count + 1)
      | AssignV(x, v) :: is -> eval {conf with is = is; rho = StringMap.add x v conf.rho} decls attacker (None :: obs) (count + 1)
      | StoreV(n, v) :: is  -> conf.mu.(n) <- v;
                               eval {conf with is = is} decls attacker (None :: obs) (count + 1)
      | Fail(p) :: is       -> eval {conf with is = []; cs = []} decls attacker (Fail(p) :: obs) (count + 1)
      | _ -> failwith "invalid directive";;

let conf = {is=[]; cs=[Seq(VarAssign("x", Expr(Cst(CstI(0)))), VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(1)), Add))))]; mu=[||]; rho=StringMap.empty} ;;

eval conf StringMap.empty (fun c -> if List.length c.cs == 0 then Exec(0) else Fetch) [] 0;;
