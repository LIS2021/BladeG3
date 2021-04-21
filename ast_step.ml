open Format

module StringMap = Map.Make(String)

type identifier = string;;

type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

type label = unit;;

type arr = {
  base : int;
  length : int;
  label : label;
};;

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

let splitIs (ls : 'a list) (n : int) =
  let rec split_is_rec (fls : 'a list) (ls : 'a list) (n : int) =
      (match ls, n with
          | a :: cls, n -> if n == 0 then
                             (fls, a, cls)
                           else if n > 0 then
                                  split_is_rec (fls @ [a]) cls (n - 1)
                                else
                                  failwith "invalid directive"
          | [], n -> failwith "invalid directive")
   in split_is_rec [] ls n;;

let isStore (i : instruction) : bool =
  match i with
      | StoreE(_, _) -> true
      | StoreV(_, _) -> true
      | _            -> false;;

let isGuard (i : instruction) : bool =
  match i with
      | Guard(_, _, _, _) -> true
      | _                 -> false;;

let freshName (name : string) (rho : value StringMap.t) : string =
  let rec helper name rho counter =
    if StringMap.mem (name ^ (string_of_int counter)) rho then
      helper name rho (counter + 1)
    else
      name ^ (string_of_int counter)
   in helper name rho 0


let rec phi (rho : value StringMap.t) (is : instruction list) : value StringMap.t =
  match is with
      | []                       -> rho
      | AssignV(x, v) :: is      -> phi (StringMap.add x v rho) is
      | AssignE(x, e) :: is      -> phi (StringMap.remove x rho) is
      | Load(x, l, e) :: is      -> phi (StringMap.remove x rho) is
      | IProtectE(x, p, e) :: is -> phi (StringMap.remove x rho) is
      | i :: is                  -> phi rho is;;

let rec pending (is : instruction list) : guard_fail_id list =
  match is with
      | []                       -> []
      | Guard(e, b, cs, p) :: is -> p :: pending is
      | Fail(p) :: is            -> p :: pending is
      | i :: is                  -> pending is;;

let rec evalExpr (e : expr) (rho : value StringMap.t) : value =
  match e with
      | Cst(v) -> v
      | Var(x) ->
          (match StringMap.find_opt x rho with
               | Some(v) -> v
               | None -> failwith ("referenced undeclared variable " ^ x))
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
               | CstB(true)  -> evalExpr e2 rho
               | CstB(false) -> evalExpr e3 rho
               | _ -> failwith "invalid operand type, expected boolean for inline if guard")
      | Length(e) ->
          (match evalExpr e rho with
               | CstA(a) -> CstI(a.length)
               | _ -> failwith "invalid operand type, expected array for length")
      | Base(e) ->
          (match evalExpr e rho with
               | CstA(a) -> CstI(a.base)
               | _ -> failwith "invalid operand type, expected array for base");;


let stepFetch (conf: configuration) (obs : observation list) (count : int) : configuration * observation list * int =
    match conf.cs with
        | Skip :: cs ->
            {conf with cs = cs}, None :: obs, count + 1
        | Fail :: cs ->
            {conf with cs = cs; is = conf.is @ [Fail(count)]}, None :: obs, count + 1
        | VarAssign(x, Expr(e)) :: cs ->
            {conf with cs = cs; is = conf.is @ [AssignE(x, e)]}, None :: obs, count + 1
        | Seq(c1, c2) :: cs ->
            {conf with cs = c1 :: c2 :: cs}, None :: obs, count
        | VarAssign(x, PtrRead(e, l)) :: cs ->
            {conf with cs = cs; is = conf.is @ [Load(x, l, e)]}, None :: obs, count + 1
        | PtrAssign(e1, e2, l) :: cs ->
            {conf with cs = cs; is = conf.is @ [StoreE(e1, e2)]}, None :: obs, count + 1
        | VarAssign(x, ArrayRead(a, e1)) :: cs ->
            let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
            let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
            let c' = If(e, (VarAssign(x, PtrRead(e', a.label))), Fail) in
            {conf with cs = c' :: cs}, None :: obs, count + 1
        | ArrAssign(a, e1, e2) :: cs ->
            let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
            let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
            let c' = If(e, (PtrAssign(e', e, a.label)), Fail) in
            {conf with cs = c' :: cs}, None :: obs, count + 1
        | While(e, c) :: cs ->
            let c1 = Seq(c, While(e, c)) in
            let c2 = If(e, c1, Skip) in
            {conf with cs = c2 :: cs}, None :: obs, count + 1
        | Protect(x, Slh, ArrayRead(a, e)) :: cs ->
            let mask = "_mask_" ^ x in
            let e1 = BinOp(e, Length(Cst(CstA(a))), Lt) in
            let e2 = BinOp(Base(Cst(CstA(a))), e, Add) in
            let c1 = VarAssign(mask, Expr(e1)) in
            let c2 = VarAssign(mask, Expr(InlineIf(Var(mask), Cst(CstB(true)), Cst(CstB(false))))) in
            let c3 = VarAssign(x, PtrRead(BinOp(e2, Var(mask), BitAnd), a.label)) in
            let c' = Seq(c1, If(Var(mask), Seq(c2, c3), Fail)) in
            {conf with cs = c' :: cs}, None :: obs, count + 1
        | Protect(x, p, PtrRead(e, l)) :: cs ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', PtrRead(e, l)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            {conf with cs = c1 :: c2 :: cs}, None :: obs, count + 1
        | Protect(x, p, ArrayRead(a, e)) :: cs ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', ArrayRead(a, e)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            {conf with cs = c1 :: c2 :: cs}, None :: obs, count + 1
        | Protect(x, p, Expr(e)) :: cs ->
            {conf with cs = cs; is = conf.is @ [IProtectE(x, p, e)]}, None :: obs, count + 1
        | _  -> failwith "invalid fetch directive";;

let stepPFetch (conf: configuration) (obs : observation list) (count : int) (b : prediction): configuration * observation list * int =
    match conf.cs, b with
        | If(e, c1, c2) :: cs, true  ->
            {conf with is = conf.is @ [Guard(e, true, c2 :: cs, count)]; cs = c1 :: cs}, None :: obs, count + 1
        | If(e, c1, c2) :: cs, false ->
            {conf with is = conf.is @ [Guard(e, false, c1 :: cs, count)]; cs = c2 :: cs}, None :: obs, count + 1
        | _ -> failwith "invalid preditive fetch directive";;

let stepExec (conf: configuration) (obs : observation list) (count : int) (n: int): configuration * observation list * int =
    match splitIs conf.is n with
        | (fs, i , ls) -> let rho1 = phi conf.rho fs in
            (match i with
                 | AssignE(id, e) ->
                     let v = evalExpr e rho1 in
                     {conf with is = fs @ [AssignV(id, v)] @ ls}, None :: obs, count + 1
                 | Guard(e, b, cls, p) ->
                     (match evalExpr e rho1 with
                            | CstB(b1) -> if b1 == b then
                                            {conf with is = fs @ [Nop] @ ls}, None :: obs, count + 1
                                          else
                                            {conf with is = fs @ [Nop]; cs = cls}, Rollback(p) :: obs, count + 1
                            | _ -> failwith "guard must be a boolean")
                 | Load(x, l, e) ->
                     if List.exists isStore fs then
                       failwith "invalid exec directive"
                     else
                       (match evalExpr e rho1 with
                            | CstI(n) -> let i' = AssignV(x, CstI(conf.mu.(n))) in
                                         {conf with is = fs @ [i'] @ ls}, Read(n, pending fs) :: obs, count + 1
                            | _ -> failwith "load index must be an integer")
                 | StoreE(e1, e2) ->
                     (match evalExpr e1 rho1 with
                          | CstI(n) -> (match evalExpr e2 rho1 with
                                            | CstI(v) -> {conf with is = fs @ [StoreV(n, v)] @ ls}, Write(n, pending fs) :: obs, count + 1
                                            | _ -> failwith "store value must be an integer")
                          | _ -> failwith "store index must be an integer")
                 | IProtectE(x, p, e) ->
                     let v = evalExpr e rho1 in
                     {conf with is = fs @ [IProtectV(x, v)] @ ls}, None :: obs, count + 1
                 | IProtectV(x, v) ->
                     if List.exists isGuard fs then
                       failwith "invalid exec directive"
                     else
                       {conf with is = fs @ [AssignV(x, v)] @ ls}, None :: obs, count + 1
                 | _ -> failwith "invalid exec directive");;

let stepRetire (conf: configuration) (obs : observation list) (count : int) : configuration * observation list * int =
  match conf.is with
      | Nop :: is           -> {conf with is = is}, None :: obs, count + 1
      | AssignV(x, v) :: is -> {conf with is = is; rho = StringMap.add x v conf.rho}, None :: obs, count + 1
      | StoreV(n, v) :: is  -> conf.mu.(n) <- v;
                               {conf with is = is}, None :: obs, count + 1
      | Fail(p) :: is       -> {conf with is = []; cs = []}, Fail(p) :: obs, count + 1
      | _ -> failwith "invalid retire directive";;

let step (conf: configuration) (dir : directive) (obs : observation list) (count : int) : (configuration * observation list * int) option =
    match conf.is, conf.cs, dir with
        | [], [], _       -> None
        | _, _, Fetch     -> Some(stepFetch conf obs count)
        | _, _, PFetch(b) -> Some(stepPFetch conf obs count b)
        | _, _, Exec(n)   -> Some(stepExec conf obs count n)
        | _, _, Retire    -> Some(stepRetire conf obs count);;

let eval (conf : configuration) (attacker : configuration -> observation list -> directive) : configuration * observation list * int =
  let rec helper conf attacker obs count =
    match step conf (attacker conf obs) obs count with
        | Some(conf', obs', count') -> helper conf' attacker obs' count'
        | None -> conf, obs, count
   in helper conf attacker [] 0;;

let evalList (conf : configuration) (attacker : directive list) : configuration * observation list * int =
  let rec helper conf attacker obs count =
    match attacker with
        | [] -> conf, obs, count
        | d :: ds -> (match step conf d obs count with
                          | Some(conf', obs', count') -> helper conf' ds obs' count'
                          | None -> conf, obs, count)
   in helper conf attacker [] 0;;


let conf = {is=[]; cs=[Seq(VarAssign("x", Expr(Cst(CstI(0)))), VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(1)), Add))))]; mu=[||]; rho=StringMap.empty};;

(* EXAMPLES *)
(* step conf Fetch [] 0;; *)
(* eval conf (fun _ -> Fetch);; *)
(* evalList conf [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];; *)
