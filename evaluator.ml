open Ast

(**		CONFIGURATIONS 		**)
type configuration = {
	is : instruction list ;
	cs : cmd list ;
	mu : int array ;
	rho : value StringMap.t ;
};;

type vmerror =
  | EndOfStream
  | InvalidDirective of directive
  | InvalidType
  | InvalidGuardType
  | InstructionOutOfRange
  | UnassignedReference of string
  | InvalidOperandType of string;;

type 'a vmresult = ('a, vmerror) result;;

let pure x = Ok x;;
let err e = Error e;;
let (let+) x f = Result.map f x;;
let (and+) r1 r2 =
  match r1, r2 with
    | Ok x, Ok y       -> Ok (x, y)
    | Error e, Ok _    -> Error e
    | Ok _, Error e    -> Error e
    | Error e, Error _ -> Error e;;
let (let*) = Result.bind;;
let (>>=) = Result.bind;;

let splitIs (ls : 'a list) (n : int) : ('a list * 'a * 'a list) vmresult =
  let rec split_is_rec fls n = function
    | [] -> err InstructionOutOfRange
    | a :: cls ->
        if n == 0 then
          pure (fls, a, cls)
        else if n > 0 then
          split_is_rec (fls @ [a]) (n - 1) cls
        else
          err InstructionOutOfRange
   in split_is_rec [] n ls;;

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


let checkInteger (v : value) : int vmresult =
  match v with
    | CstI(n) -> pure n
    | _ -> err (InvalidOperandType "integer");;

let checkBoolean (v : value) : bool vmresult =
  match v with
    | CstB(b) -> pure b
    | _ -> err (InvalidOperandType "boolean");;

let checkArray (v : value) : arr vmresult =
  match v with
    | CstA(a) -> pure a
    | _ -> err (InvalidOperandType "array");;


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
      | IFail(p) :: is            -> p :: pending is
      | i :: is                  -> pending is;;


let rec evalExpr (e : expr) (rho : value StringMap.t) : value vmresult =
  match e with
      | Cst(v) -> pure v
      | Var(x) -> Option.to_result ~none:(UnassignedReference(x)) (StringMap.find_opt x rho)
      | BinOp(e1, e2, Add) ->
          let* n1 = evalExpr e1 rho >>= checkInteger
          and+ n2 = evalExpr e2 rho >>= checkInteger in
          pure (CstI (n1 + n2))
      | BinOp(e1, e2, Lte) ->
          let* n1 = evalExpr e1 rho >>= checkInteger
          and+ n2 = evalExpr e2 rho >>= checkInteger in
          pure (CstB (n1 <= n2))
      | BinOp(e1, e2, Lt) ->
          let* v1 = evalExpr e1 rho >>= checkInteger
          and+ v2 = evalExpr e2 rho >>= checkInteger in
          pure (CstB (v1 < v2))
      | BinOp(e1, e2, BitAnd) ->
          let* v1 = evalExpr e1 rho >>= checkInteger
          and+ v2 = evalExpr e2 rho >>= checkInteger in
          pure (CstI (v1 land v2))
      | InlineIf(e1, e2, e3) ->
          let* b1 = evalExpr e1 rho >>= checkBoolean in
          if b1 then evalExpr e2 rho else evalExpr e3 rho
      | Length(e) ->
          let* a = evalExpr e rho >>= checkArray in
          pure (CstI a.length)
      | Base(e) ->
          let* a = evalExpr e rho >>= checkArray in
          pure (CstI a.base);;

let stepFetch (conf: configuration) (obs : observation list) (count : int) : (configuration * observation list * int) vmresult =
    match conf.cs with
        | Skip :: cs ->
            pure ({conf with cs = cs}, None :: obs, count + 1)
        | Fail :: cs ->
            pure ({conf with cs = cs; is = conf.is @ [IFail(count)]}, None :: obs, count + 1)
        | VarAssign(x, Expr(e)) :: cs ->
            pure ({conf with cs = cs; is = conf.is @ [AssignE(x, e)]}, None :: obs, count + 1)
        | Seq(c1, c2) :: cs ->
            pure ({conf with cs = c1 :: c2 :: cs}, None :: obs, count)
        | VarAssign(x, PtrRead(e, l)) :: cs ->
            pure ({conf with cs = cs; is = conf.is @ [Load(x, l, e)]}, None :: obs, count + 1)
        | PtrAssign(e1, e2, l) :: cs ->
            pure ({conf with cs = cs; is = conf.is @ [StoreE(e1, e2)]}, None :: obs, count + 1)
        | VarAssign(x, ArrayRead(a, e1)) :: cs ->
            let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
            let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
            let c' = If(e, (VarAssign(x, PtrRead(e', a.label))), Fail) in
            pure ({conf with cs = c' :: cs}, None :: obs, count + 1)
        | ArrAssign(a, e1, e2) :: cs ->
            let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
            let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
            let c' = If(e, (PtrAssign(e', e2, a.label)), Fail) in
            pure ({conf with cs = c' :: cs}, None :: obs, count + 1)
        | While(e, c) :: cs ->
            let c1 = Seq(c, While(e, c)) in
            let c2 = If(e, c1, Skip) in
            pure ({conf with cs = c2 :: cs}, None :: obs, count + 1)
        | Protect(x, Slh, ArrayRead(a, e)) :: cs ->
            let mask = "_mask_" ^ x in
            let e1 = BinOp(e, Length(Cst(CstA(a))), Lt) in
            let e2 = BinOp(Base(Cst(CstA(a))), e, Add) in
            let c1 = VarAssign(mask, Expr(e1)) in
            let c2 = VarAssign(mask, Expr(InlineIf(Var(mask), Cst(CstB(true)), Cst(CstB(false))))) in
            let c3 = VarAssign(x, PtrRead(BinOp(e2, Var(mask), BitAnd), a.label)) in
            let c' = Seq(c1, If(Var(mask), Seq(c2, c3), Fail)) in
            pure ({conf with cs = c' :: cs}, None :: obs, count + 1)
        | Protect(x, p, PtrRead(e, l)) :: cs ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', PtrRead(e, l)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            pure ({conf with cs = c1 :: c2 :: cs}, None :: obs, count + 1)
        | Protect(x, p, ArrayRead(a, e)) :: cs ->
            let x' = freshName x conf.rho in
            let c1 = VarAssign(x', ArrayRead(a, e)) in
            let c2 = Protect(x, p, Expr(Var(x'))) in
            pure ({conf with cs = c1 :: c2 :: cs}, None :: obs, count + 1)
        | Protect(x, p, Expr(e)) :: cs ->
            pure ({conf with cs = cs; is = conf.is @ [IProtectE(x, p, e)]}, None :: obs, count + 1)
        | _  -> err (InvalidDirective Fetch);;

let stepPFetch (conf: configuration) (obs : observation list) (count : int) (b : prediction): (configuration * observation list * int) vmresult =
    match conf.cs, b with
        | If(e, c1, c2) :: cs, true  ->
            pure ({conf with is = conf.is @ [Guard(e, true, c2 :: cs, count)]; cs = c1 :: cs}, None :: obs, count + 1)
        | If(e, c1, c2) :: cs, false ->
            pure ({conf with is = conf.is @ [Guard(e, false, c1 :: cs, count)]; cs = c2 :: cs}, None :: obs, count + 1)
        | _ -> err (InvalidDirective (PFetch b));;

let stepExec (conf: configuration) (obs : observation list) (count : int) (n: int): (configuration * observation list * int) vmresult =
    let* s = splitIs conf.is n in
    match s with
        | (fs, i , ls) -> let rho1 = phi conf.rho fs in
            (match i with
                 | AssignE(id, e) ->
                     let* v = evalExpr e rho1 in
                     pure ({conf with is = fs @ [AssignV(id, v)] @ ls}, None :: obs, count + 1)
                 | Guard(e, b, cls, p) ->
                     let* b1 = evalExpr e rho1 >>= checkBoolean in
                     if b1 == b then
                       pure ({conf with is = fs @ [Nop] @ ls}, None :: obs, count + 1)
                     else
                       pure ({conf with is = fs @ [Nop]; cs = cls}, Rollback(p) :: obs, count + 1)
                 | Load(x, l, e) ->
                     if List.exists isStore fs then
                       err (InvalidDirective (Exec n))
                     else
                       let* n = evalExpr e rho1 >>= checkInteger in
                       let i' = AssignV(x, CstI(conf.mu.(n))) in
                       pure ({conf with is = fs @ [i'] @ ls}, Read(n, pending fs) :: obs, count + 1)
                 | StoreE(e1, e2) ->
                     let* n = evalExpr e1 rho1 >>= checkInteger in
                     let* v = evalExpr e2 rho1 >>= checkInteger in
                     pure ({conf with is = fs @ [StoreV(n, v)] @ ls}, Write(n, pending fs) :: obs, count + 1)
                 | IProtectE(x, p, e) ->
                     let* v = evalExpr e rho1 in
                     pure ({conf with is = fs @ [IProtectV(x, v)] @ ls}, None :: obs, count + 1)
                 | IProtectV(x, v) ->
                     if List.exists isGuard fs then
                       err (InvalidDirective (Exec n))
                     else
                       pure ({conf with is = fs @ [AssignV(x, v)] @ ls}, None :: obs, count + 1)
                 | _ -> err (InvalidDirective (Exec n)));;

let stepRetire (conf: configuration) (obs : observation list) (count : int) : (configuration * observation list * int) vmresult =
  match conf.is with
      | Nop :: is           -> pure ({conf with is = is}, None :: obs, count + 1)
      | AssignV(x, v) :: is -> pure ({conf with is = is; rho = StringMap.add x v conf.rho}, None :: obs, count + 1)
      | StoreV(n, v) :: is  -> conf.mu.(n) <- v;
                               pure ({conf with is = is}, None :: obs, count + 1)
      | IFail(p) :: is       -> pure ({conf with is = []; cs = []}, OFail p :: obs, count + 1)
      | _ -> err (InvalidDirective Retire);;

let step (conf: configuration) (dir : directive) (obs : observation list) (count : int) : (configuration * observation list * int) vmresult =
    match conf.is, conf.cs, dir with
        | [], [], _       -> err EndOfStream
        | _, _, Fetch     -> stepFetch conf obs count
        | _, _, PFetch(b) -> stepPFetch conf obs count b
        | _, _, Exec(n)   -> stepExec conf obs count n
        | _, _, Retire    -> stepRetire conf obs count;;

let eval (conf : configuration) (speculator : configuration -> observation list -> directive) : (configuration * observation list * int) vmresult =
  let rec helper conf speculator obs count =
    match step conf (speculator conf obs) obs count with
        | Ok (conf', obs', count') -> helper conf' speculator obs' count'
        | Error EndOfStream -> pure (conf, obs, count)
        | Error e -> err e
   in helper conf speculator [] 0;;

let evalList' (conf : configuration) (speculator : directive list) (injector : (configuration * observation list * int) -> unit) : (configuration * observation list * int) vmresult=
  let rec helper conf speculator obs count =
    match speculator with
        | [] -> pure (conf, obs, count)
        | d :: ds -> (match step conf d obs count with
                          | Ok (conf', obs', count') ->
                                  injector(conf', obs', count');
                                  helper conf' ds obs' count'
                          | Error EndOfStream -> pure (conf, obs, count)
                          | Error e -> err e)
   in helper conf speculator [] 0;;

let evalList (conf : configuration) (speculator : directive list) : (configuration * observation list * int) vmresult =
    evalList' conf speculator (fun _ -> ());;

let defaultSpeculator (dist : unit -> bool) (conf : configuration) (obs : observation list) : directive =
  match conf.is, conf.cs with
    | _, If(e, c1, c2) :: _ -> PFetch (dist ())
    | Nop :: _, _           -> Retire
    | AssignV(_, _) :: _, _ -> Retire
    | StoreV(_, _) :: _, _  -> Retire
    | IFail(_) :: _, _       -> Retire
    | _ :: _, _             -> Exec 0
    | _, _                  -> Fetch;;

let conf = {is=[]; cs=[Seq(VarAssign("x", Expr(Cst(CstI(0)))), VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(1)), Add))))]; mu=[||]; rho=StringMap.empty} ;;
evalList conf [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];;
