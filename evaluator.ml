open Ast

(**		CONFIGURATIONS 		**)
type configuration = {
	is : instruction list ;
	cs : cmd list ;
	mu : int array ;
	rho : value StringMap.t ;
};;

let string_of_configuration (conf : configuration) : string =
  "is:\n" ^ (List.fold_left (fun it i -> Printf.sprintf "%s%s;\n" it (string_of_instruction i)) "" conf.is) ^ "\n\n" ^
  "cs:\n" ^ (List.fold_left (fun it c -> Printf.sprintf "%s%s;\n" it (string_of_cmd c)) "" conf.cs) ^ "\n\n" ^
  "mu: [ " ^ (Array.fold_left (fun it v -> Printf.sprintf "%s%d; " it v) "" conf.mu) ^ "]\n\n" ^
  "rho: { " ^ (StringMap.fold (fun k v it -> Printf.sprintf "%s%s: %s; " it k (string_of_value v)) conf.rho "") ^ "}\n\n";;

let string_of_output ((conf : configuration), (obs : observation list), (count : int)) : string =
  String.make 20 '-' ^ "\n\n" ^
  string_of_configuration conf ^
  "obs:\n[ " ^ (List.fold_left (fun it o -> Printf.sprintf "%s%s; " it (string_of_observation o)) "" obs) ^ "]\n\n" ^
  "count: " ^ (string_of_int count) ^ "\n\n";;

let string_of_verbose ((conf : configuration), (obs : observation list), (count : int)) : string =
  "mu: [ " ^ (Array.fold_left (fun it v -> Printf.sprintf "%s%d; " it v) "" conf.mu) ^ "]\n\n" ^
  "rho: { " ^ (StringMap.fold (fun k v it -> Printf.sprintf "%s%s: %s; " it k (string_of_value v)) conf.rho "") ^ "}\n\n" ^
  "obs:\n[ " ^ (List.fold_left (fun it o -> Printf.sprintf "%s%s; " it (string_of_observation o)) "" obs) ^ "]\n\n" ^
  "count: " ^ (string_of_int count) ^ "\n\n";;

(** Module modeling the template of a cost model **)
module type CostModel = sig
  (** Given a directive and a configuration
      returns the cost the instruction ran **)
  val cost : directive -> configuration -> int
end

(** Module modeling the template of a speculator **)
module type Speculator = sig
  (** Given a configuration and a list of observations
      returns a possible next directive **)
  val speculate : configuration -> observation list -> directive
end

type vmerror =
  | EndOfStream
  | InvalidDirective of directive
  | InvalidType
  | InvalidGuardType
  | InstructionOutOfRange
  | UnassignedReference of string
  | InvalidOperandType of string * string;;

let string_of_vmerror = function
  | EndOfStream -> "no more instructions or command available"
  | InvalidDirective d -> Printf.sprintf "invalid %s directive" (string_of_directive d)
  | InvalidType -> "invalid type"
  | InvalidGuardType -> "invalid type for guard"
  | InstructionOutOfRange -> "instruction out of range"
  | UnassignedReference s -> Printf.sprintf "identifier %s has not been initialized yet" s
  | InvalidOperandType (s1, s2) -> Printf.sprintf "invalid operand type: expected %s, got %s" s1 s2;;

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

let rollbackCount = ref 0;;
let freshRollbackId () = incr rollbackCount; !rollbackCount;;

(** Given a list and an integer
    returns the list of items before the item with the given index
    the item with the given index
    and the list of items following that item **)
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

let rec countMemIstruction (is: instruction list) : int =
  match is with
    | StoreE(_, _) :: is'
    | StoreV(_, _) :: is'
    | Load(_, _, _) :: is' -> 1 + countMemIstruction is'
    | _ :: is' -> countMemIstruction is'
    | _ -> 0;;

let isStore (i : instruction) : bool =
  match i with
    | StoreE(_, _) -> true
    | StoreV(_, _) -> true
    | _            -> false;;

let isGuard (i : instruction) : bool =
  match i with
    | Guard(_, _, _, _) -> true
    | _                 -> false;;

let isFence (i : instruction) : bool =
  match i with
      | IProtectE(_, Fence, _) -> true
      | _                    -> false;;

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
    | CstB(b) -> pure(Bool.to_int b)
    | _ -> err (InvalidOperandType ("integer", "array"));;

let checkBoolean (v : value) : bool vmresult =
  match v with
    | CstB(b) -> pure b
    | CstI(i) -> pure(i != 0)
    | _ -> err (InvalidOperandType ("boolean", "array"));;

let checkArray (v : value) : arr vmresult =
  match v with
    | CstA(a) -> pure a
    | CstI(_) -> err (InvalidOperandType ("array", "integer"))
    | CstB(_) -> err (InvalidOperandType ("array", "boolean"));;


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
        and+ v2 = evalExpr e2 rho >>= checkBoolean in
        pure (CstI (if v2 then v1 else 0))
    | InlineIf(e1, e2, e3) ->
        let* b1 = evalExpr e1 rho >>= checkBoolean in
        if b1 then evalExpr e2 rho else evalExpr e3 rho
    | Length(e) ->
        let* a = evalExpr e rho >>= checkArray in
        pure (CstI a.length)
    | Base(e) ->
        let* a = evalExpr e rho >>= checkArray in
        pure (CstI a.base);;

let canFetch (conf : configuration) : bool =
  match conf.cs with
    | Skip :: _
    | Fail :: _
    | VarAssign(_, _) :: _
    | Seq(_, _) :: _
    | PtrAssign(_, _, _) :: _
    | ArrAssign(_, _, _) :: _
    | While(_, _) :: _
    | Protect(_, _, _) :: _ -> true
    | _ -> false;;

let canPFetch (conf : configuration) : bool =
  match conf.cs with
    | If(_, _, _) :: _ -> true
    | _ -> false;;

let canEval (conf : configuration) (n : int) : bool =
  match splitIs conf.is n with
    | Ok (fs, i, ls) -> let rho1 = phi conf.rho fs in
        (match i with
           | AssignE(_, e) ->
               (match evalExpr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | Guard(e, _, _, _) ->
               (match evalExpr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | IProtectE(_, _, e) ->
               (match evalExpr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | Load(x, l, e) -> if (List.exists (fun i -> isStore i || isFence i) fs) then false else
               (match evalExpr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | StoreE(e1, e2) -> if (List.exists isFence fs) then false else
               (match evalExpr e1 rho1, evalExpr e2 rho1 with
                  | _, Error (UnassignedReference _) -> false
                  | Error (UnassignedReference _), _ -> false
                  | _, _ -> true)
           | IProtectV(x, v) -> not (List.exists isGuard fs)
           | _ -> false)
    | _ -> false;;

let canRetire (conf : configuration) : bool =
  match conf.is with
    | Nop :: _
    | AssignV(_, _) :: _
    | StoreV(_, _) :: _
    | IFail(_) :: _ -> true
    | _ -> false;;

let stepFetch (conf: configuration) (obs : observation list) : (configuration * observation list) vmresult =
  match conf.cs with
    | Skip :: cs ->
        pure ({conf with cs = cs}, None :: obs)
    | Fail :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [IFail(freshRollbackId ())]}, None :: obs)
    | VarAssign(x, Expr(e)) :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [AssignE(x, e)]}, None :: obs)
    | Seq(c1, c2) :: cs ->
        pure ({conf with cs = c1 :: c2 :: cs}, None :: obs)
    | VarAssign(x, PtrRead(e, l)) :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [Load(x, l, e)]}, None :: obs)
    | PtrAssign(e1, e2, l) :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [StoreE(e1, e2)]}, None :: obs)
    | VarAssign(x, ArrayRead(a, e1)) :: cs ->
        let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
        let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
        let c' = If(e, (VarAssign(x, PtrRead(e', a.label))), Fail) in
        pure ({conf with cs = c' :: cs}, None :: obs)
    | ArrAssign(a, e1, e2) :: cs ->
        let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
        let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
        let c' = If(e, (PtrAssign(e', e2, a.label)), Fail) in
        pure ({conf with cs = c' :: cs}, None :: obs)
    | While(e, c) :: cs ->
        let c1 = Seq(c, While(e, c)) in
        let c2 = If(e, c1, Skip) in
        pure ({conf with cs = c2 :: cs}, None :: obs)
    | Protect(x, Slh, ArrayRead(a, e)) :: cs ->
        let mask = "_mask_" ^ x in
        let e1 = BinOp(e, Length(Cst(CstA(a))), Lt) in
        let e2 = BinOp(Base(Cst(CstA(a))), e, Add) in
        let c1 = VarAssign(mask, Expr(e1)) in
        let c2 = VarAssign(mask, Expr(InlineIf(Var(mask), Cst(CstB(true)), Cst(CstB(false))))) in
        let c3 = VarAssign(x, PtrRead(BinOp(e2, Var(mask), BitAnd), a.label)) in
        let c' = Seq(c1, If(Var(mask), Seq(c2, c3), Fail)) in
        pure ({conf with cs = c' :: cs}, None :: obs)
    | Protect(x, p, PtrRead(e, l)) :: cs ->
        let x' = freshName x conf.rho in
        let c1 = VarAssign(x', PtrRead(e, l)) in
        let c2 = Protect(x, p, Expr(Var(x'))) in
        pure ({conf with cs = c1 :: c2 :: cs}, None :: obs)
    | Protect(x, p, ArrayRead(a, e)) :: cs ->
        let x' = freshName x conf.rho in
        let c1 = VarAssign(x', ArrayRead(a, e)) in
        let c2 = Protect(x, p, Expr(Var(x'))) in
        pure ({conf with cs = c1 :: c2 :: cs}, None :: obs)
    | Protect(x, p, Expr(e)) :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [IProtectE(x, p, e)]}, None :: obs)
    | _  -> err (InvalidDirective Fetch);;

let stepPFetch (conf: configuration) (obs : observation list) (b : prediction): (configuration * observation list) vmresult =
  match conf.cs, b with
    | If(e, c1, c2) :: cs, true  ->
        pure ({conf with is = conf.is @ [Guard(e, true, c2 :: cs, freshRollbackId ())]; cs = c1 :: cs}, None :: obs)
    | If(e, c1, c2) :: cs, false ->
        pure ({conf with is = conf.is @ [Guard(e, false, c1 :: cs, freshRollbackId ())]; cs = c2 :: cs}, None :: obs)
    | _ -> err (InvalidDirective (PFetch b));;

let stepExec (conf: configuration) (obs : observation list) (n: int): (configuration * observation list) vmresult =
  let* s = splitIs conf.is n in
  match s with
    | (fs, i , ls) -> let rho1 = phi conf.rho fs in
        (match i with
           | AssignE(id, e) ->
               let* v = evalExpr e rho1 in
               pure ({conf with is = fs @ [AssignV(id, v)] @ ls}, None :: obs)
           | Guard(e, b, cls, p) ->
               let* b1 = evalExpr e rho1 >>= checkBoolean in
               if b1 == b then
                 pure ({conf with is = fs @ [Nop] @ ls}, None :: obs)
               else
                 pure ({conf with is = fs @ [Nop]; cs = cls}, Rollback(p) :: obs)
           | Load(x, l, e) ->
               if List.exists (fun i -> (isStore i) || (isFence i)) fs then
                 err (InvalidDirective (Exec n))
               else
                 let* n = evalExpr e rho1 >>= checkInteger in
                 let i' = AssignV(x, CstI(conf.mu.(n))) in
                 pure ({conf with is = fs @ [i'] @ ls}, Read(n, pending fs) :: obs)
           | StoreE(e1, e2) ->
               if List.exists isFence fs then
                 err (InvalidDirective (Exec n))
               else
                 let* n = evalExpr e1 rho1 >>= checkInteger in
                 let* v = evalExpr e2 rho1 >>= checkInteger in
                 pure ({conf with is = fs @ [StoreV(n, v)] @ ls}, Write(n, pending fs) :: obs)
           | IProtectE(x, p, e) ->
               let* v = evalExpr e rho1 in
               pure ({conf with is = fs @ [IProtectV(x, v)] @ ls}, None :: obs)
           | IProtectV(x, v) ->
               if List.exists isGuard fs then
                 err (InvalidDirective (Exec n))
               else
                 pure ({conf with is = fs @ [AssignV(x, v)] @ ls}, None :: obs)
           | _ -> err (InvalidDirective (Exec n)));;

let stepRetire (conf: configuration) (obs : observation list) : (configuration * observation list) vmresult =
  match conf.is with
    | Nop :: is           -> pure ({conf with is = is}, None :: obs)
    | AssignV(x, v) :: is -> pure ({conf with is = is; rho = StringMap.add x v conf.rho}, None :: obs)
    | StoreV(n, v) :: is  -> conf.mu.(n) <- v;
                             pure ({conf with is = is}, None :: obs)
    | IFail(p) :: is       -> pure ({conf with is = []; cs = []}, OFail p :: obs)
    | _ -> err (InvalidDirective Retire);;

let step (conf: configuration) (dir : directive) (obs : observation list) (cost : directive -> configuration -> int) (count : int) : (configuration * observation list * int) vmresult =
  let updateCost dir = Result.map (fun (cs, obs) -> cs, obs, count + cost dir conf) in
    match conf.is, conf.cs, dir with
      | [], [], _       -> err EndOfStream
      | _, _, Fetch     -> updateCost Fetch (stepFetch conf obs)
      | _, _, PFetch(b) -> updateCost (PFetch b) (stepPFetch conf obs b)
      | _, _, Exec(n)   -> updateCost (Exec n) (stepExec conf obs n)
      | _, _, Retire    -> updateCost Retire (stepRetire conf obs);;

let eval' (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) (injector : (configuration * observation list * int) -> unit) : (configuration * observation list * int) vmresult =
  let module S = (val speculator : Speculator) in
  let module C = (val model : CostModel) in
  let rec helper conf speculator obs count =
    match step conf (S.speculate conf obs) obs C.cost count with
      | Ok (conf', obs', count') -> injector(conf', obs', count');
                                    helper conf' speculator obs' count'
      | Error EndOfStream -> pure (conf, obs, count)
      | Error e -> err e
   in helper conf speculator [] 0;;

let eval (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) : (configuration * observation list * int) vmresult =
  eval' conf speculator model (fun _ -> ())

let evalWithTrace (out : out_channel) (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) : (configuration * observation list * int) vmresult =
  eval' conf speculator model (fun r -> output_string out (string_of_output r))

let evalList' (conf : configuration) (speculator : directive list) (injector : (configuration * observation list * int) -> unit) (cost : directive -> configuration -> int) : (configuration * observation list * int) vmresult=
  let rec helper conf speculator obs count =
    match speculator with
      | [] -> pure (conf, obs, count)
      | d :: ds -> (match step conf d obs cost count with
                      | Ok (conf', obs', count') ->
                              injector(conf', obs', count');
                              helper conf' ds obs' count'
                      | Error EndOfStream -> pure (conf, obs, count)
                      | Error e -> err e)
   in helper conf speculator [] 0;;

let evalList (conf : configuration) (speculator : directive list) (cost : directive -> configuration -> int) : (configuration * observation list * int) vmresult =
  evalList' conf speculator (fun _ -> ()) cost;;

(** Simple implementation of the speculator
    where the PFetch directive is returned with a value obtained from
    a pseudorandom distribution when possible,
    otherwise the Retire directive is returned when possible,
    otherwise the Exec(0) directive is returned when possible,
    otherwise the Fetch directive is returned **)
let defaultSpeculator (dist : unit -> bool) : (module Speculator) =
  (module struct
     let speculate conf obs =
       match conf.is, conf.cs with
         | _, If(e, c1, c2) :: _ -> PFetch (dist ())
         | Nop :: _, _           -> Retire
         | AssignV(_, _) :: _, _ -> Retire
         | StoreV(_, _) :: _, _  -> Retire
         | IFail(_) :: _, _      -> Retire
         | _ :: _, _             -> Exec 0
         | _, _                  -> Fetch;;
  end)

let outOfOrderSpeculator (verbose : bool) : (module Speculator) =
  (module struct
     let speculate conf obs =
       let ds = if canFetch conf then [Fetch] else [] in
       let ds = if canPFetch conf then PFetch (Random.bool ()) :: ds else ds in
       let ds = if canRetire conf then Retire :: ds else ds in
       let execs = List.mapi (fun i _ -> i) conf.is |> List.filter (canEval conf) |> List.map (fun i -> Exec(i)) in
       let ds = execs @ ds in
       let len = List.length ds in
       if len = 0 then
         Fetch
       else
         let idx = Random.int len in
         if verbose then Printf.printf "[%s] -> %d\n" (String.concat "; " (List.map string_of_directive ds)) idx;
         List.nth ds idx
  end)

(** Simple implementation of the cost model
    where every instruction has the same cost
    except for the fetching of Seq **)
module UniformCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq (_, _) :: _; mu; rho} -> 0
      | _, _ -> 1
end;;

(** Simple implementation of the cost model
    where the protect instruction has a higher cost
    when implemented with a fence **)
module FenceSensitiveCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq (_, _) :: _; mu; rho} -> 0
      | Exec(n), {is; cs; mu; rho} ->
              (match splitIs is n with
                | Ok (_, IProtectE(_, Fence, _), _) -> 5
                | _ -> 1)
      | _, _ -> 1
end;;

(** Simple implementation of the cost model
    where the protect instruction has a higher cost
    when implemented with a fence
    and the instructions operating on the memory(Load/Store)
    have an higher cost than the others **)
module SimpleCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq (_, _) :: _; mu; rho} -> 0
      | Fetch, _ -> 2
      | PFetch(_), _ -> 1
      | Exec(n), {is; cs; mu; rho}  ->
          (match splitIs is n with
            | Ok (_, IProtectE(_, Fence, _), _) -> 5
            | Ok (_, Load _, _) -> 10
            | _ -> 1)
      | Retire, {is = i :: _; cs; mu; rho} ->
          (match i with
            | Nop -> 0
            | StoreV _ -> 10
            | _ -> 1)
      | _, _ -> 1
end;;

(** Simple implementation of the cost model
    where the cost of the fence is determined by
    the amount of instruction "blocked" by the fence **)
module FenceSpeculativeCost : CostModel = struct
  let n_proc = 1
  let mem_ist_cost = 10

  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq (_, _) :: _; mu; rho} -> 0
      | Fetch, {is; cs = Protect (_, Fence, _) :: _; mu; rho} ->
          let c = countMemIstruction is in
          let n = List.length is - c in
          max 0 (c * mem_ist_cost - (n / n_proc))
      | Fetch, _ -> 2
      | PFetch(_), _ -> 1
      | Exec(n), {is; cs; mu; rho}  ->
          (match splitIs is n with
            | Ok (_, Load _, _) -> 10
            | _ -> 1)
      | Retire, {is = i :: _; cs; mu; rho} ->
          (match i with
            | Nop -> 0
            | StoreV _ -> 10
            | _ -> 1)
      | _, _ -> 1
end;;

let defaultConfiguration (c : cmd) (size : int) : configuration =
    {is = []; cs = [c]; mu = Array.make size 0; rho = StringMap.empty};;
