open Ast;;

(**		CONFIGURATIONS 		**)
type configuration = {
	is  : instruction list;
	cs  : cmd list;
	mu  : int array;
	rho : value StringMap.t;
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
  "count: " ^ (string_of_int count) ^ "\n";;

(** Module modeling the template of a cost model **)
module type CostModel = sig
  (** Given a directive and a configuration
      returns the cost the instruction ran **)
  val cost : directive -> configuration -> int;;
end;;

(** Module modeling the template of a speculator **)
module type Speculator = sig
  (** Given a configuration and a list of observations
      returns a possible next directive **)
  val speculate : configuration -> observation list -> directive;;
end;;

type vmerror =
  | EndOfStream
  | InvalidDirective of directive
  | InvalidType
  | InvalidGuardType
  | InstructionOutOfRange
  | UnassignedReference of string
  | InvalidOperandType of string * string;;

let make_eof = EndOfStream;;
let make_invalid_directive d = InvalidDirective(d);;
let make_invalid_type = InvalidType;;
let make_invalid_guard_type = InvalidGuardType;;
let make_instruction_out_of_range = InstructionOutOfRange;;
let make_unassigned_reference s = UnassignedReference(s);;
let make_invalid_operand_type s1 s2 = InvalidOperandType(s1, s2);;

let string_of_vmerror = function
  | EndOfStream -> "no more instructions or command available"
  | InvalidDirective d -> Printf.sprintf "invalid %s directive" (string_of_directive d)
  | InvalidType -> "invalid type"
  | InvalidGuardType -> "invalid type for guard"
  | InstructionOutOfRange -> "instruction out of range"
  | UnassignedReference s -> Printf.sprintf "identifier %s has not been initialized yet" s
  | InvalidOperandType (s1, s2) ->
      Printf.sprintf "invalid operand type: expected %s, got %s" s1 s2;;

type 'a vmresult = ('a, vmerror) result;;

let pure x = Ok x;;
let err e = Error e;;
let (let+) x f = Result.map f x;;
let (and+) r1 r2 =
  match r1, r2 with
    | Ok x, Ok y       -> pure (x, y)
    | Error e, Ok _    -> err e
    | Ok _, Error e    -> err e
    | Error e, Error _ -> err e;;
let (let*) = Result.bind;;
let (>>=) = Result.bind;;

let rollback_count = ref 0;;
let fresh_rollback_id () = incr rollback_count; !rollback_count;;

(** Given a list and an integer
    returns the list of items before the item with the given index
    the item with the given index
    and the list of items following that item **)
let split_is (ls : 'a list) (n : int) : ('a list * 'a * 'a list) vmresult =
  let rec split_is_rec fls n = function
    | [] -> err make_instruction_out_of_range
    | a :: cls ->
        if n == 0 then
          pure (fls, a, cls)
        else if n > 0 then
          split_is_rec (fls @ [a]) (n - 1) cls
        else
          err make_instruction_out_of_range in
  split_is_rec [] n ls;;

let rec count_mem_instruction (is: instruction list) : int =
  match is with
    | StoreE(_, _)  :: is'
    | StoreV(_, _)  :: is'
    | Load(_, _, _) :: is' -> 1 + count_mem_instruction is'
    | _ :: is' -> count_mem_instruction is'
    | _ -> 0;;

let is_store (i : instruction) : bool =
  match i with
    | StoreE(_, _) -> true
    | StoreV(_, _) -> true
    | _            -> false;;

let is_guard (i : instruction) : bool =
  match i with
    | Guard(_, _, _, _) -> true
    | _                 -> false;;

let is_fence (i : instruction) : bool =
  match i with
      | IProtectE(_, Fence, _) -> true
      | _                      -> false;;

let fresh_name (name : string) (rho : value StringMap.t) : string =
  let rec helper name rho counter =
    if StringMap.mem (name ^ (string_of_int counter)) rho then
      helper name rho (counter + 1)
    else
      name ^ (string_of_int counter) in
  helper name rho 0;;

let check_integer (v : value) : int vmresult =
  match v with
    | CstI(n) -> pure n
    | CstB(b) -> pure (Bool.to_int b)
    | _       -> err (make_invalid_operand_type "integer" "array");;

let check_boolean (v : value) : bool vmresult =
  match v with
    | CstB(b) -> pure b
    | CstI(i) -> pure (i != 0)
    | _       -> err (make_invalid_operand_type "boolean" "array");;

let check_array (v : value) : arr vmresult =
  match v with
    | CstA(a) -> pure a
    | CstI(_) -> err (make_invalid_operand_type "array" "integer")
    | CstB(_) -> err (make_invalid_operand_type "array" "boolean");;

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
    | IFail(p) :: is           -> p :: pending is
    | i :: is                  -> pending is;;

let rec eval_expr (e : expr) (rho : value StringMap.t) : value vmresult =
  match e with
    | Cst(v) -> pure v
    | Var(x) -> Option.to_result ~none:(make_unassigned_reference x) (StringMap.find_opt x rho)
    | BinOp(e1, e2, Add) ->
        let* n1 = eval_expr e1 rho >>= check_integer
        and+ n2 = eval_expr e2 rho >>= check_integer in
        pure (make_int (n1 + n2))
    | BinOp(e1, e2, Lte) ->
        let* n1 = eval_expr e1 rho >>= check_integer
        and+ n2 = eval_expr e2 rho >>= check_integer in
        pure (make_bool (n1 <= n2))
    | BinOp(e1, e2, Lt) ->
        let* v1 = eval_expr e1 rho >>= check_integer
        and+ v2 = eval_expr e2 rho >>= check_integer in
        pure (make_bool (v1 < v2))
    | BinOp(e1, e2, BitAnd) ->
        let* v1 = eval_expr e1 rho >>= check_integer
        and+ v2 = eval_expr e2 rho >>= check_boolean in
        pure (make_int (if v2 then v1 else 0))
    | InlineIf(e1, e2, e3) ->
        let* b1 = eval_expr e1 rho >>= check_boolean in
        if b1 then eval_expr e2 rho else eval_expr e3 rho
    | Length(e) ->
        let* a = eval_expr e rho >>= check_array in
        pure (make_int a.length)
    | Base(e) ->
        let* a = eval_expr e rho >>= check_array in
        pure (make_int a.base);;

let can_fetch (conf : configuration) : bool =
  match conf.cs with
    | Skip               :: _
    | Fail               :: _
    | VarAssign(_, _)    :: _
    | Seq(_, _)          :: _
    | PtrAssign(_, _, _) :: _
    | ArrAssign(_, _, _) :: _
    | While(_, _)        :: _
    | Protect(_, _, _)   :: _ -> true
    | _                       -> false;;

let can_pfetch (conf : configuration) : bool =
  match conf.cs with
    | If(_, _, _) :: _ -> true
    | _                -> false;;

let can_eval (conf : configuration) (n : int) : bool =
  match split_is conf.is n with
    | Ok (fs, i, ls) ->
        let rho1 = phi conf.rho fs in
        (match i with
           | AssignE(_, e) ->
               (match eval_expr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | Guard(e, _, _, _) ->
               (match eval_expr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | IProtectE(_, _, e) ->
               (match eval_expr e rho1 with
                  | Error (UnassignedReference _) -> false
                  | _ -> true)
           | Load(x, l, e) ->
               if (List.exists (fun i -> is_store i || is_fence i) fs) then
                 false
               else
                 (match eval_expr e rho1 with
                    | Error (UnassignedReference _) -> false
                    | _ -> true)
           | StoreE(e1, e2) ->
               if (List.exists is_fence fs) then
                 false
               else
                 (match eval_expr e1 rho1, eval_expr e2 rho1 with
                    | _, Error (UnassignedReference _) -> false
                    | Error (UnassignedReference _), _ -> false
                    | _, _ -> true)
           | IProtectV(x, v) -> not (List.exists is_guard fs)
           | _ -> false)
    | _ -> false;;

let can_retire (conf : configuration) : bool =
  match conf.is with
    | Nop           :: _
    | AssignV(_, _) :: _
    | StoreV(_, _)  :: _
    | IFail(_)      :: _ -> true
    | _                  -> false;;

let step_fetch (conf: configuration) (obs : observation list) : (configuration * observation list) vmresult =
  match conf.cs with
    | Skip :: cs ->
        pure ({conf with cs = cs}, make_none :: obs)
    | Fail :: cs ->
        let i = make_ifail (fresh_rollback_id ()) in
        pure ({conf with cs = cs; is = conf.is @ [i]}, make_none :: obs)
    | VarAssign(x, Expr(e)) :: cs ->
        pure ({conf with cs = cs; is = conf.is @ [make_assign_e x e]}, make_none :: obs)
    | Seq(c1, c2) :: cs ->
        pure ({conf with cs = c1 :: c2 :: cs}, make_none :: obs)
    | VarAssign(x, PtrRead(e, l)) :: cs ->
        let i = make_load x e in
        pure ({conf with cs = cs; is = conf.is @ [i]}, make_none :: obs)
    | PtrAssign(e1, e2, l) :: cs ->
        let i = make_store_e e1 e2 in
        pure ({conf with cs = cs; is = conf.is @ [i]}, make_none :: obs)
    | VarAssign(x, ArrayRead(a, e1)) :: cs ->
        let e = make_bin_op Lt e1 (make_length (make_cst (make_array a))) in
        let e' = make_bin_op Add (make_base (make_cst (make_array a))) e1 in
        let c' = make_if e (make_var_assign x (make_ptr_read e')) make_fail in
        pure ({conf with cs = c' :: cs}, make_none :: obs)
    | ArrAssign(a, e1, e2) :: cs ->
        let e = make_bin_op Lt e1 (make_length (make_cst (make_array a))) in
        let e' = make_bin_op Add (make_base (make_cst (make_array a))) e1 in
        let c' = make_if e (make_ptr_assign e' e2) make_fail in
        pure ({conf with cs = c' :: cs}, make_none :: obs)
    | While(e, c) :: cs ->
        let c1 = make_seq c (make_while e c) in
        let c2 = make_if e c1 make_skip in
        pure ({conf with cs = c2 :: cs}, make_none :: obs)
    | Protect(x, Slh, ArrayRead(a, e)) :: cs ->
        let mask = "_mask_" ^ x in
        let e1 = make_bin_op Lt e (make_length (make_cst (make_array a))) in
        let e2 = make_bin_op Add (make_base (make_cst (make_array a))) e in
        let c1 = make_var_assign mask (make_expr e1) in
        let c2 = make_var_assign mask (make_expr (make_inline_if (make_var mask)
                                                                 (make_cst (make_bool true))
                                                                 (make_cst (make_bool false)))) in
        let c3 = make_var_assign x (make_ptr_read (make_bin_op BitAnd e2 (make_var mask))) in
        let c' = make_seq c1 (make_if (make_var mask) (make_seq c2 c3) make_fail) in
        pure ({conf with cs = c' :: cs}, make_none :: obs)
    | Protect(x, p, PtrRead(e, l)) :: cs ->
        let x' = fresh_name x conf.rho in
        let c1 = make_var_assign x' (make_ptr_read e) in
        let c2 = make_protect x p (make_expr (make_var x')) in
        pure ({conf with cs = c1 :: c2 :: cs}, make_none :: obs)
    | Protect(x, p, ArrayRead(a, e)) :: cs ->
        let x' = fresh_name x conf.rho in
        let c1 = make_var_assign x' (make_array_read a e) in
        let c2 = make_protect x p (make_expr (make_var x')) in
        pure ({conf with cs = c1 :: c2 :: cs}, make_none :: obs)
    | Protect(x, p, Expr(e)) :: cs ->
        let i = make_iprotect_e x p e in
        pure ({conf with cs = cs; is = conf.is @ [i]}, make_none :: obs)
    | _  -> err (make_invalid_directive make_fetch);;

let step_pfetch (conf: configuration) (obs : observation list) (b : prediction): (configuration * observation list) vmresult =
  match conf.cs, b with
    | If(e, c1, c2) :: cs, true  ->
        let i = make_guard e true (c2 :: cs) (fresh_rollback_id ()) in
        pure ({conf with is = conf.is @ [i]; cs = c1 :: cs}, make_none :: obs)
    | If(e, c1, c2) :: cs, false ->
        let i = make_guard e false (c1 :: cs) (fresh_rollback_id ()) in
        pure ({conf with is = conf.is @ [i]; cs = c2 :: cs}, make_none :: obs)
    | _ -> err (make_invalid_directive (make_pfetch b));;

let step_exec (conf: configuration) (obs : observation list) (n: int): (configuration * observation list) vmresult =
  let* s = split_is conf.is n in
  match s with
    | (fs, i , ls) -> let rho1 = phi conf.rho fs in
        (match i with
           | AssignE(id, e) ->
               let* v = eval_expr e rho1 in
               let i = make_assign_v id v in
               pure ({conf with is = fs @ [i] @ ls}, make_none :: obs)
           | Guard(e, b, cls, p) ->
               let* b1 = eval_expr e rho1 >>= check_boolean in
               if b1 == b then
                 pure ({conf with is = fs @ [make_nop] @ ls}, make_none :: obs)
               else
                 pure ({conf with is = fs @ [make_nop]; cs = cls}, make_rollback p :: obs)
           | Load(x, l, e) ->
               if List.exists (fun i -> (is_store i) || (is_fence i)) fs then
                 err (make_invalid_directive (make_exec n))
               else
                 let* n = eval_expr e rho1 >>= check_integer in
                 let i' = make_assign_v x (make_int conf.mu.(n)) in
                 pure ({conf with is = fs @ [i'] @ ls}, make_read n (pending fs) :: obs)
           | StoreE(e1, e2) ->
               if List.exists is_fence fs then
                 err (make_invalid_directive (make_exec n))
               else
                 let* n = eval_expr e1 rho1 >>= check_integer in
                 let* v = eval_expr e2 rho1 >>= check_integer in
                 let i = make_store_v n v in
                 pure ({conf with is = fs @ [i] @ ls}, make_write n (pending fs) :: obs)
           | IProtectE(x, p, e) ->
               let* v = eval_expr e rho1 in
               let i = make_iprotect_v x v in
               pure ({conf with is = fs @ [i] @ ls}, make_none :: obs)
           | IProtectV(x, v) ->
               if List.exists is_guard fs then
                 err (make_invalid_directive (make_exec n))
               else
                 let i = make_assign_v x v in
                 pure ({conf with is = fs @ [i] @ ls}, make_none :: obs)
           | _ -> err (make_invalid_directive (make_exec n)));;

let step_retire (conf: configuration) (obs : observation list) : (configuration * observation list) vmresult =
  match conf.is with
    | Nop           :: is -> pure ({conf with is = is}, make_none :: obs)
    | AssignV(x, v) :: is -> pure ({conf with is = is; rho = StringMap.add x v conf.rho}, make_none :: obs)
    | StoreV(n, v)  :: is -> conf.mu.(n) <- v;
                             pure ({conf with is = is}, make_none :: obs)
    | IFail(p)      :: is -> pure ({conf with is = []; cs = []}, make_ofail p :: obs)
    | _ -> err (make_invalid_directive make_retire);;

let step (conf: configuration) (dir : directive) (obs : observation list) (cost : directive -> configuration -> int) (count : int) : (configuration * observation list * int) vmresult =
  let update_cost dir = Result.map (fun (cs, obs) -> cs, obs, count + cost dir conf) in
  match conf.is, conf.cs, dir with
    | [], [], _       -> err make_eof
    | _, _, Fetch     -> update_cost make_fetch (step_fetch conf obs)
    | _, _, PFetch(b) -> update_cost (make_pfetch b) (step_pfetch conf obs b)
    | _, _, Exec(n)   -> update_cost (make_exec n) (step_exec conf obs n)
    | _, _, Retire    -> update_cost make_retire (step_retire conf obs);;

let eval' (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) (injector : (configuration * observation list * int) -> unit) : (configuration * observation list * int) vmresult =
  let module S = (val speculator : Speculator) in
  let module C = (val model : CostModel) in
  let rec helper conf speculator obs count =
    match step conf (S.speculate conf obs) obs C.cost count with
      | Ok (conf', obs', count') ->
          injector(conf', obs', count');
          helper conf' speculator obs' count'
      | Error EndOfStream -> pure (conf, obs, count)
      | Error e -> err e in
  helper conf speculator [] 0;;

let eval (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) : (configuration * observation list * int) vmresult =
  eval' conf speculator model (fun _ -> ())

let eval_with_trace (out : out_channel) (conf : configuration) (speculator : (module Speculator)) (model : (module CostModel)) : (configuration * observation list * int) vmresult =
  eval' conf speculator model (fun r -> output_string out (string_of_output r))

let eval_list' (conf : configuration) (speculator : directive list) (injector : (configuration * observation list * int) -> unit) (cost : directive -> configuration -> int) : (configuration * observation list * int) vmresult=
  let rec helper conf speculator obs count =
    match speculator with
      | [] -> pure (conf, obs, count)
      | d :: ds -> (match step conf d obs cost count with
                      | Ok (conf', obs', count') ->
                          injector(conf', obs', count');
                          helper conf' ds obs' count'
                      | Error EndOfStream -> pure (conf, obs, count)
                      | Error e -> err e) in
  helper conf speculator [] 0;;

let eval_list (conf : configuration) (speculator : directive list) (cost : directive -> configuration -> int) : (configuration * observation list * int) vmresult =
  eval_list' conf speculator (fun _ -> ()) cost;;

(** Simple implementation of the speculator
    where the PFetch directive is returned with a value obtained from
    a pseudorandom distribution when possible,
    otherwise the Retire directive is returned when possible,
    otherwise the Exec(0) directive is returned when possible,
    otherwise the Fetch directive is returned **)
let default_speculator (dist : unit -> bool) : (module Speculator) =
  (module struct
     let speculate conf obs =
       match conf.is, conf.cs with
         | _, If(e, c1, c2) :: _ -> make_pfetch (dist ())
         | Nop :: _, _           -> make_retire
         | AssignV(_, _) :: _, _ -> make_retire
         | StoreV(_, _) :: _, _  -> make_retire
         | IFail(_) :: _, _      -> make_retire
         | _ :: _, _             -> make_exec 0
         | _, _                  -> make_fetch;;
  end);;

let out_of_order_speculator (verbose : bool) : (module Speculator) =
  (module struct
     let speculate conf obs =
       let ds = if can_fetch conf then [make_fetch] else [] in
       let ds = if can_pfetch conf then make_pfetch (Random.bool ()) :: ds else ds in
       let ds = if can_retire conf then make_retire :: ds else ds in
       let execs = List.mapi Fun.const conf.is
                     |> List.filter (can_eval conf)
                     |> List.map make_exec in
       let ds = execs @ ds in
       let len = List.length ds in
       if len = 0 then
         make_fetch
       else
         let idx = Random.int len in
         if verbose then
           Printf.printf "[%s] -> %d\n" (String.concat "; " (List.map string_of_directive ds)) idx;
         List.nth ds idx;;
  end);;

(** Simple implementation of the cost model
    where every instruction has the same cost
    except for the fetching of Seq **)
module UniformCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq(_, _) :: _; mu; rho} -> 0
      | _, _ -> 1;;
end;;

(** Simple implementation of the cost model
    where the protect instruction has a higher cost
    when implemented with a fence **)
module FenceSensitiveCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq(_, _) :: _; mu; rho} -> 0
      | Exec(n), {is; cs; mu; rho} ->
          (match split_is is n with
             | Ok (_, IProtectE(_, Fence, _), _) -> 5
             | _ -> 1)
      | _, _ -> 1;;
end;;

(** Simple implementation of the cost model
    where the protect instruction has a higher cost
    when implemented with a fence
    and the instructions operating on the memory(Load/Store)
    have an higher cost than the others **)
module SimpleCost : CostModel = struct
  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq(_, _) :: _; mu; rho} -> 0
      | Fetch, _ -> 2
      | PFetch(_), _ -> 1
      | Exec(n), {is; cs; mu; rho}  ->
          (match split_is is n with
            | Ok (_, IProtectE(_, Fence, _), _) -> 5
            | Ok (_, Load _, _) -> 10
            | _ -> 1)
      | Retire, {is = i :: _; cs; mu; rho} ->
          (match i with
            | Nop -> 0
            | StoreV _ -> 10
            | _ -> 1)
      | _, _ -> 1;;
end;;

(** Simple implementation of the cost model
    where the cost of the fence is determined by
    the amount of instruction "blocked" by the fence **)
module FenceSpeculativeCost : CostModel = struct
  let n_proc = 1;;
  let mem_ist_cost = 10;;

  let cost dir conf =
    match dir, conf with
      | Fetch, {is; cs = Seq(_, _) :: _; mu; rho} -> 0
      | Fetch, {is; cs = Protect(_, Fence, _) :: _; mu; rho} ->
          let c = count_mem_instruction is in
          let n = List.length is - c in
          max 0 (c * mem_ist_cost - (n / n_proc))
      | Fetch, _ -> 2
      | PFetch(_), _ -> 1
      | Exec(n), {is; cs; mu; rho}  ->
          (match split_is is n with
            | Ok (_, Load _, _) -> 10
            | _ -> 1)
      | Retire, {is = i :: _; cs; mu; rho} ->
          (match i with
            | Nop -> 0
            | StoreV _ -> 10
            | _ -> 1)
      | _, _ -> 1;;
end;;

let default_configuration (c : cmd) (size : int) : configuration =
  {is = []; cs = [c]; mu = Array.make size 0; rho = StringMap.empty};;

let dynamic_mu_configuration (c : cmd) : configuration =
  let rec max_mu (dim : int) (c : cmd) : int =
    match c with
      | VarAssign(id, r)
      | Protect(id, _, r) ->
          let dim' = (match r with
            | ArrayRead(a, e) -> a.length + a.base
            | _ -> dim) in
          max dim' dim
      | ArrAssign(a, _, _) ->
          let dim' = a.length + a.base in
          max dim' dim
      | Seq(c1, c2)
      | If(_, c1, c2) ->
          let dim' = max_mu dim c1 in
          max_mu dim' c2
      | While(_, c1) -> max_mu dim c1
      | _ -> dim
   in {is = []; cs = [c]; mu = Array.make (max_mu 0 c) 0; rho = StringMap.empty};;
