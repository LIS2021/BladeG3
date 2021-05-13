open Ast;;

module L = Llvm;;

module VarMap = Map.Make(String);;

let context = L.global_context ();;

let void_t = L.void_type context;;
let i32_t = L.i32_type context;;
let i64_t = L.i64_type context;;
let fmain_t = L.function_type void_t [||];;
let ffail_t = L.function_type void_t [| |];;
(*
let fstarttime_t = L.function_type i64_t [||];;
let fendtime_t = L.function_type void_t [| i64_t |];;
*)

let i32_const (i : int) = L.const_int i32_t i;;

let lmodule = L.create_module context "BLADEVM";;

let fmain = L.define_function "main" fmain_t lmodule;;
let fphony_fence = L.declare_function "phony_fence" fmain_t lmodule;;
let ffail = L.declare_function "fail" ffail_t lmodule;;
(*
let fstarttime = L.declare_function "starttime" fstarttime_t lmodule;;
let fendtime = L.declare_function "endtime" fendtime_t lmodule;;
*)

let print_mu (mu : L.llvalue) (dim : int) (builder : L.llbuilder) : L.llbuilder =
  let mu_t = L.type_of mu in
  let fprintmu_t = L.function_type void_t [| mu_t; i32_t |] in
  let fprintmu = L.declare_function "printmu" fprintmu_t lmodule in
  let _ = L.build_call fprintmu [| mu; (i32_const dim) |] "" builder in
  builder;;

let print_var (rho : L.llvalue VarMap.t) (builder : L.llbuilder) : L.llbuilder =
  let max_id = VarMap.fold (fun k v it -> let ln = String.length k in (if ln > it then ln else it)) rho 0 in
  let idarr = L.build_array_alloca i32_t (i32_const max_id) "idarr" builder in
  let idarr_t = L.type_of idarr in
  let fprintvar_t = L.function_type void_t [| idarr_t; i32_t; i32_t |] in
  let fprintvar = L.declare_function "printvar" fprintvar_t lmodule in
  let store_char (i : int) (c : int) : unit =
    let gep = L.build_gep idarr [| i32_const i |] "ptr" builder in
    L.build_store (i32_const c) gep builder |> ignore in
  let var_iterator (id : string) (lval : L.llvalue) : unit =
    let ch = id |> String.to_seq |> List.of_seq in
    let ch' = List.map (fun c -> Char.code c) ch in
    List.iteri store_char ch';
    let var = L.build_load lval id builder in
    L.build_call fprintvar [| idarr; (i32_const (String.length id)); var |] "" builder |> ignore in
  VarMap.iter var_iterator rho;
  builder;; 

type injector = {
    phony : string ;
    repl : string ;
};;

let inject_fence (ir : string) (inj : injector) : string =
  let irl = ir |> String.to_seq |> List.of_seq in
  let phonyl = inj.phony |> String.to_seq |> List.of_seq in
  let repll = inj.repl |> String.to_seq |> List.of_seq in
  let rec check_match (irl : char list) (phonyl : char list) (prev : char list) : (char list * char list) =
    match irl, phonyl with
      | i :: irl', p :: phonyl' ->
          if i = p then check_match irl' phonyl' (prev @ [i])
          else [List.hd prev], (List.tl prev) @ irl
      | _, [] -> repll, irl
      | [], p :: _ -> prev, [] in
  let rec helper (irl : char list) (phonyl : char list) : char list = 
    match irl, phonyl with
      | i :: irl', p :: phonyl' -> 
          if i = p then 
            let prev, cont = check_match irl' phonyl' [i] in
            prev @ (helper cont phonyl)
          else i :: helper irl' phonyl
      | [], _ -> []
      | i :: irl', [] -> irl in
  helper irl phonyl |> List.to_seq |> String.of_seq;;

type llerror = 
  | InvalidType of string
  | UndefinedVariable of string;;

let string_of_llerror = function
  | InvalidType t -> Printf.sprintf "espression expected of type %s." t
  | UndefinedVariable x -> Printf.sprintf "variable %s is undefined." x;;


type 'a llresult = ('a, llerror) result;;

let pure x = Ok x;;
let err e = Error e;;
let (let*) = Result.bind;;

let check_declared_var (rho : L.llvalue VarMap.t) (id : string) : L.llvalue llresult =
  match VarMap.find_opt id rho with
    | Some(lval) -> pure lval
    | _ -> err(UndefinedVariable id);;

let check_array (e : expr) : arr llresult = 
  match e with
    | Cst(CstA(a)) -> pure a
    | _ -> err(InvalidType "array");;


let rec build_expr (rho : L.llvalue VarMap.t) (mu : L.llvalue) (builder : L.llbuilder) (e : expr) : (L.llvalue * L.llbuilder) llresult = 
  match e with
    | Cst(CstI(i)) -> pure(i32_const i, builder)
    | Var(x) -> 
        let* lval = check_declared_var rho x in
        pure(L.build_load lval x builder, builder)
    | BinOp(e1, e2, op) ->
        let* ve1, builder = build_expr rho mu builder e1 in
        let* ve2, builder = build_expr rho mu builder e2 in
        let lval = (match op with
          | Add -> L.build_add ve1 ve2 "add" builder
          | BitAnd -> L.build_and ve1 ve2 "bitand" builder
          | Lte -> 
              let bv = L.build_icmp L.Icmp.Sle ve1 ve2 "le" builder in
              L.build_zext bv i32_t "ext" builder
          | Lt -> 
              let bv = L.build_icmp L.Icmp.Slt ve1 ve2 "lt" builder in
              L.build_zext bv i32_t "ext" builder) in
        pure(lval, builder)
    | InlineIf(e1, e2, e3) ->
        let* ve1, builder = build_expr rho mu builder e1 in
        let nbit1 = L.build_add (i32_const (-1)) ve1 "add" builder in
        let bit1 = L.build_xor nbit1 (i32_const (-1)) "xor" builder in
        let* ve2, builder = build_expr rho mu builder e2 in
        let* ve3, builder = build_expr rho mu builder e3 in
        let tbr = L.build_and ve2 bit1 "and" builder in
        let fbr = L.build_and ve3 nbit1 "and" builder in
        pure(L.build_add tbr fbr "add" builder, builder)
    | Base(e) -> 
        let* a = check_array e in
        pure(i32_const a.base, builder)
    | Length(e) -> 
        let* a = check_array e in
        pure(i32_const a.length, builder)
    | _ -> failwith "syntax error";;


let build_rhs (rho : L.llvalue VarMap.t) (mu : L.llvalue) (builder : L.llbuilder) (r : rhs) : (L.llvalue * L.llbuilder) llresult = 
  match r with
    | Expr(e) -> build_expr rho mu builder e
    | PtrRead(e, _) ->
        let* ve, builder = build_expr rho mu builder e in
        let ptr = L.build_gep mu [| ve |] "eptr" builder in
        let vptr = L.build_load ptr "ve" builder in
        pure(vptr, builder)
    | ArrayRead(a, e1) -> 
        let bthen = L.append_block context "then" fmain in
        let belse = L.append_block context "else" fmain in
        let bcont = L.append_block context "cont" fmain in
        (* condition *)
        let* ve1, builder = build_expr rho mu builder e1 in
        let be = L.build_icmp L.Icmp.Slt ve1 (i32_const a.length) "be" builder in
        let _ = L.build_cond_br be bthen belse builder in
        (* then *)
        let builder_then = L.builder_at_end context bthen in
        let ve' = L.build_add (i32_const a.base) ve1 "vep" builder_then in
        let ptr = L.build_gep mu [| ve' |] "epptr" builder_then in
        let vrt = L.build_load ptr "vrt" builder_then in 
        let _ = L.build_br bcont builder_then in
        (* else *)
        let builder_else = L.builder_at_end context belse in
        let _ = L.build_call ffail [| |] "" builder_else in
        let vrf = i32_const 0 in
        let _ = L.build_br bcont builder_else in
        (* cont. *)
        let builder_cont = L.builder_at_end context bcont in
        let phi = L.build_phi [(vrt, bthen); (vrf, belse)] "phi" builder_cont in
        pure(phi, builder_cont);;

let rec build_cmd (rho : L.llvalue VarMap.t) (mu : L.llvalue) (builder : L.llbuilder) (c : cmd) : L.llbuilder llresult =
  match c with
    | Skip -> pure builder
    | Fail -> let _ = L.build_call ffail [| |] "" builder in pure builder
    | VarAssign(id, rhs) ->
        let* vrhs, builder = build_rhs rho mu builder rhs in
        let* lval' = check_declared_var rho id in
        let _ = L.build_store vrhs lval' builder in
        pure(builder)
    | PtrAssign(e1, e2, _) -> 
        let* ve1, builder = build_expr rho mu builder e1 in
        let* ve2, builder = build_expr rho mu builder e2 in
        let ptr = L.build_gep mu [| ve1 |] "eptr" builder in
        let _ = L.build_store ve2 ptr builder in
        pure(builder)
    | ArrAssign(a, e1, e2) ->
        let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
        let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
        let c' = If(e, (PtrAssign(e', e2, a.label)), Fail) in
        build_cmd rho mu builder c'
    | Seq(c1, c2) ->
        let* builder = build_cmd rho mu builder c1 in
        build_cmd rho mu builder c2
    | If(e, c1, c2) ->
        let* ve, builder = build_expr rho mu builder e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) "ne" builder in
        let bthen = L.append_block context "then" fmain in
        let belse = L.append_block context "else" fmain in
        let bcont = L.append_block context "cont" fmain in
        let _ = L.build_cond_br be bthen belse builder in
        let builder_then = L.builder_at_end context bthen in
        let* builder_then = build_cmd rho mu builder_then c1 in
        let _ = L.build_br bcont builder_then in
        let builder_else = L.builder_at_end context belse in
        let* builder_else = build_cmd rho mu builder_else c2 in
        let _ = L.build_br bcont builder_else in
        pure(L.builder_at_end context bcont)
    | While(e, c1) ->
        let bcond = L.append_block context "cond" fmain in
        let bwhile = L.append_block context "while" fmain in
        let bfoll = L.append_block context "foll" fmain in
        let _ = L.build_br bcond builder in
        let builder_cond = L.builder_at_end context bcond in
        let* ve, builder_cond = build_expr rho mu builder_cond e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) "ne" builder_cond in
        let _ = L.build_cond_br be bwhile bfoll builder_cond in
        let builder_while = L.builder_at_end context bwhile in
        let* builder_while = build_cmd rho mu builder_while c1 in
        let _ = L.build_br bcond builder_while in
        pure(L.builder_at_end context bfoll)
    | Protect(id, Slh, ArrayRead(a, e)) ->
        let* ve, builder = build_expr rho mu builder e in
        let be1 = L.build_icmp L.Icmp.Slt ve (i32_const a.length) "be1" builder in
        let nbe1 = L.build_not be1 "nbe1" builder in
        let ve1 = L.build_zext nbe1 i32_t "ve1" builder in
        let vr = L.build_add (i32_const (-1)) ve1 "r" builder in
        let ve2 = L.build_add (i32_const a.base) ve "ve2" builder in
        let masked = L.build_and vr ve2 "masked" builder in
        let* lval' = check_declared_var rho id in
        let ptr = L.build_gep mu [| masked |] "maskedptr" builder in
        let vmasked = L.build_load ptr "vmasked" builder in
        let _ = L.build_store vmasked lval' builder in
        Printf.printf "slh\n";
        pure(builder)
    | Protect(id, _, rhs) -> 
        let* vrhs, builder = build_rhs rho mu builder rhs in
        let _ = L.build_call fphony_fence [||] "" builder in
        let* lval' = check_declared_var rho id in
        let _ = L.build_store vrhs lval' builder in
        pure(builder);;

let build_decl (builder : L.llbuilder) (c : cmd) : (L.llvalue VarMap.t * L.llbuilder * int) =
  let rec helper (rho : L.llvalue VarMap.t) (mud : int) (builder : L.llbuilder) (c : cmd) : (L.llvalue VarMap.t * L.llbuilder * int) =
    match c with 
      | VarAssign(id, r)
      | Protect(id, _, r) -> 
          let rho', builder' = (match VarMap.find_opt id rho with
            | Some(_) -> rho, builder
            | None ->
                let lval = L.build_alloca i32_t id builder in
                VarMap.add id lval rho, builder) in
          let mud' = (match r with
            | ArrayRead(a, e) -> a.length + a.base
            | _ -> mud) in
          rho', builder', (if mud' > mud then mud' else mud)
      | ArrAssign(a, _, _) -> 
          let mud' = a.length + a.base in
          rho, builder, (if mud' > mud then mud' else mud)
      | Seq(c1, c2)
      | If(_, c1, c2) ->
          let rho', builder', mud' = helper rho mud builder c1 in
          helper rho' mud' builder' c2
      | While(_, c1) -> helper rho mud builder c1
      | _ -> rho, builder, mud in
  helper VarMap.empty 0 builder c;;

let build_mu (dim : int) (builder : L.llbuilder) : L.llvalue =
  let store_zero (mu : L.llvalue) (i : int) (z : int) : unit =
    let gep = L.build_gep mu [| i32_const i |] "ptr" builder in
    L.build_store (i32_const z) gep builder |> ignore in
  let mu = L.build_array_alloca i32_t (i32_const dim) "mu" builder in
  let ls_of_zero = List.init dim (fun _ -> 0) in
  List.iteri (store_zero mu) ls_of_zero;
  mu

let build_ir (verbose : bool) (ifence : bool) (ast : cmd) : string llresult =
  let fence_injector =
    { phony = "call void @phony_fence()";
      repl = "fence seq_cst"; } in
  let builder = L.builder_at_end context (L.entry_block fmain) in
  let rho, builder, mud = build_decl builder ast in
  let mu = build_mu mud builder in
  let* builder = build_cmd rho mu builder ast in
  let builder = (if verbose then print_mu mu mud builder else builder) in
  let builder = (if verbose then print_var rho builder else builder) in
  let _ = L.build_ret_void builder in
  let ir = L.string_of_llmodule lmodule in
  pure(if ifence then inject_fence ir fence_injector else ir);;
 
