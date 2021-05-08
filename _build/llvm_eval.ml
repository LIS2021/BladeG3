open Ast;;

module L = Llvm;;

type var_ht = (identifier, L.llvalue) Hashtbl.t;;

let context = L.global_context ();;

let void_t = L.void_type context;;
let i32_t = L.i32_type context;;
let fmain_t = L.function_type void_t [||];;

let i32_const (i : int) = L.const_int i32_t i;;

let lmodule = L.create_module context "BLADEVM";;

let fmain = L.define_function "main" fmain_t lmodule;;

let get_tmp_addr unit : string = "tmp";;

let builder_try unit : unit =
  let builder = L.builder_at_end context (L.entry_block fmain) in
  let r_x = L.build_alloca i32_t "rx" builder in
  let r_y = L.build_alloca i32_t "ry" builder in
  let r_z = L.build_alloca i32_t "rz" builder in
  let _ = L.build_store (i32_const 3) r_x builder in
  let _ = L.build_store (i32_const 4) r_y builder in
  let x = L.build_load r_x "x" builder in
  let y = L.build_load r_y "y" builder in
  let z = L.build_add x y "z" builder in
  let _ = L.build_store z r_z builder in
  L.build_ret_void builder |> ignore;;


let rec build_expr (rho : var_ht) (mu : L.llvalue) (builder : L.llbuilder) (e : expr) : (L.llvalue * L.llbuilder) =
  match e with
    | Cst(CstI(i)) -> i32_const i, builder
    | Var(x) -> 
        (match Hashtbl.find_opt rho x with
          | Some(lval) -> L.build_load lval x builder, builder
          | None -> failwith "syntax error")
    | BinOp(e1, e2, op) ->
        let ve1, builder = build_expr rho mu builder e1 in
        let ve2, builder = build_expr rho mu builder e2 in
        (match op with
          | Add -> L.build_add ve1 ve2 "add" builder, builder
          | BitAnd -> L.build_and ve1 ve2 "bitand" builder, builder
          | Lte -> 
              let bv = L.build_icmp L.Icmp.Sle ve1 ve2 "le" builder in
              L.build_zext bv i32_t "ext" builder, builder
          | Lt -> 
              let bv = L.build_icmp L.Icmp.Slt ve1 ve2 "lt" builder in
              L.build_zext bv i32_t "ext" builder, builder)
    | InlineIf(e1, e2, e3) ->
        let ve1, builder = build_expr rho mu builder e1 in
        let nbit1 = L.build_add (i32_const (-1)) ve1 "add" builder in              (* ???????? *)
        let bit1 = L.build_xor nbit1 (i32_const (-1)) "xor" builder in
        let ve2, builder = build_expr rho mu builder e2 in
        let ve3, builder = build_expr rho mu builder e3 in
        let tbr = L.build_and ve2 bit1 "and" builder in
        let fbr = L.build_and ve3 nbit1 "and" builder in
        L.build_add tbr fbr "add" builder, builder
    | Base(e) -> 
        (match e with
          | Cst(CstA(a)) -> i32_const a.base, builder
          | _ -> failwith "syntax error")
    | Length(e) -> 
        (match e with
          | Cst(CstA(a)) -> i32_const a.length, builder
          | _ -> failwith "syntax error")
    | _ -> failwith "syntax error";;


let build_rhs (rho : var_ht) (mu : L.llvalue) (builder : L.llbuilder) (r : rhs) : (L.llvalue * L.llbuilder) =
  match r with
    | Expr(e) -> build_expr rho mu builder e
    | PtrRead(e, _) ->
        let ve, builder = build_expr rho mu builder e in
        let ptr = L.build_gep mu [| ve |] "eptr" builder in
        let vptr = L.build_load ptr "ve" builder in
        vptr, builder
    | _ -> failwith "not reachable";;


let rec build_cmd (rho : var_ht) (mu : L.llvalue) (builder : L.llbuilder) (c : cmd) : L.llbuilder =
  match c with
    | Skip -> builder
    | Fail -> let _ = L.build_ret_void builder in builder
    | VarAssign(id, rhs) ->
        (match rhs with
          | ArrayRead(a, e1) ->
              let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
              let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
              let c' = If(e, (VarAssign(id, PtrRead(e', a.label))), Fail) in
              build_cmd rho mu builder c'
          | Expr _
          | PtrRead _ ->
              let vrhs, builder = build_rhs rho mu builder rhs in
              (match Hashtbl.find_opt rho id with
                | Some(lval) -> 
                    let _ = L.build_store vrhs lval builder in
                    builder
                | None ->
                    let lval = L.build_alloca i32_t id builder in
                    let _ = L.build_store vrhs lval builder in
                    Hashtbl.add rho id lval;
                    builder))
    | PtrAssign(e1, e2, _) -> 
        let ve1, builder = build_expr rho mu builder e1 in
        let ve2, builder = build_expr rho mu builder e2 in
        let ptr = L.build_gep mu [| ve1 |] "eptr" builder in
        let _ = L.build_store ve2 ptr builder in
        builder
    | ArrAssign(a, e1, e2) ->
        let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
        let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
        let c' = If(e, (PtrAssign(e', e2, a.label)), Fail) in
        build_cmd rho mu builder c'
    | Seq(c1, c2) ->
        let builder = build_cmd rho mu builder c1 in
        build_cmd rho mu builder c2
    | If(e, c1, c2) ->
        let ve, builder = build_expr rho mu builder e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) "ne" builder in
        let bthen = L.append_block context "then" fmain in
        let belse = L.append_block context "else" fmain in
        let bcont = L.append_block context "cont" fmain in
        let _ = L.build_cond_br be bthen belse builder in
        let builder_then = L.builder_at_end context bthen in
        let builder_then = build_cmd rho mu builder_then c1 in
        let _ = L.build_br bcont builder_then in
        let builder_else = L.builder_at_end context belse in
        let builder_else = build_cmd rho mu builder_else c2 in
        let _ = L.build_br bcont builder_else in
        L.builder_at_end context bcont
    | While(e, c1) ->
        let bcond = L.append_block context "cond" fmain in
        let bwhile = L.append_block context "while" fmain in
        let bfoll = L.append_block context "foll" fmain in
        let _ = L.build_br bcond builder in
        let builder_cond = L.builder_at_end context bcond in
        let ve, builder_cond = build_expr rho mu builder_cond e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) "ne" builder_cond in
        let _ = L.build_cond_br be bwhile bfoll builder_cond in
        let builder_while = L.builder_at_end context bwhile in
        let builder_while = build_cmd rho mu builder_while c1 in
        let _ = L.build_br bcond builder_while in
        L.builder_at_end context bfoll
    | _ -> failwith "Not implemented";;

let usage_msg = "llvm_eval.native <file>";;
let input_file = ref "";;

let () =
  Arg.parse [] (fun s -> input_file := s) usage_msg;
  let in_file = open_in !input_file in
  try
    match Parser.parse_channel in_file with
      | Some(c) ->
          let builder = L.builder_at_end context (L.entry_block fmain) in
          let mu = L.build_array_alloca i32_t (i32_const 100) "mu" builder in
          let rho = Hashtbl.create 10 in
          let builder = build_cmd rho mu builder c in
          let _ = L.build_ret_void builder in
          Printf.printf "%s\n" (L.string_of_llmodule lmodule);
          L.dispose_module lmodule
      | None -> failwith "Parsing error"
  with e ->
    close_in_noerr in_file;
    raise e   

