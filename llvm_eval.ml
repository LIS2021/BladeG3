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


let build_expr (rho : vat_ht) (mu : L.llvalue) (builder : L.llbuilder) (e : expr) : (L.llvalue * L.llbuilder) =
  match e with
    | Cst(CstI(i)) -> i32_const i
    | Var(x) -> 
        (match vat_ht.find_opt x rho with
          | Some(lval) -> L.build_load lval x builder, builder
          | None -> failwith "syntax error")
    | BinOp(e1, e2, op) ->
        let ve1, builder = build_expr rho mu builder e1 in
        let ve2, builder = build_expr rho mu builder e2 in
        (match op with
          | Add -> L.build_add ve1 ve2 (get_tmp_addr ()) builder, builder
          | BitAnd -> L.build_and ve1 ve2 (get_tmp_addr ()) builder, builder
          | Let -> 
              let bv = L.build L.Icmp.Sle ve1 ve2 (get_tmp_addr ()) builder in
              L.build_zext bv i32_t (get_tmp_addr ()) builder, builder
          | Lt -> 
              let bv = L.build L.Icmp.Slt ve1 ve2 (get_tmp_addr ()) builder in
              L.build_zext bv i32_t (get_tmp_addr ()) builder, builder)
    | InlineIf(e1, e2, e3) ->
        let ve1, builder = build_expr rho mu builder e1 in
        let nbit1 = L.build_add (i32_const -1) ve1 (get_tmp_addr ()) builder in              (* ???????? *)
        let bit1 = L.build_xor bit1 (i32_const -1) (get_tmp_addr ()) builder in
        let ve2 = build_expr rho mu builder e2 in
        let ve3 = build_expr rho mu builder e3 in
        let tbr = L.build_and ve2 bit1 (get_tmp_addr ()) builder in
        let fbr = L.build_and ve3 nbit1 (get_tmp_addr ()) builder in
        L.build_add tbr fbr (get_tmp_addr ()) builder, builder
    | Base(e) -> 
        (match e with
          | CstA(a) -> i32_const a.base, builder
          | _ -> failwith "syntax error")
    | Length(e) -> 
        (match e with
          | CstA(a) -> i32_const a.length, builder
          | _ -> failwith "syntax error")
    | _ -> failwith "syntax error";;


let build_rhs (rho : vat_ht) (mu : L.llvalue) (builder : L.llbuilder) (r : rhs) : (L.llvalue * L.llbuilder) =
  match r with
    | Expr(e) -> build_expr rho mu builder e
    | PtrRead(e, _) ->
        let ve = build_expr rho mu builder e in
        let ptr = L.build_gep mu [| ve |] (get_tmp_addr ()) builder in
        let vptr = L.build_load ptr (get_tmp_addr ()) builder in
        vptr, builder
    | _ -> failwith "not reachable";;


let build_cmd (rho : var_ht) (mu : L.llvalue) (builder : L.llbuilder) (c : cmd) : L.llbuilder =
  match c with
    | Skip -> builder
    | Fail -> let _ = L.build_ret_void builder in builder
    | VarAssign(id, rhs) ->
        (match rhs with
          | ArrayRead(a, e1, e2) ->
              let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
              let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
              let c' = If(e, (VarAssign(id, PtrRead(e', a.label))), Fail) in
              build_cmd rho mu builder c'
          | Expr _
          | PtrRead _ ->
              let vrhs, builder = build_rhs rho mu builder rhs in
              (match var_ht.find_opt id rho with
                | Some(lval) -> 
                    let _ = L.build_store vrhs lval builder in
                    builder
                | None ->
                    let lval = L.build_alloca i32_t id builder in
                    let _ = L.build_store vrhs lval builder in
                    var_ht.add id lval;
                    builder))
    | PtrAssign(e1, e2, _) -> 
        let ve1, builder = build_expr rho mu builder e1 in
        let ve2, builder = build_expr rho mu builder e2 in
        let ptr = L.build_gep mu [| ve1 |] (get_tmp_addr ()) builder in
        let _ = L.build_store ve2 ptr builder in
        builder
    | ArrAssign(a, e1, e2) ->
        let e = BinOp(e1, Length(Cst(CstA(a))), Lt) in
        let e' = BinOp(Base(Cst(CstA(a))), e1, Add) in
        let c' = If(e, (PtrAssign(e', e2, a.label)), Fail) in
        build_cmd rho mu builder c'
    | Seq(c1, c2) ->
        let builder = build_cmd rho mu builder c1 in
        builder_cmd rho mu builder c2
    | If(e, c1, c2) ->
        let ve, builder = build_expre rho mu builder e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) (get_tmp_addr ()) builder in
        let bthen = L.append_block context "then" fmain in
        let belse = L.append_block context "else" fmain in
        let bcont = L.append_block context "cont" fmain in
        let _ = L.build_cond_br be bthen belse builder in
        let builder_then = L.builder_at_end context bthen in
        let builder_then = build_command rho mu builder_then c1 in
        let _ = L.build_br bcont builder_then in
        let builder_else = L.builder_at_end context belse in
        let builder_else = build_command rho mu builder_else c2 in
        let _ = L.build_br bcont builder_else in
        L.builder_at_end context bcont
    | While(e, c1) ->
        let ve, builder = build_expre rho mu builder e in
        let be = L.build_icmp L.Icmp.Ne ve (i32_const 0) (get_tmp_addr ()) builder in
        let bcond = L.append_block context "cond" fmain in
        let bwhile = L.append_block context "while" fmain in
        let bfoll = L.append_block context "foll" fmain in
        let _ = L.build_br be bcond builder in
        let builder_cond = L.builder_at_end context bcond in
        let _ = L.build_cond_br be bwhile bfoll builder_cond in
        let builder_while = L.builder_at_end context bwhile in
        let builder_while = build_command rho mu builder_while c1 in
        let _ = L.build_br bcond builder_while in
        L.builder_at_end context bfoll
    | _ -> failwith "Not implemented";;

let () =
  let builder = L.builder_at_end context (L.entry_block fmain) in

  Printf.printf "%s\n" (L.string_of_llmodule lmodule);
  L.dispose_module lmodule
