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
    (* non possono essere valutati tipi diversi dagli interi, i bool escono solo dai binop e
     * gli array possono essere solo argomenti di base e length *)
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
          | _ -> failwith "not implemented")
    | Base(e) -> 
        match e with
          | CstA(a) -> i32_const a.base, builder
          | _ -> failwith "syntax error"
    | Length(e) -> 
        match e with
          | CstA(a) -> i32_const a.length, builder
          | _ -> failwith "syntax error"
    | _ -> failwith "not implemented";;


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
              let c' = If(e, (VarAssign(x, PtrRead(e', a.label))), Fail) in
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
                    HT.add id lval;
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
    | _ -> failwith "Not implemented";;

let () =
  builder_try (); 
  Printf.printf "%s\n" (L.string_of_llmodule lmodule);
  L.dispose_module lmodule
