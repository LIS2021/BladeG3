
let a1 = { base=0 ; length=4 ; label=() };;
let b1 = { base=10 ; length=5 ; label=() };;
let conf = {
  is=[];
  cs=[
      Seq( ArrAssign(a1, Cst(CstI(0)), Cst(CstI(1))),
      Seq( ArrAssign(a1, Cst(CstI(1)), Cst(CstI(2))),
      Seq( ArrAssign(b1, Cst(CstI(3)), Cst(CstI(42))),
      Seq( VarAssign("x", ArrayRead(a1, Cst(CstI(0)))),
      Seq( VarAssign("y", ArrayRead(a1, Cst(CstI(1)))),
      Seq( VarAssign("z", Expr(BinOp(Var("x"), Var("y"), Add))),
           VarAssign("w", ArrayRead(b1, Var("z")))
      ))))))
  ];
  mu= Array.make 20 0;
  rho=StringMap.empty
};;

let dirs = [
    Fetch; Fetch; PFetch(true); Fetch;
    Fetch; Fetch; PFetch(true); Fetch;
    Fetch; Fetch; PFetch(true); Fetch;
    Exec 0; Retire; Exec 0; Retire; Exec 0; Retire;
    Exec 0; Retire; Exec 0; Retire; Exec 0; Retire;
    Fetch; Fetch; PFetch(true); Fetch;
    Exec 0; Retire; Exec 0; Retire;
    Fetch; Fetch; PFetch(true); Fetch;
    Exec 0; Retire; Exec 0; Retire;
    Fetch; Fetch;
    Exec 0; Retire;
    Fetch; PFetch(true); Fetch;
    Exec 0; Retire; Exec 0; Retire;


  ];;

(* evalListFile conf dirs;; *)

(* EXAMPLES *)
(* step conf Fetch [] 0;; *)
(* eval conf (fun _ -> Fetch);; *)
(* evalList conf [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];; *)


