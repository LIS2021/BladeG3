
let conf = {
    is=[]; 
    cs=[
        Seq(
            VarAssign("x", Expr(Cst(CstI(0)))), 
            VarAssign("y", Expr(BinOp(Var("x"), Cst(CstI(1)), Add)))
        )
    ]; 
    mu=[||]; 
    rho=StringMap.empty
};;

let dirs = [Fetch; Fetch; Fetch; Exec 0; Retire; Exec 0; Retire];;


evalListPrint conf dirs;;

