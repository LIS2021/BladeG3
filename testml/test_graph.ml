open Graph

module G = UseGraph;;

let g = G.empty ();;
let (g, n2) = G.add g;;
let (g, n3) = G.add g;;
let (g, n4) = G.add g;;
let (g, n5) = G.add g;;
let g = G.set_edge g (G.source g, n2) (-1);;
let g = G.set_edge g (G.source g, n4) (-1);;
let g = G.set_edge g (n2, n3) 1;;
let g = G.set_edge g (n4, n5) (-1);;
let g = G.set_edge g (n5, n3) 1;;
let g = G.set_edge g (n3, G.sink g) (-1);;

G.print_graph g;;
let (g, cut) = G.edmonds_karp g;;
G.print_nodes cut;;
G.print_graph g;;

let g = G.empty ();;
let (g, n2) = G.add g;;
let (g, n3) = G.add g;;
let (g, n4) = G.add g;;
let (g, n5) = G.add g;;
let (g, n6) = G.add g;;
let g = G.set_edge g (G.source g, n2) (-1);;
let g = G.set_edge g (n2, n3) (-1);;
let g = G.set_edge g (n2, n4) (-1);;
let g = G.set_edge g (n3, n6) 1;;
let g = G.set_edge g (n4, n5) (-1);;
let g = G.set_edge g (n5, n6) 1;;
let g = G.set_edge g (n6, G.sink g) (-1);;

G.print_graph g;;
let (g, cut) = G.edmonds_karp g;;
G.print_nodes cut;;
G.print_graph g;;

let ls = (n4, "x") :: (n3, "y") :: (n6, "z") :: [];;

let lprot = G.filter_assoc ls cut;;

let a1 = { base=0 ; length=4 ; label=() };;
let b1 = { base=10 ; length=5 ; label=() };; 
let c =   Seq( ArrAssign(a1, Cst(CstI(0)), Cst(CstI(1))), 
          Seq( ArrAssign(a1, Cst(CstI(1)), Cst(CstI(2))), 
          Seq( ArrAssign(b1, Cst(CstI(3)), Cst(CstI(42))), 
          Seq( VarAssign("x", ArrayRead(a1, Cst(CstI(0)))), 
          Seq( VarAssign("y", ArrayRead(a1, Cst(CstI(1)))), 
          Seq( VarAssign("z", Expr(BinOp(Var("x"), Var("y"), Add))), 
               VarAssign("w", ArrayRead(b1, Var("z"))) 
          ))))))
 
let c' = protect_cmd c lprot;;

Printf.printf "%s\n" (strCmd c');;
