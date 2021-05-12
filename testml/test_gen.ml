open Ast
open Def_use_gen
open Graph
open Blade
open Utils

module H = HashTableGen
module G = UseGraph
module B = Blade

let a1 = { base=0 ; length=4 ; label=() };;
let b1 = { base=10 ; length=5 ; label=() };; 
let c =   Seq( ArrAssign(a1, Cst(CstI(0)), Cst(CstI(1))), 
          Seq( ArrAssign(a1, Cst(CstI(1)), Cst(CstI(2))), 
          Seq( ArrAssign(b1, Cst(CstI(3)), Cst(CstI(42))), 
          Seq( VarAssign("x", ArrayRead(a1, Cst(CstI(0)))), 
          Seq( VarAssign("y", ArrayRead(a1, Cst(CstI(1)))), 
          Seq( VarAssign("z", Expr(BinOp(Var("x"), Var("y"), Add))), 
               VarAssign("w", ArrayRead(b1, Var("z"))) 
))))));;
 
Printf.printf "Original code: \n";;
Printf.printf "%s\n" (strCmd c);;
(*
let gen = H.new_gen ();;
H.populate_graph gen c 1;;

H.print_generated gen;;

let g = H.get_graph gen;;

let (_, cut) = G.edmonds_karp g;;

let pairs = H.get_pairs gen;;

let lprot = G.filter_assoc pairs cut;;

let c' = B.protect_cmd c lprot;;
*)

let c'' = B.blade c ;;

Printf.printf "Result code: \n";;
Printf.printf "%s\n" (strCmd c'');;



