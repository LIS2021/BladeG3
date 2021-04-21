open Format

module StringMap = Map.Make(String)

type identifier = string;;

type op = string;;

type expr =
  | CstI of int
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

type rhs =
  | Expr of expr
  | PtrRead of expr
  | ArrayRead of identifier * expr;;

type protect = Slh | Fence | Auto;;

type cmd =
  | Skip 
  | Fail
  | VarAssign of identifier * rhs
  | PtrAssign of expr * expr
  | ArrAssign of identifier * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Protect of identifier * protect * rhs;;

(** 		DIRECTIVES 		**)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | Fail of int
  | Rollback of int

(**		INSTRUCTION SET		**)
type instruction =
  | Nop
  | Assign of identifier * expr
  | Load of identifier * expr 			(* 	id := load(e) 		*)
  | Store of expr * expr
  | IProtect of identifier * protect * expr 	(* 	id := protect(e) 	*)
  | Guard of expr * prediction * cmd list * int
  | Fail of int ;;

(**		CONFIGURATIONS 		**)
type configuration = {
	mutable is : instruction list ; 
	mutable cs : cmd list ; 
	mutable mu : int array ; 
	mutable rho : int StringMap.t ;
};;

type decl_type =
  | TypI
  | TypA of int * int
  | TypP;;




let rec eval (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int = 
    match attacker conf with
        | Fetch     -> evalFetch conf decls attacker obs count
        | _         -> failwith "not implemented"

and

evalFetch (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int = 
    match conf.cs with
        | Skip::cs1         ->  printf "Skip %d\n" count; 
                                conf.is <- conf.is @ [Nop];
                                conf.cs <- cs1; 
                                eval conf decls attacker obs (count+1)
        | Seq(c1, c2)::cs1  ->  conf.cs <- c1::c2::cs1;
                                eval conf decls attacker obs count
        | []                ->  printf "end\n"; 
                                (conf, obs, count)
        | _                 -> failwith "not implemented";;
                

let rec blade (command : cmd) : cmd =
	failwith "not implemented";;

let conf = {is=[]; cs=[Seq(Skip, Skip)]; mu=[||]; rho=StringMap.empty} ;;

eval conf StringMap.empty (fun _ -> Fetch) [] 0;;

