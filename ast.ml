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
	failwith "not implemented";;

let rec blade (command : cmd) : cmd =
	failwith "not implemented";;

