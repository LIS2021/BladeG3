module StringMap = Map.Make(String)

type identifier = string;;

type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

type label = unit;;

type arr = {
  base : int;
  length : int;
  label : label;
};;

let makeArr (b : int) (l : int) : arr =
  {base = b; length = l; label = ()};;

type value =
  | CstI of int
  | CstB of bool
  | CstA of arr;;

let makeInt (n : int) : value = CstI(n);;
let makeBool (b : bool) : value = CstB(b);;
let makeArray (a : arr) : value = CstA(a);;

type expr =
  | Cst of value
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

let makeCst v = Cst(v);;
let makeVar x = Var(x);;

type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

let makeExpr e = Expr(e);;
let makePtrRead e = PtrRead(e, ());;

type protect = Slh | Fence | Auto;;

type cmd =
  | Skip
  | Fail
  | VarAssign of identifier * rhs
  | PtrAssign of expr * expr * label
  | ArrAssign of arr * expr * expr
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

type guard_fail_id = int;;

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | OFail of guard_fail_id
  | Rollback of int

(**		INSTRUCTION SET		**)
type instruction =
  | Nop
  | AssignE of identifier * expr
  | AssignV of identifier * value
  | Load of identifier * label * expr 			(* 	id := load(e) 		*)
  | StoreE of expr * expr
  | StoreV of int * int
  | IProtectE of identifier * protect * expr 	(* 	id := protect(e) 	*)
  | IProtectV of identifier * value 	(* 	id := protect(v) 	*)
  | Guard of expr * prediction * cmd list * guard_fail_id
  | IFail of guard_fail_id ;;
