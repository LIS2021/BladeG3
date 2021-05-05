(** This file contains the type definitions for the AST 
    along with utilities function to format and print 
    the various computation steps 
**)
module StringMap = Map.Make(String)

type identifier = string;;

(** 		BINARY OPERATORS 		**)
type op =
  | Add
  | Lte
  | Lt
  | BitAnd;;

(** Given a representation binary operation 
    returns the corresponding symbols to be printed **)
let string_of_op = function
  | Add -> "+"
  | Lte -> "<="
  | Lt -> "<"
  | BitAnd -> "&";;

(** The security label type**)  
type label = unit;;

(** A representation of arrays as a structure
    with a base index, a length and a security label **)
type arr = {
  base : int;
  length : int;
  label : label;
};;

(** Given a representation of an array
    returns the corresponding string to be printed **)
let string_of_arr a = Printf.sprintf "{%d,%d}" a.base a.length

let makeArr (b : int) (l : int) : arr =
  {base = b; length = l; label = ()};;

(** 		VALUES 		**)
type value =
  | CstI of int
  | CstB of bool
  | CstA of arr;;

(** Given a representation of a value
    returns the corresponding string to be printed **)
let string_of_value = function
  | CstI n -> string_of_int n
  | CstB b -> string_of_bool b
  | CstA a -> string_of_arr a;;

let makeInt (n : int) : value = CstI(n);;
let makeBool (b : bool) : value = CstB(b);;
let makeArray (a : arr) : value = CstA(a);;

(** 		EXPRESSIONS 		**)
type expr =
  | Cst of value
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of expr
  | Base of expr;;

(** Given a representation of an expression
    recursively returns the corresponding string to be printed **)
let rec string_of_expr = function
  | Cst v -> string_of_value v
  | Var x -> x
  | BinOp (e1, e2, op) -> Printf.sprintf "%s %s %s" (string_of_expr e1) (string_of_op op) (string_of_expr e2)
  | InlineIf (e1, e2, e3) -> Printf.sprintf "%s ? %s : %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Length e -> Printf.sprintf "length(%s)" (string_of_expr e)
  | Base e -> Printf.sprintf "base(%s)" (string_of_expr e);;

let makeCst v = Cst(v);;
let makeVar x = Var(x);;

(** 		RIGHT HAND-SIDE 		**)
type rhs =
  | Expr of expr
  | PtrRead of expr * label
  | ArrayRead of arr * expr;;

(** Given a representation of a right hand-side
    returns the corresponding string to be printed **)
let string_of_rhs = function
  | Expr e -> string_of_expr e
  | PtrRead (e, _) -> Printf.sprintf "*%s" (string_of_expr e)
  | ArrayRead (a, e) -> Printf.sprintf "%s[%s]" (string_of_arr a) (string_of_expr e);;

let makeExpr e = Expr(e);;
let makePtrRead e = PtrRead(e, ());;

type protect = Slh | Fence | Auto;;

(** Given a representation of a protect
    returns the corresponding string to be printed **)
let string_of_protect = function
  | Slh -> "slh"
  | Fence -> "fence"
  | Auto -> "auto";;

(** 		COMMANDS 		**)
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

(** Given a representation of a command
    returns the corresponding string to be printed 
    with the correct indentation **)
let string_of_cmd =
  let rec helper (indent : int) (c : cmd) =
    let spaces = String.make indent ' ' in
    match c with
      | Skip -> Printf.sprintf "%sskip" spaces
      | Fail -> Printf.sprintf "%sfail" spaces
      | VarAssign (x, r) -> Printf.sprintf "%s%s := %s" spaces x (string_of_rhs r)
      | PtrAssign (e1, e2, _) -> Printf.sprintf "%s*%s := %s" spaces (string_of_expr e1) (string_of_expr e2)
      | ArrAssign (a, e1, e2) -> Printf.sprintf "%s%s[%s] := %s" spaces (string_of_arr a) (string_of_expr e1) (string_of_expr e2)
      | Seq (c1, c2) -> Printf.sprintf "%s;\n%s" (helper indent c1) (helper indent c2)
      | If (e, c1, c2) -> Printf.sprintf "%s(if %s then\n%s\nelse\n%s)" spaces (string_of_expr e) (helper (indent + 2) c1) (helper (indent + 2) c2)
      | While (e, c1) -> Printf.sprintf "%s(while %s do\n%s)" spaces (string_of_expr e) (helper (indent + 2) c1)
      | Protect (x, p, r) -> Printf.sprintf "%s%s := protect_%s(%s)" spaces x (string_of_protect p) (string_of_rhs r)
   in helper 0

(** 		DIRECTIVES 		**)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

(** Given a representation of a directive
    returns the corresponding string to be printed **)
let string_of_directive = function
  | Fetch -> "fetch"
  | PFetch b -> Printf.sprintf "speculative fetch with %b" b
  | Exec n -> Printf.sprintf "exec %d" n
  | Retire -> Printf.sprintf "retire";;

type guard_fail_id = int;;

(** 		OBSERVATIONS 		**)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | OFail of guard_fail_id
  | Rollback of int

(** Given a representation of a observation
    returns the corresponding string to be printed **)
let string_of_observation = function
  | None -> "none"
  | Read (n, s) -> Printf.sprintf "read %d pending [%s]" n (String.concat ", " (List.map string_of_int s))
  | Write (n, s) -> Printf.sprintf "write %d pending [%s]" n (String.concat ", " (List.map string_of_int s))
  | OFail n -> Printf.sprintf "fail %d" n
  | Rollback n -> Printf.sprintf "rollback %d" n;;

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

(** Given a representation of a instruction
    returns the corresponding string to be printed **)
let string_of_instruction = function
  | Nop -> "nop"
  | AssignE (x, e) -> Printf.sprintf "%s := %s" x (string_of_expr e)
  | AssignV (x, v) -> Printf.sprintf "%s := %s" x (string_of_value v)
  | Load (x, _, e) -> Printf.sprintf "%s := load(%s)" x (string_of_expr e)
  | StoreE (e1, e2) -> Printf.sprintf "store(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | StoreV (n1, n2) -> Printf.sprintf "store(%d, %d)" n1 n2
  | IProtectE (x, p, e) -> Printf.sprintf "%s := protect_%s(%s)" x (string_of_protect p) (string_of_expr e)
  | IProtectV (x, v) -> Printf.sprintf "%s := %s" x (string_of_value v)
  | Guard (e, b, cs, n) -> Printf.sprintf "guard(%s, %b, %s, %d)" (string_of_expr e) b (String.concat ", " (List.map string_of_cmd cs)) n
  | IFail n -> Printf.sprintf "fail %d" n;;
