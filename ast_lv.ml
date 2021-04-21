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


let splitIs (ls : 'a list) (n: int) =
    let rec split_is_rec (fls : 'a list) (ls : 'a list) (n: int) =
        (match ls, n with
            | a::cls, n     ->  if n == 0 then 
                                    (fls, a, cls) 
                                else if n > 0 then
                                    split_is_rec (fls @ [a]) cls (n-1)
                                else 
                                    failwith "invalid directive"
            | [], n         ->  failwith "invalid directive") in
        split_is_rec [] ls n;;




let rec eval (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int = 
    match attacker conf with
        | Fetch         -> evalFetch conf decls attacker obs count
        | PFetch(b)     -> evalPFetch conf decls attacker obs count b
        | Exec(n)       -> evalExec conf decls attacker obs count n
        | _             -> failwith "not implemented"

and

evalFetch (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) : configuration * observation list * int = 
    match conf.cs with
        | Skip::cs1         ->  printf "Skip %d\n" count; 
                                conf.cs <- cs1; 
                                eval conf decls attacker obs (count+1)
        | Seq(c1, c2)::cs1  ->  conf.cs <- c1::c2::cs1;
                                eval conf decls attacker obs count
        | []                ->  printf "end\n"; 
                                (conf, obs, count)
        | _                 ->  failwith "not implemented"
                
and

evalPFetch (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) (b : prediction): configuration * observation list * int = 
    match conf.cs, b with
        | If(e, c1, c2)::cs1, true     ->   conf.is <- conf.is @ [Guard(e, true, c2::cs1, count)];
                                            conf.cs <- c1::cs1;
                                            eval conf decls attacker (None::obs) (count+1)
        | If(e, c1, c2)::cs1, false    ->   conf.is <- conf.is @ [Guard(e, false, c1::cs1, count)];
                                            conf.cs <- c2::cs1;
                                            eval conf decls attacker (None::obs) (count+1)
        | _                            -> failwith "invalid directive"

and

evalExec (conf: configuration) (decls : decl_type StringMap.t) (attacker : configuration -> directive) (obs : observation list) (count : int) (n: int): configuration * observation list * int = 
    let (fs, i , ls) = splitIs conf.is n in
        let rho1 = phi fs conf.rho in
            match i with
                | Nop                 ->    failwith "invalid directive"
                | Assign(id, e)       ->    let v = evalexpr e rho1 in
                                                let i1 = Assign(id, v) in
                                                conf.is <- fs @ [i1] @ ls;
                                                eval conf decls attacker (None::obs) (count+1)
                (* TODO Assign(id, v) *)
                | Guard(e, b, cls, p) ->    let b1 = evalexpr e rho1 in
                                                if b1 == b then
                                                    conf.is <- fs @ [Nop] @ ls;
                                                    eval conf decls attacker (None::obs) (count+1)
                                                else
                                                    conf.is <- fs @ [Nop];
                                                    conf.cs <- cls
                                                    eval conf decls attacker (Rollback(p)::obs) (count+1)
                | _                   ->    failwith "not implemented"


let rec blade (command : cmd) : cmd =
	failwith "not implemented";;

(*let conf = {is=[]; cs=[Seq(Skip, Skip)]; mu=[||]; rho=StringMap.empty} ;;

eval conf StringMap.empty (fun _ -> Fetch) [] 0;;
*)




