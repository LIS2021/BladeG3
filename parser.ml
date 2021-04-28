open Ast;;
open Opal;;

let keywords =
  [ "skip"
  ; "protect"
  ; "if"
  ; "then"
  ; "else"
  ; "while"
  ; "do"
  ; "fail"
  ; "true"
  ; "false"
  ]

let identifier = (spaces >> letter <~> many alpha_num) => implode
                   >>= fun s -> if List.mem s keywords then mzero else return s;;
let integer = spaces >> many1 digit => implode % int_of_string;;
let boolean = (token "true" >> return true) <|> (token "false" >> return false);;
let array = spaces >> between (token "{") (token "}")
                              (let* b = integer in
                               token "," >>
                               let* l = integer in
                               return (makeArr b l));;
let plusOp = token "+" >> return (fun x y -> BinOp(x, y, Add));;
let lteOp = token "<=" >> return (fun x y -> BinOp(x, y, Lte));;
let ltOp = token "<" >> return (fun x y -> BinOp(x, y, Lt));;
let andOp = token "&" >> return (fun x y -> BinOp(x, y, BitAnd));;
let value = (integer => makeInt) <|> (boolean => makeBool) <|> (array => makeArray);;

let rec expr input = (let* e1 = expr0 in
                      (ternaryExpr e1) <|> (return e1)) input
and ternaryExpr e1 input = (token "?" >>
                            let* e2 = expr in
                            token ":" >>
                            let* e3 = expr in
                            return (InlineIf(e1, e2, e3))) input
and expr0 input = (chainl1 expr1 (lteOp <|> ltOp)) input
and expr1 input = (chainl1 expr2 plusOp) input
and expr2 input = (chainl1 expr3 andOp) input
and expr3 input = (((between (token "(") (token ")") expr))
                 <|> (value => makeCst)
                 <|> (identifier => makeVar)) input;;
let rec rhs input =
  (((token "*" >> expr) => makePtrRead)
  <|> (let* a = array in
       let* e = between (token "[") (spaces >> token "]") expr in
       return (ArrayRead(a, e)))
  <|> (expr => makeExpr)) input;;

let seqOp = token ";" >> return (fun x y -> Seq(x, y));;
let rec command input = (chainl1 cmd1 seqOp) input
and cmd1 input =
  ((between (token "(") (token ")") command)
  <|> (token "skip" >> return Skip)
  <|> (token "fail" >> return Fail)
  <|> (token "*" >>
       let* e1 = expr in
       token "=" >>
       let* e2 = expr in
       return (PtrAssign(e1, e2, ())))
  <|> (token "if" >>
       let* e1 = expr in
       token "then" >>
       let* c1 = command in
       token "else" >>
       let* c2 = command in
       return (If(e1, c1, c2)))
  <|> (token "while" >>
       let* e1 = expr in
       token "do" >>
       let* c1 = command in
       return (While(e1, c1)))
  <|> (let* a = array in
       let* e1 = between (token "[") (token "]") expr in
       token ":=" >>
       let* e2 = expr in
       return (ArrAssign(a, e1, e2)))
  <|> (let* x = identifier in
       token ":=" >>
       let* r = rhs in
       return (VarAssign(x, r)))
  <|> (let* x = identifier in
       token ":=" >>
       token "protect" >>
       let* r = between (token "(") (token ")") rhs in
       return (Protect(x, Auto, r)))) input;;
