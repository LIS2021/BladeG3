open Ast
open Opal

let keywords =
  ["skip"; "protect"; "if"; "then"; "else"; "while"; "do"; "fail"; "true"; "false"];;

let identifier =
  (spaces >> letter <~> many alpha_num) => implode
     >>= fun s -> if List.mem s keywords then mzero else return s;;

let integer =
   let* _ = spaces in
   let* op = option Fun.id (exactly '-' >> return Int.neg) in
   let* n = many1 digit => implode % int_of_string in
   return (op n);;

let boolean = (token "true" >> return true) <|> (token "false" >> return false);;

let array = between (token "{") (token "}")
                    (let* b = integer in
                     let* _ = token "," in
                     let* l = integer in
                     return (make_arr b l));;

let plus_op = token "+" >> return (make_bin_op Add);;
let lte_op = token "<=" >> return (make_bin_op Lte);;
let lt_op = token "<" >> return (make_bin_op Lt);;
let and_op = token "&" >> return (make_bin_op BitAnd);;
let value = (integer => make_int) <|> (boolean => make_bool) <|> (array => make_array);;

let rec expr input = input |>
  let* e1 = expr0 in
  (ternary_expr e1) <|> (return e1)
and ternary_expr e1 input = input |>
  let* _ = token "?" in
  let* e2 = expr in
  let* _ = token ":" in
  let* e3 = expr in
  return (make_inline_if e1 e2 e3)
and expr0 input = input |> chainl1 expr1 (lte_op <|> lt_op)
and expr1 input = input |> chainl1 expr2 plus_op
and expr2 input = input |> chainl1 expr3 and_op
and expr3 input = input |> choice
  [ between (token "(") (token ")") expr
  ; value => make_cst
  ; identifier => make_var
  ];;
let rec rhs input = input |> choice
  [ (token "*" >> expr) => make_ptr_read
  ; (let* a = array in
     let* e = between (token "[") (spaces >> token "]") expr in
     return (make_array_read a e))
  ; expr => make_expr
  ];;

let seq_op = token ";" >> return make_seq;;
let rec command input = input |> chainl1 cmd1 seq_op
and cmd1 input = input |> choice
  [ between (token "(") (token ")") command
  ; token "skip" >> return Skip
  ; token "fail" >> return Fail
  ; (let* _ = token "*" in
     let* e1 = expr in
     let* _ = token "=" in
     let* e2 = expr in
     return (make_ptr_assign e1 e2 ))
  ; (let* _ = token "if" in
     let* e1 = expr in
     let* _ = token "then" in
     let* c1 = command in
     let* _ = token "else" in
     let* c2 = command in
     return (make_if e1 c1 c2))
  ; (let* _ = token "while" in
     let* e1 = expr in
     let* _ = token "do" in
     let* c1 = command in
     return (make_while e1 c1))
  ; (let* a = array in
     let* e1 = between (token "[") (token "]") expr in
     let* _ = token ":=" in
     let* e2 = expr in
     return (make_arr_assign a e1 e2))
  ; (let* x = identifier in
     let* _ = token ":=" in
     let* r = rhs in
     return (make_var_assign x r))
  ; (let* x = identifier in
     let* _ = token ":=" in
     let* _ = token "protect" in
     let* r = between (token "(") (token ")") rhs in
     return (make_protect x Auto r))
  ];;

let parse_channel channel =
  parse (command << spaces << eof ()) (LazyStream.of_channel channel)
