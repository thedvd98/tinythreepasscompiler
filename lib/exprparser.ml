(* https://www.codewars.com/kata/5265b0885fda8eac5900093b/train/ocaml *)

module List = struct
  include List
  let reduce op li = match li with
    | [] -> 0
    | [a] -> a
    | hd::tail -> List.fold_left op hd tail
end;;

let tokenize code =
  let rec explode string =
    if String.length string = 0 then []
    else [String.sub string 0 1] @
         explode (String.sub string 1 ((String.length string) - 1))
  in
  let specialChars =
    [
      "["; "]"; "-"; "+"; "*"; "/"; "("; ")"
    ]
  in
  let nonSpecialHelper = function
    | "" -> []
    | str -> [str]
  in
  let rec tokenizeHelper = function
    | [],currentItem, tokens ->
      tokens @ (nonSpecialHelper currentItem)
    | " "::lst, currentItem, tokens ->
      tokenizeHelper(
        lst,"",
        tokens @ nonSpecialHelper currentItem)
    | item::lst, currentItem, tokens ->
      if List.mem item specialChars then
        tokenizeHelper(
          lst, "",
          tokens @ nonSpecialHelper currentItem @ [item])
      else
        tokenizeHelper(lst, currentItem ^ item,tokens)
  in
  tokenizeHelper(explode code, "", [])

type expression =
  | SymOp of string
  | SymVar of int
  | SymNumber of string
  | SymExp of (expression list)

exception AstGeneration of string
exception Optimization of string
exception ParserError of string

let is_number tk : bool=
List.fold_left (fun acc ch -> let code = Char.code ch in
                   if (code >= 48 && code <= 57) || ch = '.' then (true && acc)
                   else false) true (tk |> String.to_seq |> List.of_seq);;

let is_variable tk : bool=
  List.fold_left (fun acc ch ->
      let code = Char.code ch in
      if (code >= 97 && code <= 122) || (code >= 65 && code <= 90) then (true && acc)
      else false)
    true
    (tk |> String.to_seq |> List.of_seq);;

let tail_after_closing_paren li =
  (* make it start from n = 1 *)
  let rec tail_from_corresponding_closed_paren li n = match li with
    | [] -> []
    | l when n = 0 -> l
    | hd::tail when hd = "(" -> tail_from_corresponding_closed_paren tail (n + 1)
    | hd::tail when hd = ")" -> tail_from_corresponding_closed_paren tail (n - 1)
    | _::tail -> tail_from_corresponding_closed_paren tail n
  in
  tail_from_corresponding_closed_paren li 1

(*assume that it will find it*)
let find_index element li =
  let rec aux element li index = match li with
    | hd::tail when element = hd -> index
    | hd::tail -> (aux element tail (index + 1))
    | [] -> index
  in
  (aux element li 0)

let scan tokens =
  let rec scan_aux tokens env = match tokens with
    | [] -> []
    | hd::tail when hd = "[" -> (scan_args tail [])
    | hd::tail when is_number hd -> SymNumber(hd)::(scan_aux tail env)
    | hd::tail when is_variable hd -> SymVar(find_index hd env)::(scan_aux tail env)
    | hd::tail when hd = "(" -> SymExp((scan_aux tail env))::(scan_aux (tail_after_closing_paren tail) env)
    | hd::_ when hd = ")" -> []
    | hd::tail -> SymOp(hd)::(scan_aux tail env)
  and scan_args tokens env = match tokens with
    | hd::tail when hd = "]" -> (scan_aux tail env)
    | hd::tail -> (scan_args tail (env@[hd]))
    | [] -> []
  in
  (scan_aux tokens [])

let rec put_parens (tokens : expression list) (op: string) (op2: string) : expression list = match tokens with
| [] -> []
| [SymExp(sub_exp)] ->
(match (put_parens sub_exp op op2) with
     | [SymExp(_)] as exp -> exp
     | ([] | _ as x) -> [SymExp(x)])

  | ([SymNumber(_)] | [SymVar(_)] as symbol) -> symbol
  | [SymOp(_)] -> raise (ParserError "Operator in a wrong place ")
  | [SymOp("-");SymNumber(n)] -> [SymNumber("-"^n)]
  | [SymOp("+");SymNumber(n)] -> [SymNumber(n)]
  | [SymOp(_);SymNumber(_)] -> raise (ParserError "strange")

  (* cases for when there is a sign in front of a number*)
  | SymOp("-")::SymNumber(n)::tail -> (put_parens (SymNumber("-"^n)::tail) op op2)
  | SymOp("+")::SymNumber(n)::tail -> (put_parens (SymNumber(n)::tail) op op2)

  (* generic cases *)
  | a::SymOp(x)::b::[] when x = op || x = op2 ->
    [SymExp((put_parens [a] op op2) @ [SymOp(x)] @ (put_parens [b] op op2))]
  | a::SymOp(x)::b::[] when x != op && x != op2 ->
    (put_parens [a] op op2) @ [SymOp(x)] @ (put_parens [b] op op2)

  | a::SymOp(x)::b::tail when x = op || x = op2 ->
    let newexp = SymExp((put_parens [a] op op2) @ [SymOp(x)] @ (put_parens [b] op op2)) in
    (put_parens (newexp::tail) op op2)

  | SymExp(a)::SymOp(x)::b::tail when x != op && x != op2 ->
    (put_parens [SymExp(a)] op op2)@SymOp(x)::(put_parens (b::tail) op op2)

  | a::SymOp(x)::b::tail when x != op && x != op2 ->
    (put_parens [a] op op2) @ [SymOp(x)] @ (put_parens (b::tail) op op2)
  | _ -> raise (ParserError "Error Wrong symbol inserted")


let precedence_parens (symbols: expression list) =
    put_parens (put_parens symbols "*" "/") "+" "-"

let rec string_of_expr (expr : expression list): string = match expr with
  | [] -> ""
  | SymNumber(x)::tail -> x^(string_of_expr tail)
  | SymVar(x)::tail -> "Arg("^(string_of_int x)^")"^(string_of_expr tail)
  | SymOp(x)::tail -> x^(string_of_expr tail)
  | SymExp(x)::tail -> "("^(string_of_expr x)^")"^(string_of_expr tail)

let test (expr: string) :string =
  tokenize expr |> scan |> precedence_parens |> string_of_expr;;


type ast =
  | Imm of int  (* immediate value *)
  | Arg of int (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)

type flat_ast =
  | FlatImm of int
  | FlatArg of int
  | FlatAdd of (flat_ast list)
  | FlatMul of (flat_ast list)
  | FlatSub of (flat_ast list)
  | FlatDiv of (flat_ast list)

let rec flatten = function
  | Imm(x) -> FlatImm(x)
  | Arg(x) -> FlatArg(x)
  | Add(a,b) -> let flattened_a = (flatten a) in
    let flattened_b = (flatten b) in
    (match (flattened_a, flattened_b) with
     | (FlatAdd(x), FlatAdd(y)) -> FlatAdd(x@y)
     | (x, FlatAdd(y)) -> FlatAdd(x::y)
     | (FlatAdd(x), y) -> FlatAdd(x@[y])
     | (x, y) -> FlatAdd([x;y])
    )
  | Mul(a,b) -> let flattened_a = (flatten a) in
    let flattened_b = (flatten b) in
    (match (flattened_a, flattened_b) with
     | (FlatMul(x), FlatMul(y)) -> FlatMul(x@y)
     | (x, FlatMul(y)) -> FlatMul(x::y)
     | (FlatMul(x), y) -> FlatMul(x@[y])
     | (x, y) -> FlatMul([x;y])
    )
  | Sub(a,b) -> let flattened_a = (flatten a) in
    let flattened_b = (flatten b) in
    (match (flattened_a, flattened_b) with
     | (FlatSub(x), FlatSub(y)) -> FlatSub(x@y)
     | (x, FlatSub(y)) -> FlatSub(x::y)
     | (FlatSub(x), y) -> FlatSub(x@[y])
     | (x, y) -> FlatSub([x;y])
    )
  | Div(a,b) -> let flattened_a = (flatten a) in
    let flattened_b = (flatten b) in
    (match (flattened_a, flattened_b) with
     | (FlatDiv(x), FlatDiv(y)) -> FlatDiv(x@y)
     | (x, FlatDiv(y)) -> FlatDiv(x::y)
     | (FlatDiv(x), y) -> FlatDiv(x@[y])
     | (x, y) -> FlatDiv([x;y])
    )

let flat_get_ints fl_imm =
  let filter = function
    | FlatImm(a) -> true
    | _ -> false
  in
  let to_int = function
    | FlatImm(a) -> a
    | _ -> 0
  in
  (List.map to_int (List.filter filter fl_imm))

let flat_imm_filter_out flat_li =
  let f = function
    | FlatImm(_) -> false
    | _ -> true
  in
  (List.filter f flat_li)

let contains_only_imm flat_li =
  let f = function
    | FlatImm(_) -> true
    | _ -> false
  in
  List.for_all f flat_li

let rec optimize_flat = function
  | (FlatImm(_) | FlatArg(_) as e) -> e
  | FlatAdd(li) when (contains_only_imm li)->
      let total = List.reduce (+) (flat_get_ints li) in
      FlatImm(total)
  | FlatAdd(li) ->
      FlatAdd(List.map optimize_flat li)

  | FlatMul(li) ->
    if (contains_only_imm li) then
      let total = List.reduce (fun x y-> x*y) (flat_get_ints li) in
      FlatImm(total)
    else
      FlatMul(List.map optimize_flat li)
  | FlatSub(li) ->
    if (contains_only_imm li) then
      let total = List.reduce (-) (flat_get_ints li) in
      FlatImm(total)
    else
      FlatSub(List.map optimize_flat li)

  | FlatDiv(li) ->
    if (contains_only_imm li) then
      let total = List.reduce (/) (flat_get_ints li) in
      FlatImm(total)
    else
      FlatDiv(List.map optimize_flat li)
;;

let optimize_flat_multiple_times tree =
  let rec aux a prev =
    if a = prev
    then a
    else (aux (optimize_flat a) a)
  in
  (aux (optimize_flat tree) tree);;


let rec flat_to_ast = function
  | FlatImm(a) -> Imm(a)
  | FlatArg(a) -> Arg(a)
  | FlatAdd([a]) -> (flat_to_ast a)
  | FlatAdd([a;b]) -> Add((flat_to_ast a), (flat_to_ast b))
  | FlatAdd(hd::tail) -> Add((flat_to_ast hd), (flat_to_ast (FlatAdd(tail))))
  | FlatMul([a]) -> (flat_to_ast a)
  | FlatMul([a;b]) -> Mul((flat_to_ast a), (flat_to_ast b))
  | FlatMul(hd::tail) -> Mul((flat_to_ast hd), (flat_to_ast (FlatMul(tail))))
  | FlatDiv([a]) -> (flat_to_ast a)
  | FlatDiv([a;b]) -> Div((flat_to_ast a), (flat_to_ast b))
  | FlatDiv(hd::tail) -> Div((flat_to_ast hd), (flat_to_ast (FlatDiv(tail))))
  | FlatSub([a]) -> (flat_to_ast a)
  | FlatSub([a;b]) -> Sub((flat_to_ast a), (flat_to_ast b))
  | FlatSub(hd::tail) -> Sub((flat_to_ast hd), (flat_to_ast (FlatSub(tail))))
  | _ -> raise (AstGeneration "flat_to_ast empty list or something")
;;


let rec generate_ast (symbols: expression): ast = match symbols with
  | SymExp([]) -> Imm(0)
  | SymNumber(n) -> Imm(int_of_string n)
  | SymVar(a) -> Arg(a)
  | SymExp(a::SymOp("*")::b::[]) -> Mul((generate_ast a), (generate_ast b))
  | SymExp(a::SymOp("/")::b::[]) -> Div((generate_ast a), (generate_ast b))
  | SymExp(a::SymOp("+")::b::[]) -> Add((generate_ast a), (generate_ast b))
  | SymExp(a::SymOp("-")::b::[]) -> Sub((generate_ast a), (generate_ast b))  (* TODO I could put Add and put a minux in fron of the second term in some way*)
  | SymExp(a::[]) -> (generate_ast a)
  | _ -> raise (AstGeneration "not recognized symbol in ast generation")

let t1 = "1 + 2 + 3 + a" |> tokenize |> scan |> precedence_parens |> List.hd |> generate_ast;;
let t2 = "1 + 2 * 3 + a" |> tokenize |> scan |> precedence_parens |> List.hd |> generate_ast;;
let t3 = "1 + 2 * b * 3 + a - (4 / 2)" |> tokenize |> scan |> precedence_parens |> List.hd |> generate_ast;;
let t4 = "1 + 2 + 3 + (a + 10 + 20)" |> tokenize |> scan |> precedence_parens |> List.hd |> generate_ast;;
let t5 = "1 + 2 - 5" |> tokenize |> scan |> precedence_parens |> List.hd |> generate_ast;;

let test1 (expr: string) =
  let result = tokenize expr |> scan |> precedence_parens in
  (match result with
   | [] ->  None
   | [e] -> Some(generate_ast e)
   | _ -> None
  )

let rec optimize_ast (ast_tree: ast) = match ast_tree with
  | (Imm _ | Arg _) as e -> e
  | Add(Imm(a), Imm(b)) -> (Imm(a+b))
  | Sub(Imm(a), Imm(b)) -> (Imm(a-b))
  | Mul(Imm(a), Imm(b)) -> (Imm(a*b))
  | Div(Imm(a), Imm(b)) -> (Imm(a/b))

  | Add(Arg _, Arg _) as e -> e
  | Sub(Arg _, Arg _) as e -> e
  | Mul(Arg _, Arg _) as e -> e
  | Div(Arg _, Arg _) as e -> e

  | Add (Add(Arg(x), Imm(a)), Imm(b)) -> (Add (Arg (x), ((Imm (a+b)))))
  | Add (Add(Imm(a), Arg(x)), Imm(b)) -> (Add (Arg (x), ((Imm (a+b)))))
  | Add (Imm(a), Add(Arg (x), Imm(b))) -> (Add ((Imm (a+b)), Arg (x)))
  | Add (Imm(a), Add(Imm (b), Arg(x))) -> (Add ((Imm (a+b)), Arg (x)))
  | Add(a, b) -> (Add((optimize_ast a), (optimize_ast b)))

  | Sub (Sub(Arg(x), Imm(a)), Imm(b)) -> (Sub (Arg (x), ((Imm (a-b)))))
  | Sub (Sub(Imm(a), Arg(x)), Imm(b)) -> (Sub ((Imm (a-b)), Arg (x)))
  | Sub (Imm(a), Sub(Arg (x), Imm(b))) -> (Sub ((Imm (a-b)), Arg (x)))
  | Sub (Imm(a), Sub(Imm (b), Arg(x))) -> (Sub ((Imm (a-b)), Arg (x)))
  | Sub(a, b) -> (Sub((optimize_ast a), (optimize_ast b)))

  | Mul (Mul(Arg(x), Imm(a)), Imm(b)) -> (Mul (Arg (x), ((Imm (a*b)))))
  | Mul (Mul(Imm(a), Arg(x)), Imm(b)) -> (Mul ((Imm (a*b)), Arg (x)))
  | Mul (Imm(a), Mul(Arg (x), Imm(b))) -> (Mul ((Imm (a*b)), Arg (x)))
  | Mul (Imm(a), Mul(Imm (b), Arg(x))) -> (Mul ((Imm (a*b)), Arg (x)))
  | Mul(a, b) -> (Mul((optimize_ast a), (optimize_ast b)))

  | Div (Div(Arg(x), Imm(a)), Imm(b)) -> (Div (Arg (x), ((Imm (a/b)))))
  | Div (Div(Imm(a), Arg(x)), Imm(b)) -> (Div ((Imm (a/b)), Arg (x)))
  | Div (Imm(a), Div(Arg (x), Imm(b))) -> (Div ((Imm (a/b)), Arg (x)))
  | Div (Imm(a), Div(Imm (b), Arg(x))) -> (Div ((Imm (a/b)), Arg (x)))
  | Div(a, b) -> (Div((optimize_ast a), (optimize_ast b)))

let optimize_ast_multiple_times tree =
  let rec aux a prev =
    if a = prev
    then a
    else (aux (optimize_ast a) a)
  in
  (aux (optimize_ast tree) tree);;


let test2 (expr: string) =
  let result = tokenize expr |> scan |> precedence_parens in
  (match result with
   | [] ->  None
   | [e] -> Some(flat_to_ast (optimize_flat (flatten ((generate_ast e)))))
   | _ -> None
  )
let rec string_of_ast(ast_tree: ast): string = match ast_tree with
  | Imm(x) -> string_of_int x
  | Arg(x) -> "Arg("^(string_of_int x)^")"
  | Mul(a, b) -> "("^(string_of_ast a) ^ "*" ^ (string_of_ast b)^")"
  | Div(a, b) -> "("^(string_of_ast a) ^ "/" ^ (string_of_ast b)^")"
  | Add(a, b) -> "("^(string_of_ast a) ^ "+" ^ (string_of_ast b)^")"
  | Sub(a, b) -> "("^(string_of_ast a) ^ "-" ^ (string_of_ast b)^")"

let rec generate_code = function
  | Imm(x) -> ["IM "^(string_of_int x)]
  | Arg(x) -> ["AR "^(string_of_int x)]
  | Add(a, b) -> (generate_code b) @ ["PU"] @ (generate_code a) @ ["SW"; "PO"; "AD"]
  | Sub(a, b) -> (generate_code a) @ ["PU"] @ (generate_code b) @ ["SW"; "PO"; "SU"]
  | Mul(a, b) -> (generate_code b) @ ["PU"] @ (generate_code a) @ ["SW"; "PO"; "MU"]
  | Div(a, b) -> (generate_code a) @ ["PU"] @ (generate_code b) @ ["SW"; "PO"; "DI"]

exception CompilerError of string

module type COMPILER =
sig
  val pass1: string -> ast
  val pass2: ast -> ast
  val codeGen: ast -> string list
  val compile: string -> string list 
end


module Compiler : COMPILER =
struct
  let pass1 code = 
    tokenize code |> scan |> precedence_parens |> List.hd |> generate_ast

  let pass2 ast =
    optimize_ast_multiple_times ast


  let codeGen ast = 
    generate_code ast

  let compile code =
    codeGen(pass2(pass1 code))

end
