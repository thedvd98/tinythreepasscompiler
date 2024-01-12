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
  | SymVar of string
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
      else false) true (tk |> String.to_seq |> List.of_seq);;

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


let rec scan tokens = match tokens with
  | [] -> []
  | hd::tail when is_number hd -> SymNumber(hd)::(scan tail)
  | hd::tail when is_variable hd -> SymVar(hd)::(scan tail)
  | hd::tail when hd = "(" -> SymExp((scan tail))::(scan (tail_after_closing_paren tail))
  | hd::_ when hd = ")" -> []
  | hd::tail -> SymOp(hd)::(scan tail)

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

  | a::SymOp(x)::b::[] when x = op || x = op2->
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
  | SymVar(x)::tail -> x^(string_of_expr tail)
  | SymOp(x)::tail -> x^(string_of_expr tail)
  | SymExp(x)::tail -> "("^(string_of_expr x)^")"^(string_of_expr tail)

let test (expr: string) :string =
  tokenize expr |> scan |> precedence_parens |> string_of_expr;;


type ast =
  | Imm of int  (* immediate value *)
  | Arg of string (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)

type flat_ast =
  | FlatImm of int
  | FlatArg of string
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
  | FlatAdd(li) ->
    if (contains_only_imm li) then
      let total = List.reduce (+) (flat_get_ints li) in
      FlatImm(total)
    else
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
  | SymExp(a::SymOp("-")::b::[]) -> Sub((generate_ast a), (generate_ast b))
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
  | Add((Arg _ as a), b) -> Add(a, (optimize_ast b))
  | Add(a, (Arg _ as b)) -> Add((optimize_ast a), b)

  | Sub(Arg _, Arg _) as e -> e
  | Mul(Arg _, Arg _) as e -> e
  | Div(Arg _, Arg _) as e -> e

  | Add(a, b) -> optimize_ast (Add((optimize_ast a), (optimize_ast b)))
  | Sub(a, b) -> optimize_ast (Sub((optimize_ast a), (optimize_ast b)))
  | Mul(a, b) -> optimize_ast (Mul((optimize_ast a), (optimize_ast b)))
  | Div(a, b) -> optimize_ast (Div((optimize_ast a), (optimize_ast b)))

let test2 (expr: string) =
  let result = tokenize expr |> scan |> precedence_parens in
  (match result with
   | [] ->  None
   | [e] -> Some(flat_to_ast (optimize_flat (flatten ((generate_ast e)))))
   | _ -> None
  )
let rec string_of_ast(ast_tree: ast): string = match ast_tree with
  | Imm(x) -> string_of_int x
  | Arg(x) -> x
  | Mul(a, b) -> "("^(string_of_ast a) ^ "*" ^ (string_of_ast b)^")"
  | Div(a, b) -> "("^(string_of_ast a) ^ "/" ^ (string_of_ast b)^")"
  | Add(a, b) -> "("^(string_of_ast a) ^ "+" ^ (string_of_ast b)^")"
  | Sub(a, b) -> "("^(string_of_ast a) ^ "-" ^ (string_of_ast b)^")"
