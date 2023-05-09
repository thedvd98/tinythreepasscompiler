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

let put_parens tokens op =
  let rec put_paren (tokens : expression list) (op: string) (recsubexp:bool) : expression list = match tokens with
    | [] -> []
    | [SymExp(sub_exp)] ->
      let result = (put_paren sub_exp op true) in
      let value: bool = (match result with
          | [] -> true
          | [SymExp(_)]-> false
          | _ -> true)
      in
      if value = true then
        [SymExp(result)]
      else
        result

    | [SymNumber(x)] -> [SymNumber(x)]
    | [SymVar(x)] -> [SymVar(x)]
                     (* TODO Some error handling *)
    | [SymOp(x)] -> [SymVar("Wrong "^x^" position")]
    | [SymOp(_);SymNumber(_)] -> [SymVar("strange")]

    | a::SymOp(x)::b::[] when x = op ->
      if recsubexp = false then
        (put_paren [a] op recsubexp) @ [SymOp(x)] @ (put_paren [b] op recsubexp)
      else
        [SymExp(
            (put_paren [a] op recsubexp) @ [SymOp(x)] @ (put_paren [b] op recsubexp)
          )]
    | a::SymOp(x)::b::[] when x != op ->
        (put_paren [a] op recsubexp) @ [SymOp(x)] @ (put_paren [b] op recsubexp)

    | a::SymOp(x)::b::tail when x = op ->
      let result = (put_paren [a] op recsubexp) @ [SymOp(x)] @ (put_paren [b] op recsubexp) in
      let newexp = SymExp(result) in
      (put_paren (newexp::tail) op recsubexp)

    | SymExp(a)::SymOp(x)::b::tail when x != op ->
      (put_paren [SymExp(a)] op recsubexp)@SymOp(x)::(put_paren (b::tail) op recsubexp)

    | a::SymOp(x)::b::tail when x != op ->
      let arg1 = (put_paren [a] op recsubexp) in
      arg1 @ [SymOp(x)] @ (put_paren (b::tail) op recsubexp)
    | _ -> [SymVar("Error3")]
  in
  put_paren tokens op true


let precedence_parens (symbols: expression list) =
  List.fold_left put_parens symbols ["*"; "/"; "+"; "-"]

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

exception AstGeneration of string
exception Optimization of string

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
  | Add(a, b) -> optimize_ast (Add((optimize_ast a), (optimize_ast b)))
  | Sub(a, b) -> optimize_ast (Sub((optimize_ast a), (optimize_ast b)))
  | Mul(a, b) -> optimize_ast (Mul((optimize_ast a), (optimize_ast b)))
  | Div(a, (Arg _ as e)) -> (Div((optimize_ast a), e))
  | Div((Arg _ as e), b) -> (Div(e, (optimize_ast b)))
  | Div(a, b) -> optimize_ast (Div((optimize_ast a), (optimize_ast b)))

let test2 (expr: string) =
  let result = tokenize expr |> scan |> precedence_parens in
  (match result with
   | [] ->  None
   | [e] -> Some(optimize_ast(generate_ast e))
   | _ -> None
  )
