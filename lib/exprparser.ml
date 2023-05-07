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
  String.for_all (fun ch -> let code = Char.code ch in
                   if (code >= 48 && code <= 57) || ch = '.' then true
                   else false) tk;;
let is_variable tk : bool=
  String.for_all (fun ch ->
      let code = Char.code ch in
      if (code >= 97 && code <= 122) || (code >= 65 && code <= 90) then true
      else false) tk;;

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
  let rec put_paren (tokens : expression list) (op: string) (recsubexp:bool) = match tokens with
    | [] -> []
    | [SymExp(sub_exp)] ->
      let len = List.length sub_exp in
      if len = 1 || len = 3 then
        SymExp(put_paren sub_exp op false)::[]
      else
        let newexp = SymExp((put_paren sub_exp op true)) in
        [newexp]

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
      let newexp = SymExp(
          (put_paren [a] op recsubexp) @ [SymOp(x)] @ (put_paren [b] op recsubexp)
        ) in
      (put_paren (newexp::tail) op recsubexp)

    | SymExp(a)::SymOp(x)::b::tail when x != op ->
      (put_paren [SymExp(a)] op recsubexp)@SymOp(x)::(put_paren (b::tail) op recsubexp)

    | a::SymOp(x)::b::tail when x != op ->
      let arg1 = (put_paren [a] op recsubexp) in
      arg1 @ [SymOp(x)] @ (put_paren (b::tail) op recsubexp)
    | _ -> [SymVar("Error3")]
  in
  let result = put_paren tokens op true in
  (match result with
  | [SymExp([SymExp(x)])] -> [SymExp(x)]
  | x -> x
  )

let rec string_of_expr (expr : expression list): string = match expr with
  | [] -> ""
  | SymNumber(x)::tail -> x^(string_of_expr tail)
  | SymVar(x)::tail -> x^(string_of_expr tail)
  | SymOp(x)::tail -> x^(string_of_expr tail)
  | SymExp(x)::tail -> "("^(string_of_expr x)^")"^(string_of_expr tail)

