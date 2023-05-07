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
  let rec put_paren (tokens : expression list) op recsubexp = match tokens with
    | [] -> []
    | [x] -> x::[]
    | a::SymOp(x)::b::tail when x = op ->
      let newexp = SymExp([a; SymOp(x); b]) in
      (put_paren (newexp::tail) op false)

    | SymExp(a)::SymOp(x)::b::tail when x != op ->
      (put_paren [SymExp(a)] op recsubexp)@SymOp(x)::(put_paren (b::tail) op recsubexp)

    | a::SymOp(x)::b::tail when x != op -> a::SymOp(x)::(put_paren (b::tail) op recsubexp)

    | SymExp(sub_exp)::tail when recsubexp = true ->
      let newexp = SymExp((put_paren sub_exp op recsubexp)) in
      (put_paren (newexp::tail) op false)

    | SymExp(sub_exp)::tail when recsubexp = false ->
      SymExp(sub_exp)::(put_paren tail op true)
    |_ -> []
  in
  put_paren tokens op true;;


let rec string_of_expr (expr : expression list): string = match expr with
  | [] -> ""
  | SymNumber(x)::tail -> x^(string_of_expr tail)
  | SymVar(x)::tail -> x^(string_of_expr tail)
  | SymOp(x)::tail -> x^(string_of_expr tail)
  | SymExp(x)::tail -> "("^(string_of_expr x)^")"^(string_of_expr tail)


(* let%test "rev" = *)
(*   List.equal Int.equal (List.rev [ 3; 2; 1 ]) [ 1; 2; 4 ] *)
