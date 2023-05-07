open Alcotest
open Exprparser

let expr_pp ppf e = Fmt.pf ppf "%s" (string_of_expr e)
let expr_equal a b = (a = b);;
let expr_testable = Alcotest.testable expr_pp expr_equal

let test_tokenize input expected () =
  let output = tokenize input in
  check (list string) "same" output expected;;

let test_parens op input expected () =
  let output = put_parens (tokenize input |> scan) op in
  check (expr_testable) "fuck" expected output;;

let suite =
  [ "basic", `Quick, test_tokenize "1 + 2" ["1"; "+"; "2"]
  ; "basic", `Quick, test_tokenize "1 + 2 * 3" ["1"; "+"; "2"; "*"; "3"]
  ]

let suite_parens =
  [ "1.", `Quick, test_parens "*" "1" [SymNumber("1")]
  ; "2.", `Quick, test_parens "*" "1 + a" [SymNumber("1"); SymOp("+"); SymVar("a")]
  ; "3.", `Quick, test_parens "*" "(1)" [SymExp([SymNumber("1")])]
  ; "4.", `Quick, test_parens "*" "(1 + a)" [SymExp([SymNumber("1"); SymOp("+"); SymVar("a")])]
  ; "5.", `Quick, test_parens "+" "1 + a" [SymExp([SymNumber("1"); SymOp("+"); SymVar("a")])]
  ; "6.", `Quick, test_parens "+" "(1 + a)" [SymExp([SymNumber("1"); SymOp("+"); SymVar("a")])]
  ; "7.", `Quick, test_parens "+" "1 + a * 2" (tokenize "(1 + a) * 2" |> scan)
  ; "8.", `Quick, test_parens "*" "1 + a * 2" (tokenize "1 + (a * 2)" |> scan)
  ; "9.", `Quick, test_parens "*" "1 + a * 2 + 10 * a * b" (tokenize "1 + (a * 2) + ((10 * a) * b)" |> scan)
  ; "10.", `Quick, test_parens "*" "1 * 1 * 1 * 1 * 1" (tokenize "((((1*1)*1) * 1) * 1)" |> scan)
  ]
let suite_subexp_parens =
  [ "0.", `Quick, test_parens "+" "1 + a + b" (tokenize "((1 + a) + b)" |> scan)
  ; "1.", `Quick, test_parens "+" "((a * b)+a)" (tokenize "((a * b) + a)" |> scan)
  ; "2.", `Quick, test_parens "+" "((a + b)+a)" (tokenize "((a + b) + a)" |> scan)
  ; "3.", `Quick, test_parens "+" "(a * (b+a))" (tokenize "(a * (b + a))" |> scan)
  ; "4.", `Quick, test_parens "+" "(a + (b+a))" (tokenize "(a + (b + a))" |> scan)
  ; "5.", `Quick, test_parens "+" "(a + (b+a-1+3))" (tokenize "(a + ((b + a) - (1+3)))" |> scan)
  ; "6.", `Quick, test_parens "+" "(a + (b+a-1+3 * 4 + (2)))" (tokenize "(a + ((b + a) - (1+3) * (4+(2))))" |> scan)
  ; "7.", `Quick, test_parens "+" "((a + b)+a + c)" (tokenize "(((a + b) + a) + c)" |> scan)
  ; "8.", `Quick, test_parens "+" "((1 + 4 + 3) +a)" (tokenize "(((1 + 4) + 3) + a)" |> scan)
  ; "9.", `Quick, test_parens "+" "(a + ((1 + 4 + 3) +a))" (tokenize "(a + (((1 + 4) + 3) + a))" |> scan)
  ; "10.", `Quick, test_parens "*" "((a + b)+a + c)" (tokenize "((a + b) + a + c)" |> scan)
  ; "11.", `Quick, test_parens "*" "((a + b - r)+a + c)" (tokenize "((a + b - r) + a + c)" |> scan)
  ; "12.", `Quick, test_parens "*" "((a + (1+2) - r)+a + c)" (tokenize "((a + (1+2) - r) + a + c)" |> scan)
  ; "13.", `Quick, test_parens "+" "((a + (1+2) - r)+a + c)" (tokenize "((((a + (1+2)) - r) + a) + c)" |> scan)
  ; "14.", `Quick, test_parens "*" "(a * b * c)" (tokenize "((a*b) * c)" |> scan)
  ; "15.", `Quick, test_parens "*" "(a * b * c * d)" (tokenize "(((a*b) * c) * d)" |> scan)
  ; "16.", `Quick, test_parens "*" "((a * b * c * d))" (tokenize "(((a*b) * c) * d)" |> scan)
  ]

let () =
  Alcotest.run "Dummy" [ "tokenize", suite
                       ; "easy parenthesis", suite_parens
                       ; "sub expression parenthesis", suite_subexp_parens
                       ];

