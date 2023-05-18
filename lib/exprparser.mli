val tokenize : string -> string list
type expression =
  | SymOp of string
  | SymVar of string
  | SymNumber of string
  | SymExp of expression list
;;

type ast =
  | Imm of int  (* immediate value *)
  | Arg of string (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)


val is_number : string -> bool
val is_variable : string -> bool
val tail_after_closing_paren : string list -> string list
val scan : string list -> expression list
val put_parens : expression list -> string -> expression list
val string_of_expr : expression list -> string
val optimize_ast : ast -> ast
val generate_ast : expression -> ast
val precedence_parens : expression list -> expression list

val string_of_ast: ast -> string
