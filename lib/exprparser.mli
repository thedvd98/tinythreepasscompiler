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

type flat_ast =
  | Empty
  | FlatImm of int
  | FlatArg of string
  | FlatAdd of (flat_ast list)
  | FlatMul of (flat_ast list)
  | FlatSub of (flat_ast list)
  | FlatDiv of (flat_ast list)



val is_number : string -> bool
val is_variable : string -> bool
val tail_after_closing_paren : string list -> string list
val scan : string list -> expression list
val put_parens : expression list -> string -> expression list
val string_of_expr : expression list -> string
val optimize_ast : ast -> ast
val generate_ast : expression -> ast
val precedence_parens : expression list -> expression list

val flatten: ast -> flat_ast
val flat_to_ast: flat_ast -> ast
val optimize_flat: flat_ast -> flat_ast

val string_of_ast: ast -> string
