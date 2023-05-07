val tokenize : string -> string list
type expression =
  | SymOp of string
  | SymVar of string
  | SymNumber of string
  | SymExp of expression list
;;

val is_number : string -> bool
val is_variable : string -> bool
val tail_after_closing_paren : string list -> string list
val scan : string list -> expression list
val put_parens : expression list -> string -> expression list
val string_of_expr : expression list -> string
