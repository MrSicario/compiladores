(** AST **)
open Printf

(* Algebraic datatype for expressions *)
type exp =
  | Var : string -> exp
  | Num : int -> exp
  | Bool : bool -> exp
  | Plus : exp * exp -> exp
  | Sub : exp * exp -> exp
  | Times : exp * exp -> exp
  | Div : exp * exp -> exp
  | Eq : exp * exp -> exp
  | Lt : exp * exp -> exp
  | And : exp * exp -> exp
  | If : exp * exp * exp -> exp
  | Fun : string * exp -> exp
  | App : exp * exp -> exp

(* Pretty printing *)
let rec string_of_exp(e : exp) : string = 
  match e with
  | Var x -> x
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Plus (e1, e2) -> sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Sub (e1, e2) -> sprintf "(- %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Times (e1, e2) -> sprintf "(* %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Div (e1, e2) -> sprintf "(/ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Eq (e1, e2) -> sprintf "(= %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Lt (e1, e2) -> sprintf "(< %s %s)" (string_of_exp e1) (string_of_exp e2)
  | And (e1, e2) -> sprintf "(and %s %s)" (string_of_exp e1) (string_of_exp e2)
  | If (cond, e1, e2) -> sprintf "(if %s %s %s)" (string_of_exp cond) (string_of_exp e1) (string_of_exp e2)
  | Fun (id, body) -> sprintf "(%s %s)" id (string_of_exp body)
  | App (fun_expr, arg_expr) -> sprintf "(%s %s)" (string_of_exp fun_expr) (string_of_exp arg_expr)

