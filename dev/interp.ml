(** Interpreter **)

open Ast
open Lib

let rec interp : env -> exp -> value =
  fun env e ->
  match e with
  | Var x -> List.assoc x env (* Functions on list can be found in the [List] module and accessed with the [List.function] syntax *)
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Fun (id, body) -> ClosureV (id, body, env)
  | Plus  (e1, e2) -> liftNumV ( + ) (interp env e1) (interp env e2)
  | Sub (e1, e2) -> liftNumV ( - ) (interp env e1) (interp env e2)
  | Times (e1, e2) -> liftNumV ( * ) (interp env e1) (interp env e2)
  | Div (e1, e2) -> liftNumV ( / ) (interp env e1) (interp env e2)
  | Eq (e1, e2) -> liftNumVBoolV ( = ) (interp env e1) (interp env e2)
  | Lt (e1, e2) -> liftNumVBoolV ( < ) (interp env e1) (interp env e2)
  | And (e1, e2) ->
    begin
      match interp env e1 with
      | BoolV false -> BoolV false
      | BoolV true -> liftBoolV ( && ) (BoolV true) (interp env e2)
      | _ -> failwith (Printf.sprintf "Not a BoolV")
    end
  | If (cond, e1, e2) ->
    begin
      match interp env cond with
      | BoolV true -> interp env e1
      | BoolV false -> interp env e2
      | _ -> failwith (Printf.sprintf "Condition is not a BoolV")
    end
  | App (fun_body, arg_body) ->
    begin
      match (interp env fun_body) with
      | ClosureV (id, body, fenv) -> interp (extend_env id (interp env arg_body) fenv) body
      | _ -> failwith (Printf.sprintf "Not a ClosureV")
    end

let simplify = fun exp ->
  match exp with
  | Plus (e, Num 0) | Plus (Num 0, e) -> e
  | Times (e, Num 1) | Times (Num 1, e) -> e
  | _ -> exp