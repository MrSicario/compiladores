open Printf
open Ast
open Parse

module Gensym = struct
  let counter = ref 0
  let init () = counter := 0

  let fresh s =
    let id = !counter in
    counter := id + 1;
    sprintf "%s%d" s id
end

(* Lexical Environment *)
module Env = Map.Make (String)

let gen_env l name =
  let rec find_duplicate ls = 
    match ls with
    | [] -> Ok ""
    | hd :: tl -> if List.mem hd tl then Error hd else find_duplicate tl
  in match find_duplicate l with
    | Ok _ ->
      let rec gen_env l env l' =
        match l with
        | [] -> (List.rev l', env)
        | id :: tail ->
          let id' = Gensym.fresh (name ^ "_" ^ id) in
          gen_env tail (Env.add id id' env) (id' :: l')
      in gen_env l Env.empty []
    | Error x -> raise (CTError (sprintf "Identifier already declared in namespace: %s" x))

let extend_env l env =
  let rec find_duplicate ls = 
    match ls with
    | [] -> Ok ""
    | hd :: tl -> if List.mem hd tl then Error hd else find_duplicate tl
  in match find_duplicate l with
    | Ok _ ->
      let rec gen_env l env l' =
        match l with
        | [] -> (List.rev l', env)
        | id :: tail ->
          let id' = Gensym.fresh ("lambda" ^ "_" ^ id) in
          gen_env tail (Env.add id id' env) (id' :: l')
      in gen_env l env []
    | Error x -> raise (CTError (sprintf "Identifier already declared in namespace: %s" x))

let rec  alpha_rename_expr expr env =
  match expr with
  | Id x -> 
    begin
      match Env.find_opt x env with
      | Some x_new -> Id x_new
      | None -> raise (CTError (sprintf "Free identifier: %s" x))
    end
  | Num _ -> expr
  | Bool _ -> expr
  | Lambda (params, expr) ->
    let (params', lambda_env) = extend_env params env in
    Lambda (params', alpha_rename_expr expr lambda_env)
  | Prim1 (op, e) -> Prim1 (op, alpha_rename_expr e env)
  | Prim2 (op, e1, e2) ->
      Prim2 (op, alpha_rename_expr e1 env, alpha_rename_expr e2 env)
  | If (e1, e2, e3) ->
      If
        ( alpha_rename_expr e1 env,
          alpha_rename_expr e2 env,
          alpha_rename_expr e3 env)
  | Let (x, e1, e2) ->
    let x' = Gensym.fresh x in
    let env' = Env.add x x' env in
    Let (x', alpha_rename_expr e1 env, alpha_rename_expr e2 env')
  | Apply (f, expr_list) -> 
      Apply (f, List.map (fun e -> alpha_rename_expr e env) expr_list)
  | LamApply (lambda, args) -> 
    LamApply (alpha_rename_expr lambda env, List.map (fun expr -> alpha_rename_expr expr env) args)
  | Tuple expr_list ->
    let arename_env = fun x -> alpha_rename_expr x env in
    Tuple (List.map arename_env expr_list)
  | Set (e, k, v) -> Set (alpha_rename_expr e env, alpha_rename_expr k env, alpha_rename_expr v env)
  | LetRec (_, _) -> failwith "TBD"