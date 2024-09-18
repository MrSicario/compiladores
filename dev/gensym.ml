open Printf
open Ast

module Gensym = struct
  let counter = ref 0
  let init () = counter := 0

  let fresh s =
    let id = !counter in
    counter := id + 1;
    sprintf "$%s%d" s id
end

module Env = Map.Make (String)

let rec  alpha_rename_expr expr env =
  match expr with
  | Id x -> begin match Env.find_opt x env with
            | Some x_new -> Id x_new
            | None -> failwith (sprintf "Free identifier: %s" x) end
  | Num _ -> expr
  | Bool _ -> expr
  | Prim1 (op, e) -> Prim1 (op, alpha_rename_expr e env)
  | Prim2 (op, e1, e2) ->
      Prim2 (op, alpha_rename_expr e1 env, alpha_rename_expr e2 env)
  | If (e1, e2, e3) ->
      If
        ( alpha_rename_expr e1 env,
          alpha_rename_expr e2 env,
          alpha_rename_expr e3 env )
  | Let (x, e1, e2) ->
      let x' = Gensym.fresh x in
      let env' = Env.add x x' env in
      Let (x', alpha_rename_expr e1 env, alpha_rename_expr e2 env')