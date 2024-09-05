open Ast
open Asm
open Printf

type env = (string * int) list
let extend_env (name : string) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

let rec lookup (name : string) (env : env) : int =
  match env with
  | [] -> failwith (sprintf "Identifier %s not in environment." name)
  | (n, i)::tail ->
    if n = name then i else (lookup name tail)

let rec compile_expr (expr : expr) (env : env) : instruction list =
  match expr with 
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Id x -> 
    let slot = lookup x env in
    [ IMov (Reg(RAX), (RegOffset (RSP, slot))) ]
  | Prim1 (op, e) ->
    begin
      match op with
      | Add1 -> (compile_expr e env) @ [ IAdd (Reg(RAX), Const(1L)) ]
      | Sub1 -> (compile_expr e env) @ [ IAdd (Reg(RAX), Const(-1L)) ]
    end
  | Let (id, e, b) ->
    let (env', slot) = extend_env id env in
    (compile_expr e env)
    @ [ IMov (RegOffset (RSP, slot), Reg (RAX)) ]
    @ (compile_expr b env')
  | _ -> failwith "TO BE DONE!"

let compile e : string =
  let instrs = compile_expr e [] in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
