open Ast
open Asm
open Anf
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

(* DEPRECATED - Usar compile_aexpr*)
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

let rec compile_aexpr (expr : aexpr) (env : env) : instruction list =
  match expr with
  | Let (id, c, a) ->
    let (env', slot) = extend_env id env in
    (compile_cexpr c env)
    @ [ IMov (RegOffset (RSP, slot), Reg (RAX)) ]
    @ (compile_aexpr a env')
  | Ret c -> compile_cexpr c env

and compile_cexpr (expr : cexpr) (env : env) : instruction list =
  match expr with
  | Atom i -> move_immexpr i env
  | Prim1 (op, c) ->
    begin
      match op with
      | Add1 -> 
        (compile_cexpr c env) 
        @ [ IAdd (Reg(RAX), Const(1L)) ]
      | Sub1 -> 
        (compile_cexpr c env) 
        @ [ IAdd (Reg(RAX), Const(-1L)) ]
    end
  | Prim2 (op, i1, i2) ->
    begin
      match op with
      | Add -> (move_immexpr i1 env) @ (add_immexpr i2 env)
      | _ -> failwith "TO BE DONE!"
    end

and move_immexpr (expr : immexpr) (env : env) : instruction list =
  match expr with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Id x ->
    let slot = lookup x env in
    [ IMov (Reg RAX, RegOffset (RSP, slot)) ]
  | _ -> failwith "TBD"

and add_immexpr (expr : immexpr) (env : env) : instruction list =
  match expr with
  | Num n -> [ IAdd (Reg RAX, Const n) ]
  | Id x ->
    let slot = lookup x env in
    [ IAdd (Reg RAX, RegOffset (RSP, slot)) ]
  | _ -> failwith "TBD"

let compile (e : expr) : string =
  let instrs = compile_aexpr (anf e) [] in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
