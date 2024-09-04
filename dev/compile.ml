open Ast
open Asm

type env = (string * int) list
let extend_env (name : string) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

let rec compile_expr (expr : expr) (env : env) : instruction list =
  match expr with 
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Id x -> let slot = List.assoc x env in
    [ IMov (Reg(RAX), (RegOffset (RSP, slot))) ]
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
