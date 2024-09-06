open Ast
open Asm
open Anf

type env = (string * int) list

type iflag =
  | FRet
  | FMov
  | FAdd
  | FCmp

let extend_env (name : string) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

let rec lookup (name : string) (env : env) : int =
  match env with
  | [] -> failwith (Printf.sprintf "Unknown identifier '%s'" name)
  | (n, i)::tail ->
    if n = name then i else (lookup name tail)

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
  | Atom i -> compile_immexpr i env FMov
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
      | Add -> (compile_immexpr i1 env FMov) @ (compile_immexpr i2 env FAdd)
      | _ -> failwith "TO BE DONE!"
    end

and compile_immexpr (expr : immexpr) (env : env) (i : iflag) : instruction list =
  match i with
  | FRet -> [ IRet ]
  | FMov ->
    begin
      match expr with
      | Num n -> [ IMov (Reg RAX, Const n) ]
      | Id x -> let slot = lookup x env in
        [ IMov (Reg RAX, RegOffset (RSP, slot)) ]
      | _ -> failwith "TBD"
    end
  | FAdd ->
    begin
      match expr with
      | Num n -> [ IAdd (Reg RAX, Const n) ]
      | Id x -> let slot = lookup x env in
        [ IAdd (Reg RAX, RegOffset (RSP, slot)) ]
      | _ -> failwith "TBD"
    end
  | FCmp ->
    begin
      match expr with
      | Num n -> [ ICmp (Reg RAX, Const n)]
      | Id x -> let slot = lookup x env in
        [ ICmp (Reg RAX, RegOffset (RSP, slot)) ]
      | _ -> failwith "TBD"
    end

let compile (e : expr) : string =
  let instrs = compile_aexpr (anf e) [] in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
