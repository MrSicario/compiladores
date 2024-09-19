open Ast
open Asm
open Anf
open Gensym

let bool_true = Int64.of_string "0x8000000000000001" (* 0x10...01 *)
let bool_false = 1L (* 0x0...1 *)

type env = (string * int) list
let empty_env = []
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
  | Atom i -> [ IMov (Reg RAX, arg_immexpr i env) ]
  | Prim1 (op, c) -> 
    let head = compile_cexpr c env in
    begin match op with
    | Add1 -> head @ [ IAdd (Reg(RAX), arg_immexpr (Num 1L) empty_env) ]
    | Sub1 -> head @ [ IAdd (Reg(RAX), arg_immexpr (Num (-1L)) empty_env) ]
    | Not -> 
      head 
      @ [ IMov (Reg(R10), Const(Int64.sub bool_true 1L)) ] 
      @ [ IXor (Reg(RAX), Reg(R10)) ]
    end
  | Prim2 (op, i1, i2) -> let head = [ IMov (Reg RAX, arg_immexpr i1 env) ] in
    begin match op with
    | Add -> head @ [ IAdd (Reg RAX, arg_immexpr i2 env) ]
    | And -> head @ (and_immexpr i2 env)
    | Or -> head @ (or_immexpr i2 env)
    | Lte -> head @ (lte_immexpr i2 env)
    end
  | If (cond_expr, then_expr, else_expr) ->
    let else_label = Gensym.fresh "else" in
    let done_label = Gensym.fresh "done" in
    [ IMov (Reg RAX, arg_immexpr cond_expr env) ]
    @ [ ICmp (Reg RAX, Const (bool_false)) ]
    @ [ IJe  (Label else_label) ]
    @ (compile_cexpr then_expr env)
    @ [ IJmp (Label done_label) ]
    @ [ ILabel (Label else_label) ]
    @ (compile_cexpr else_expr env)
    @ [ ILabel (Label done_label)]

and arg_immexpr (expr : immexpr) (env : env) : arg =
  match expr with
  | Num n -> Const (Int64.shift_left n 1)
  | Bool true -> Const (bool_true)
  | Bool false -> Const (bool_false)
  | Id x -> RegOffset (RSP, lookup x env)

and and_immexpr (expr : immexpr) (env : env) : instruction list =
  let done_label = Gensym.fresh "and" in
  [ IMov (Reg R10, Const bool_false) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg0 is false -> false *)
  @ [ IMov (Reg RAX, arg_immexpr expr env) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg1 is false -> false*)
  @ [ IMov (Reg RAX, Const bool_true) ] (* else -> true *)
  @ [ ILabel (Label done_label) ]

and or_immexpr (expr : immexpr) (env : env) : instruction list =
  let done_label = Gensym.fresh "or" in
  [ IMov (Reg R10, Const bool_true) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg R10, arg_immexpr expr env) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

and lte_immexpr (expr : immexpr) (env : env) : instruction list =
  let lte_label = Gensym.fresh "lte" in
  let done_label = Gensym.fresh "ltedone" in
  [ IMov (Reg R10, arg_immexpr expr env)]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJle (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_false) ]
  @ [ IJmp (Label done_label)]
  @ [ ILabel (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

let compile_prog (p : prog) : string =
  let _, e = p in
  let instrs = compile_aexpr (anf e) empty_env in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
