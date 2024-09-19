open Ast
open Asm
open Anf
open Gensym
open Parse

let bool_true = Int64.of_string "0x8000000000000001" (* 0x10...01 *)
let bool_false = 1L (* 0x0...1 *)

(* Lexical Environment *)
type env = (string * int) list
let empty_env = []
let extend_env (name : string) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)
let rec lookup_env (name : string) (env : env) : int =
  match env with
  | [] -> raise (CTError (Printf.sprintf "Unbound identifier: %s" name))
  | (n, i)::tail ->
    if n = name then i else (lookup_env name tail)

(* Function Environment *)
type fenv = fundef list
let empty_fenv : fenv = []
let rec lookup_fenv : string -> fenv -> fundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (Printf.sprintf "Undefined function: %s" s))
    | (f::fs) -> if fundef_name f = s then f else lookup_fenv s fs

(* Compiler *)
let rec compile_aexpr (expr : aexpr) (env : env) (fenv : fenv) : instruction list =
  match expr with
  | Let (id, c, a) ->
    let (env', slot) = extend_env id env in
    (compile_cexpr c env fenv)
    @ [ IMov (RegOffset (RSP, slot), Reg (RAX)) ]
    @ (compile_aexpr a env' fenv)
  | Ret c -> compile_cexpr c env fenv

and compile_cexpr (expr : cexpr) (env : env) (fenv : fenv) : instruction list =
  match expr with
  | Atom i -> [ IMov (Reg RAX, arg_immexpr i env empty_fenv) ]
  | Prim1 (op, c) -> 
    let head = compile_cexpr c env fenv in
    begin match op with
    | Add1 -> head @ [ IAdd (Reg(RAX), arg_immexpr (Num 1L) empty_env empty_fenv) ]
    | Sub1 -> head @ [ IAdd (Reg(RAX), arg_immexpr (Num (-1L)) empty_env empty_fenv) ]
    | Not -> 
      head 
      @ [ IMov (Reg(R10), Const(Int64.sub bool_true 1L)) ] 
      @ [ IXor (Reg(RAX), Reg(R10)) ]
    | Print -> failwith "To Be Done"
    end
  | Prim2 (op, i1, i2) -> 
    let head = [ IMov (Reg RAX, arg_immexpr i1 env empty_fenv) ] in
    begin match op with
    | Add -> head @ [ IAdd (Reg RAX, arg_immexpr i2 env empty_fenv) ]
    | And -> head @ (and_immexpr i2 env empty_fenv)
    | Or -> head @ (or_immexpr i2 env empty_fenv)
    | Lte -> head @ (lte_immexpr i2 env empty_fenv)
    end
  | If (cond_expr, then_expr, else_expr) ->
    let else_label = Gensym.fresh "else" in
    let done_label = Gensym.fresh "done" in
    [ IMov (Reg RAX, arg_immexpr cond_expr env empty_fenv) ]
    @ [ ICmp (Reg RAX, Const (bool_false)) ]
    @ [ IJe  (Label else_label) ]
    @ (compile_cexpr then_expr env fenv)
    @ [ IJmp (Label done_label) ]
    @ [ ILabel (Label else_label) ]
    @ (compile_cexpr else_expr env fenv)
    @ [ ILabel (Label done_label)]

and arg_immexpr (expr : immexpr) (env : env) (_ : fenv) : arg =
  match expr with
  | Num n -> Const (Int64.shift_left n 1)
  | Bool true -> Const (bool_true)
  | Bool false -> Const (bool_false)
  | Id x -> RegOffset (RSP, lookup_env x env)

and and_immexpr (expr : immexpr) (env : env) (_ : fenv) : instruction list =
  let done_label = Gensym.fresh "and" in
  [ IMov (Reg R10, Const bool_false) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg0 is false -> false *)
  @ [ IMov (Reg RAX, arg_immexpr expr env empty_fenv) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg1 is false -> false*)
  @ [ IMov (Reg RAX, Const bool_true) ] (* else -> true *)
  @ [ ILabel (Label done_label) ]

and or_immexpr (expr : immexpr) (env : env) (_ : fenv) : instruction list =
  let done_label = Gensym.fresh "or" in
  [ IMov (Reg R10, Const bool_true) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg R10, arg_immexpr expr env empty_fenv) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

and lte_immexpr (expr : immexpr) (env : env) (_ : fenv) : instruction list =
  let lte_label = Gensym.fresh "lte" in
  let done_label = Gensym.fresh "ltedone" in
  [ IMov (Reg R10, arg_immexpr expr env empty_fenv)]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJle (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_false) ]
  @ [ IJmp (Label done_label)]
  @ [ ILabel (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

let compile_prog (p : prog) : string =
  let _, e = p in
  let instrs = compile_aexpr (anf e) empty_env empty_fenv in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
