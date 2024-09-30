open Ast
open Asm
open Anf
open Gensym
open Parse
open Checks

(* Compile constants *)
let bool_true = Int64.of_string "0x8000000000000001" (* 0x10...01 *)
let bool_false = 1L (* 0x0...1 *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L

(* Lexical Environment *)
type env = (string * reg * int) list
let empty_env = []
let extend_env (name : string) (reg : reg) (env : env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, reg, -slot)::env, -slot)
let rec lookup_env (name : string) (env : env) : reg * int =
  match env with
  | [] -> raise (CTError (Printf.sprintf "Free identifier: %s" name))
  | (n, reg, i)::tail ->
    if n = name then (reg, i) else (lookup_env name tail)

(* Function Environment *)
type afenv = afundef list
let empty_afenv : afenv = []
let rec lookup_afenv : string -> afenv -> afundef =
  fun s fenv -> 
    match fenv with
    | [] -> raise (CTError (Printf.sprintf "Undefined function: %s" s))
    | (f::fs) -> if afundef_name f = s then f else lookup_afenv s fs

(* Compiler *)
let test_if_number =
  [ ITest (Reg RAX, Const 1L) ]
  @ [ IJnz (Label "error_not_number") ]

let test_if_bool =
  [ ITest (Reg RAX, Const 1L) ]
  @ [ IJz (Label "error_not_bool") ]

let error_handlers =
  let error_not_number =
    [ ILabel (Label "error_not_number") ]
    @ [ IPush (Reg RBP) ]
    @ [ IMov (Reg RBP, Reg RSP) ]
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ IMov (Reg RDI, Const 1L) ]
    @ [ ICall (Label "error") ]
    @ [ IMov (Reg RSP, Reg RBP) ]
    @ [ IPop (Reg RBP) ]
    @ [ IRet ]
  in
  let error_not_bool =
    [ ILabel (Label "error_not_bool") ]
    @ [ IPush (Reg RBP) ]
    @ [ IMov (Reg RBP, Reg RSP) ]
    @ [ IMov (Reg RSI, Reg RAX) ]
    @ [ IMov (Reg RDI, Const 2L) ]
    @ [ ICall (Label "error") ]
    @ [ IMov (Reg RSP, Reg RBP) ]
    @ [ IPop (Reg RBP) ]
    @ [ IRet ]
  in error_not_number @ error_not_bool

let rec compile_aexpr (expr : aexpr) (env : env) (fenv : afenv) : instruction list =
  match expr with
  | Let (id, c, a) ->
    let (env', slot) = extend_env id RBP env in
    (compile_cexpr c env fenv)
    @ [ IMov (RegOffset (RBP, slot), Reg (RAX)) ]
    @ (compile_aexpr a env' fenv)
  | Ret c -> compile_cexpr c env fenv

and compile_cexpr (expr : cexpr) (env : env) (fenv : afenv) : instruction list =
  match expr with
  | Atom i -> [ IMov (Reg RAX, arg_immexpr i env empty_afenv) ]
  | Prim1 (op, c) -> 
    let head = compile_cexpr c env fenv in
    begin match op with
    | Add1 -> head @ test_if_number @ [ IAdd (Reg(RAX), arg_immexpr (Num 1L) empty_env empty_afenv) ]
    | Sub1 -> head @ test_if_number @ [ ISub (Reg(RAX), arg_immexpr (Num 1L) empty_env empty_afenv) ]
    | Not -> 
      head 
      @ test_if_bool
      @ [ IMov (Reg(R10), Const(Int64.sub bool_true 1L)) ] 
      @ [ IXor (Reg(RAX), Reg(R10)) ]
    | Print -> failwith "To Be Done"
    end
  | Prim2 (op, imm1, imm2) -> 
    let const1 = [ IMov (Reg RAX, arg_immexpr imm1 env empty_afenv) ] in
    let const2 = [ IMov (Reg RAX, arg_immexpr imm2 env empty_afenv) ] in
    begin match op with
    | Add -> 
      const1
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ const2
      @ test_if_number
      @ [ IMov (Reg RAX, Reg R10) ]
      @ [ IAdd (Reg RAX, arg_immexpr imm2 env empty_afenv) ]
    | And -> 
      const1
      @ test_if_bool
      @ (and_immexpr imm2 env empty_afenv)
    | Or -> 
      const1
      @ test_if_bool 
      @ (or_immexpr imm2 env empty_afenv)
    | Lte -> 
      const1
      @ test_if_number
      @ [ IMov (Reg R10, Reg RAX) ]
      @ const2
      @ test_if_number
      @ [ IMov (Reg RAX, Reg R10) ]
      @ (lte_immexpr imm2 env empty_afenv)
    end
  | If (cond_expr, then_expr, else_expr) ->
    let else_label = Gensym.fresh "else" in
    let done_label = Gensym.fresh "done" in
    [ IMov (Reg RAX, arg_immexpr cond_expr env empty_afenv) ]
    @ test_if_bool
    @ [ ICmp (Reg RAX, Const (bool_false)) ]
    @ [ IJe  (Label else_label) ]
    @ (compile_cexpr then_expr env fenv)
    @ [ IJmp (Label done_label) ]
    @ [ ILabel (Label else_label) ]
    @ (compile_cexpr else_expr env fenv)
    @ [ ILabel (Label done_label)]
  | Apply (name, args) -> (* TBD *)
    begin match List.length args with
    | n when n <= 6 -> [ ICall (Label name) ]
    | n when n > 6 -> [ ICall (Label name) ]
    | _ -> failwith "Failed pattern matching at Apply"
    end

and arg_immexpr (expr : immexpr) (env : env) (_ : afenv) : arg =
  match expr with
  | Num n -> 
    if n > max_int || n < min_int 
      then raise (CTError (Printf.sprintf "Integer overflow: " ^ (Int64.to_string n)))
      else Const (Int64.shift_left n 1)
  | Bool true -> Const (bool_true)
  | Bool false -> Const (bool_false)
  | Id x -> 
    let reg, n = lookup_env x env in
    if n = 0 then Reg reg else RegOffset (reg, n)

and and_immexpr (expr : immexpr) (env : env) (_ : afenv) : instruction list =
  let done_label = Gensym.fresh "and" in
  [ IMov (Reg R10, Const bool_false) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg0 is false -> false *)
  @ [ IMov (Reg RAX, arg_immexpr expr env empty_afenv) ]
  @ test_if_bool
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ] (* if arg1 is false -> false*)
  @ [ IMov (Reg RAX, Const bool_true) ] (* else -> true *)
  @ [ ILabel (Label done_label) ]

and or_immexpr (expr : immexpr) (env : env) (_ : afenv) : instruction list =
  let done_label = Gensym.fresh "or" in
  [ IMov (Reg R10, Const bool_true) ]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg R10, Reg RAX) ]
  @ [ IMov (Reg RAX, arg_immexpr expr env empty_afenv) ]
  @ test_if_bool
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJe (Label done_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

and lte_immexpr (expr : immexpr) (env : env) (_ : afenv) : instruction list =
  let lte_label = Gensym.fresh "lte" in
  let done_label = Gensym.fresh "ltedone" in
  [ IMov (Reg R10, arg_immexpr expr env empty_afenv)]
  @ [ ICmp (Reg RAX, Reg R10) ]
  @ [ IJle (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_false) ]
  @ [ IJmp (Label done_label)]
  @ [ ILabel (Label lte_label) ]
  @ [ IMov (Reg RAX, Const bool_true) ]
  @ [ ILabel (Label done_label) ]

let compile_afundefs (fenv: afundef list) : instruction list =
  let rec gen_64bits_env l env =
    match l with
    | [] -> env
    | hd::tl ->
      begin match List.length env with
      | 0 -> 
        let env' = (hd, RDI, 0)::env in
        gen_64bits_env tl env'
      | 1 -> 
        let env' = (hd, RSI, 0)::env in
        gen_64bits_env tl env'
      | 2 -> 
        let env' = (hd, RDX, 0)::env in
        gen_64bits_env tl env'
      | 3 ->
        let env' = (hd, RCX, 0)::env in
        gen_64bits_env tl env'
      | 4 ->
        let env' = (hd, R8, 0)::env in
        gen_64bits_env tl env'
      | 5 ->
        let env' = (hd, R9, 0)::env in
        gen_64bits_env tl env'
      | n when n >= 6 ->
        let env' = (hd, RBP, (n-4))::env in
        gen_64bits_env tl env'
      | _ -> failwith "Failed pattern matching"
      end
  in
  let compile_afundef f fenv =
    match f with
    | DefFun (name, args, expr) ->
      [ ILabel (Label name) ]
      @ [ IPush (Reg RBP) ]
      @ [ IMov (Reg RBP, Reg RSP) ]
      @ [ ISub (Reg RSP, Const (Int64.of_int (8 * (get_depth expr)))) ]
      @ compile_aexpr expr (gen_64bits_env args []) fenv (* TBD: Generar el env de la función acorde al método de llamada *)
      @ [ IMov (Reg RSP, Reg RBP) ]
      @ [ IPop (Reg RBP) ]
      @ [ IRet ]
    | DefSys (_, _, _) -> failwith "To Be Done" (* TBD: Llamado a funciones foraneas *)
  in let rec accumulate fl fenv acc =
    match fl with
    | [] -> acc
    | hd :: tail -> 
      let fenv' = hd :: fenv in
      let instrs = compile_afundef hd fenv in
      accumulate tail fenv' (acc @ instrs)
  in accumulate fenv [] []

let gen_prologue aexpr =
  let header = "
section .text
extern error
global our_code_starts_here

our_code_starts_here:
  push RBP
  mov RBP, RSP" in
  let depth = get_depth aexpr in
  if depth = 0
    then header
    else header ^ "
  sub RSP, 8*" ^ string_of_int depth

let compile_prog (p : prog) : string =
  let f, e = p in
  let afenv = check_afundefs (List.map anf_fundef f) in
  let aexpr = check_anf (anf_expr e) afenv in
  let instrs = compile_aexpr aexpr empty_env afenv in
  let fun_instrs = compile_afundefs afenv in
  let prologue = gen_prologue aexpr in
  let epilogue ="
  mov RSP, RBP
  pop RBP
  ret
" in
  prologue ^ pp_instrs instrs ^ epilogue ^ pp_instrs fun_instrs ^ pp_instrs error_handlers
